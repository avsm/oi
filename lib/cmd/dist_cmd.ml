(* The [oi dist] command group. Kept separate from the {!Dist} helper
   module (which {!Build} depends on) to avoid a Dist -> Docker ->
   Build -> Dist module cycle; the CLI is still exactly [oi dist]. *)

(* -- the [oi dist] command group --------------------------------------- *)

open Cmdliner

let ( / ) = Filename.concat

let absolutize_output output =
  if Filename.is_relative output then Filename.concat (Sys.getcwd ()) output
  else output

(* -- [oi dist makefile] ------------------------------------------------- *)

(* Binaries the just-finished [oi build] produced for the requested
   roots, read from their per-layer manifests in the oi cache. Used for
   the Makefile header / [make dest] summary. Best-effort: a missing or
   unreadable manifest just contributes nothing. *)
let built_binaries ~harness ~solved =
  let { Harness.cache; os_key; _ } = harness in
  let cache_root = Oi.Cache.root_s cache in
  Oi.Build_pipeline.root_layer_hashes solved
  |> List.concat_map (fun hash ->
      let path = Oi.Manifest_layer.path_for ~cache_root ~os_key ~hash in
      match Oi.Manifest_layer.try_read ~path with
      | Some m -> m.Oi.Manifest_layer.binaries
      | None -> [])
  |> List.sort_uniq compare

(* Drive a real [oi build] (registry-restore where possible) so the plan
   is validated and the cache warmed, then report the produced binaries.
   Best-effort: build failures warn but still let the Makefile be emitted
   (it reproduces from source regardless). *)
let oi_build_and_binaries ~harness ~pipeline_env ~req ~layer_remote
    ~source_remote ~targets ~clock =
  let solved, build_result =
    Progress_ui.with_ui
      ~target:(String.concat ", " targets)
      ~clock:(clock :> _ Eio.Resource.t)
      ~enabled:(Tty.is_tty ())
    @@ fun reporter ->
    let solved = Oi.Build_pipeline.solve pipeline_env ~reporter req in
    let result =
      Oi.Build_pipeline.build pipeline_env ~reporter
        {
          solved;
          layer_remote;
          source_remote;
          jobs = None;
          upload_archive_url = None;
          archive_sources = false;
          snapshot_reporepo = false;
        }
    in
    (solved, result)
  in
  (match build_result with
  | Some (r : D10ir.Direct.result) when r.failed = 0 && r.skipped = 0 -> ()
  | Some (r : D10ir.Direct.result) ->
      Oi.Say.warn
        "oi build reported %d failed, %d skipped node(s); the Makefile is \
         still emitted but may not build those."
        r.failed r.skipped
  | None ->
      Oi.Say.warn
        "oi build produced no result; emitting the Makefile from the plan only.");
  built_binaries ~harness ~solved

(* Snapshot the cwd into [<output>/src/local-src.tar.zst], excluding
   anything git ignores (tracked + untracked-but-not-ignored). Used by
   the no-[TARGET] release flow. *)
let snapshot_local ~cwd ~output =
  let dir = output / "src" in
  List.iter
    (fun d ->
      try Unix.mkdir d 0o755 with Unix.Unix_error (EEXIST, _, _) -> ())
    [ output; dir ];
  let tarball = dir / "local-src.tar.zst" in
  let q = Filename.quote in
  let cmd =
    if Sys.file_exists (cwd / ".git") then
      Fmt.str
        "cd %s && git ls-files -z --cached --others --exclude-standard | tar \
         --null -T - --owner=0 --group=0 -cf - | zstd -q -19 -f -o %s"
        (q cwd) (q tarball)
    else
      Fmt.str
        "tar -C %s --exclude=.git --owner=0 --group=0 -cf - . | zstd -q -19 -f \
         -o %s"
        (q cwd) (q tarball)
  in
  if Sys.command cmd <> 0 then
    Oi.Error.fail_config_error "oi dist makefile: failed to snapshot %s" cwd

(* The local-source root for no-[TARGET] mode: snapshot the cwd and
   anchor it on the deepest dep node (whose env/prefix already wires the
   assembled dependency prefix). *)
let local_root ~local_mode ~plan ~bin_roots ~cwd_s ~output :
    Makefile_export.local option =
  if not local_mode then None
  else
    match List.rev plan.D10ir.Plan.nodes with
    | [] ->
        Oi.Error.fail_config_error
          "oi dist makefile: the project has no buildable dependencies to \
           anchor the local build."
    | last :: _ ->
        snapshot_local ~cwd:cwd_s ~output;
        let sent = last.D10ir.Plan.prefix in
        let script =
          Fmt.str
            "set -e\n\
             'dune' 'build' '@install'\n\
             'dune' 'install' '--prefix' %s '--libdir' %s\n"
            (Filename.quote sent)
            (Filename.quote (sent ^ "/lib"))
        in
        Some
          {
            Makefile_export.name = "local";
            sha256 = "local-src";
            strip = 0;
            script;
            env = last.D10ir.Plan.env;
            prefix = sent;
            deps = bin_roots;
          }

(* Two solves of the same closure (the persistent solve cache makes the
   second ~free, and layer hashes are content-addressed so they agree):
   the normal one drives the validating [oi build]; the
   [force_source = true] one yields the unified [D10ir.Plan.t] the
   Makefile is a pure projection of. *)
let run_makefile ~harness ~refresh ~registry ~use_registry ~with_repos
    ~with_deps ~toolchain_override ~targets ~output =
  if output = "" then
    Oi.Error.fail_config_error "oi dist makefile: -o DIR is required";
  let output = absolutize_output output in
  let { Harness.fs; clock; _ } = harness in
  (* No TARGET: project mode — build the cwd project's deps from the
     registry and the project itself from a gitignore-clean snapshot. *)
  let local_mode = targets = [] in
  let cwd_s, _ = Workspace.resolved_cwd fs in
  let targets =
    if local_mode then (Oi.Project.load ~fs cwd_s).deps else targets
  in
  if targets = [] then
    Oi.Error.fail_config_error
      "oi dist makefile: a TARGET, or a project dir with *.opam, is required";
  (* Solve the WHOLE requested/closure set as ONE group, exactly like
     [oi build] project mode (collect_solver_roots). [Build_pipeline.parse]
     would turn each entry into its own [Plain] singleton group — and a
     depopt provider that is a *sibling* dependency (e.g. [bytesrw] for
     [jsont]) would then be absent from the consumer's per-group
     [in_solution], so the depopt never activates and the layer is built
     without it. Coalescing into one [Group] puts the full closure in a
     single solve so cross-dependency depopts resolve correctly. *)
  let grouped_targets =
    let toks, handles, extra =
      List.fold_left
        (fun (toks, hs, extra) s ->
          match Oi.Build_pipeline.parse s with
          | Oi.Build_pipeline.Plain t -> (t :: toks, hs, extra)
          | Oi.Build_pipeline.Overlay_pkg { handle; spec } ->
              (spec :: toks, handle :: hs, extra)
          | (Oi.Build_pipeline.Overlay_all _ | Oi.Build_pipeline.Group _) as g
            ->
              (toks, hs, g :: extra))
        ([], [], []) targets
    in
    (if toks = [] then []
     else
       [
         Oi.Build_pipeline.Group
           { tokens = List.rev toks; handles = List.sort_uniq compare handles };
       ])
    @ List.rev extra
  in
  let {
    Pipeline_setup.env = pipeline_env;
    request = req;
    layer_remote;
    source_remote;
    _;
  } =
    Pipeline_setup.prepare ~harness ~refresh ~locked:false
      ~skip_local:(not local_mode) ~registry ~use_registry ~with_repos
      ~with_deps ~toolchain_override ~targets:grouped_targets ()
  in
  let binaries =
    oi_build_and_binaries ~harness ~pipeline_env ~req ~layer_remote
      ~source_remote ~targets ~clock
  in
  let recipe_solved =
    Oi.Build_pipeline.solve pipeline_env
      { req with Oi.Build_pipeline.force_source = true }
  in
  match recipe_solved.Oi.Build_pipeline.merged with
  | None ->
      Oi.Error.fail_config_error
        "oi dist makefile: every solve group failed; nothing to emit."
  | Some plan ->
      let bin_roots = Oi.Build_pipeline.root_layer_hashes recipe_solved in
      let local = local_root ~local_mode ~plan ~bin_roots ~cwd_s ~output in
      Makefile_export.emit plan ~output ~registry ~binaries ~bin_roots ?local ();
      if binaries <> [] then
        Oi.Say.field "binaries" "%s" (String.concat " " binaries);
      Oi.Say.ok "wrote portable Makefile to %s (run: make)" output

let makefile_run (c : Terms.common) refresh registry use_registry with_repos
    with_deps _jobs toolchain_override targets output =
  Harness.run @@ fun ~sw env ->
  let harness =
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  run_makefile ~harness ~refresh ~registry ~use_registry ~with_repos ~with_deps
    ~toolchain_override ~targets ~output

let makefile_man =
  [
    `S Manpage.s_description;
    `P
      "Write a self-contained Makefile that builds $(i,TARGET) with no $(b,oi) \
       or $(b,opam) at build time — only $(b,make), $(b,curl), \
       $(b,tar)+$(b,zstd) and a host OCaml toolchain.";
    `P
      "With no $(i,TARGET), run from a project dir: the gitignore-clean \
       working tree is vendored as the source and built against its registry \
       deps — a self-contained release build.";
    `P
      "$(b,make) builds and assembles $(b,./dest); $(b,make V=1) is verbose; \
       $(b,make install) copies the binaries under $(b,DESTDIR)/$(b,PREFIX).";
    `S Manpage.s_examples;
    `Pre
      "  oi dist makefile utop -o ./utop-mk\n\
      \  oi dist makefile -o ./release      # current project\n\
      \  cd ./release && make && make install PREFIX=/opt/app";
  ]

let makefile_cmd =
  let output =
    Arg.(
      value & opt string ""
      & info ~docv:"DIR" ~doc:"Output directory (required)." [ "o"; "output" ])
  in
  let targets =
    Arg.(
      value & pos_all string []
      & info ~docv:"TARGET"
          ~doc:
            "Package name or $(b,@HANDLE/PKG). Omit to build the current \
             project from a gitignore-clean snapshot."
          [])
  in
  Cmd.v
    (Cmd.info "makefile"
       ~doc:"Emit a portable, oi-free Makefile build from registry archives"
       ~man:makefile_man)
    Term.(
      const makefile_run $ Terms.common $ Terms.refresh $ Terms.registry
      $ Terms.use_registry $ Terms.with_repos $ Terms.with_deps $ Terms.jobs
      $ Terms.toolchain $ targets $ output)

(* -- the group --------------------------------------------------------- *)

let cmd =
  let info =
    Cmd.info "dist"
      ~doc:
        "Emit portable build artefacts (Dockerfiles, obuilder specs, \
         Makefiles) for a target."
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Generate a self-contained build description for the current \
             project or a $(b,TARGET):";
          `I ("$(b,oi dist docker)", "Dockerfiles (project / CI / registry).");
          `I ("$(b,oi dist obuilder)", "obuilder specs (s-expressions).");
          `I
            ( "$(b,oi dist makefile)",
              "a portable Makefile that needs no $(b,oi) at build time." );
        ]
  in
  Cmd.group info (Docker.subcommands @ [ makefile_cmd ])
