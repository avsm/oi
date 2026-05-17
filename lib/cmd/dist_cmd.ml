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
(* The gitignore-clean fileset of [cwd]: tracked + untracked-but-not-
   ignored, from a single [git ls-files -z] (NUL-separated) captured via
   the Eio-backed process wrapper — no shell, no [tar | tar] pipeline.
   Falls back to a recursive walk (skipping [.git]) for a non-git tree.
   Paths are relative to [cwd]. *)
let kind_opt p =
  try Some (Unix.lstat p).Unix.st_kind with Unix.Unix_error _ -> None

(* Recursive [.git]-skipping walk (the non-git fallback), as flat
   mutually-recursive helpers so nesting stays shallow. *)
let rec walk_tree ~cwd rel acc =
  match Sys.readdir (if rel = "" then cwd else cwd / rel) with
  | exception Sys_error _ -> acc
  | entries -> Array.fold_left (visit_entry ~cwd rel) acc entries

and visit_entry ~cwd rel acc name =
  if name = ".git" then acc
  else
    let r = if rel = "" then name else rel / name in
    match kind_opt (cwd / r) with
    | Some Unix.S_DIR -> walk_tree ~cwd r acc
    | Some (Unix.S_REG | Unix.S_LNK) -> r :: acc
    | _ -> acc

let gitignore_clean_files ~sys ~cwd =
  if Sys.file_exists (cwd / ".git") then
    D10.Sysops.Cmd.run_out sys
      [
        "git"; "-C"; cwd; "ls-files"; "-z"; "--cached"; "--others";
        "--exclude-standard";
      ]
    |> String.split_on_char '\000'
    |> List.filter (fun s -> s <> "")
  else walk_tree ~cwd "" []

(* Copy [cwd/rel] -> [dst_root/rel] preserving symlinks (a source tree
   may contain them; dereferencing would be wrong) and the source's
   permission bits. Submodule gitlinks / vanished entries are skipped. *)
let copy_one ~fs ~cwd ~dst_root rel =
  let src = cwd / rel and dst = dst_root / rel in
  match kind_opt src with
  | None | Some Unix.S_DIR -> ()
  | Some Unix.S_LNK ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
        Eio.Path.(fs / Filename.dirname dst);
      (try Unix.symlink (Unix.readlink src) dst
       with Unix.Unix_error _ -> ())
  | Some Unix.S_REG ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
        Eio.Path.(fs / Filename.dirname dst);
      let perm = (Unix.stat src).Unix.st_perm in
      Eio.Path.with_open_in Eio.Path.(fs / src) (fun i ->
          Eio.Path.with_open_out ~create:(`Or_truncate perm)
            Eio.Path.(fs / dst)
          @@ fun o -> Eio.Flow.copy i o);
      (try Unix.chmod dst perm with Unix.Unix_error _ -> ())
  | Some _ -> ()

(* Snapshot the gitignore-clean [cwd] tree to [<output>/source/] — a
   persistent INPUT (kept across [make clean], unlike the [src/]/[dest/]
   build scratch). Built in a [Filename.temp_dir] staging directory under
   [output] then renamed into place; the only recursive deletes are
   [Eio.Path.rmtree] of those uniquely-named temp dirs — never a shelled
   [rm -rf] of a derived path. *)
let snapshot_local ~sys ~fs ~cwd ~output =
  if Filename.is_relative output || output = "/" || output = "" then
    Oi.Error.fail_config_error
      "oi dist makefile: refusing to snapshot into unsafe output %S" output;
  let final = output / "source" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / output);
  (* Unique staging dir alongside [final], same filesystem so the
     [Sys.rename] swap is atomic. [Filename.temp_dir] creates it. *)
  let tmp = Filename.temp_dir ~temp_dir:output ~perms:0o755 "source." ".tmp" in
  List.iter
    (copy_one ~fs ~cwd ~dst_root:tmp)
    (gitignore_clean_files ~sys ~cwd);
  if Sys.file_exists final then begin
    (* Move the prior tree onto a fresh empty temp dir, swap the new
       one in, then drop the old — never an [rm -rf] of [final]. *)
    let old = Filename.temp_dir ~temp_dir:output "source." ".old" in
    Sys.rename final old;
    Sys.rename tmp final;
    Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / old)
  end
  else Sys.rename tmp final

(* The local-source root for no-[TARGET] mode: snapshot the cwd and
   anchor it on the deepest dep node (whose env/prefix already wires the
   assembled dependency prefix). *)
let local_root ~sys ~fs ~local_mode ~plan ~bin_roots ~cwd_s ~output :
    Makefile_export.local option =
  if not local_mode then None
  else
    match List.rev plan.D10ir.Plan.nodes with
    | [] ->
        Oi.Error.fail_config_error
          "oi dist makefile: the project has no buildable dependencies to \
           anchor the local build."
    | last :: _ ->
        snapshot_local ~sys ~fs ~cwd:cwd_s ~output;
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
            (* Resolved by oi-build-node.sh as [$ROOT/source/], the
               persistent shipped tree (kept across [make clean]). *)
            sha256 = "source";
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
(* Solve the WHOLE requested/closure set as ONE group, like [oi build]
   project mode (collect_solver_roots). [Build_pipeline.parse] alone
   makes each entry its own [Plain] singleton group, so a depopt
   provider that is a *sibling* dependency (e.g. [bytesrw] for [jsont])
   is absent from the consumer's per-group [in_solution] and never
   activates — building the layer without it. One [Group] puts the full
   closure in a single solve so cross-dependency depopts resolve. *)
let coalesce_targets targets : Oi.Build_pipeline.target list =
  let toks, handles, extra =
    List.fold_left
      (fun (toks, hs, extra) s ->
        match Oi.Build_pipeline.parse s with
        | Oi.Build_pipeline.Plain t -> (t :: toks, hs, extra)
        | Oi.Build_pipeline.Overlay_pkg { handle; spec } ->
            (spec :: toks, handle :: hs, extra)
        | (Oi.Build_pipeline.Overlay_all _ | Oi.Build_pipeline.Group _) as g ->
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

let run_makefile ~harness ~refresh ~registry ~use_registry ~with_repos
    ~with_deps ~toolchain_override ~targets ~output =
  if output = "" then
    Oi.Error.fail_config_error "oi dist makefile: -o DIR is required";
  let output = absolutize_output output in
  let { Harness.fs; sys; clock; _ } = harness in
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
  let grouped_targets = coalesce_targets targets in
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
      let local =
        local_root ~sys ~fs ~local_mode ~plan ~bin_roots ~cwd_s ~output
      in
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
