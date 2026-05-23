(* The makefile-emit engine, lifted out of dist_cmd.ml so both [Dist_cmd]
   ([oi dist makefile]) and [Osdist_bundle] ([oi dist bundle]) can call it
   without a module cycle. Behaviour and helpers are byte-for-byte the same
   as before; only the wrapping cmdliner glue stayed in dist_cmd.ml. *)

let ( / ) = Filename.concat

let absolutize_output output =
  if Filename.is_relative output then Filename.concat (Sys.getcwd ()) output
  else output

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
          install_to = None;
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

let kind_opt p =
  try Some (Unix.lstat p).Unix.st_kind with Unix.Unix_error _ -> None

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
        "git";
        "-C";
        cwd;
        "ls-files";
        "-z";
        "--cached";
        "--others";
        "--exclude-standard";
      ]
    |> String.split_on_char '\000'
    |> List.filter (fun s -> s <> "")
  else walk_tree ~cwd "" []

let copy_one ~fs ~cwd ~dst_root rel =
  let src = cwd / rel and dst = dst_root / rel in
  match kind_opt src with
  | None | Some Unix.S_DIR -> ()
  | Some Unix.S_LNK -> (
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
        Eio.Path.(fs / Filename.dirname dst);
      try Unix.symlink (Unix.readlink src) dst with Unix.Unix_error _ -> ())
  | Some Unix.S_REG -> (
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
        Eio.Path.(fs / Filename.dirname dst);
      let perm = (Unix.stat src).Unix.st_perm in
      Eio.Path.with_open_in
        Eio.Path.(fs / src)
        (fun i ->
          Eio.Path.with_open_out ~create:(`Or_truncate perm) Eio.Path.(fs / dst)
          @@ fun o -> Eio.Flow.copy i o);
      try Unix.chmod dst perm with Unix.Unix_error _ -> ())
  | Some _ -> ()

let snapshot_local ~sys ~fs ~cwd ~output =
  if Filename.is_relative output || output = "/" || output = "" then
    Oi.Error.fail_config_error
      "oi dist makefile: refusing to snapshot into unsafe output %S" output;
  let final = output / "source" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / output);
  let tmp = Filename.temp_dir ~temp_dir:output ~perms:0o755 "source." ".tmp" in
  List.iter (copy_one ~fs ~cwd ~dst_root:tmp) (gitignore_clean_files ~sys ~cwd);
  if Sys.file_exists final then begin
    let old = Filename.temp_dir ~temp_dir:output "source." ".old" in
    Sys.rename final old;
    Sys.rename tmp final;
    Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / old)
  end
  else Sys.rename tmp final

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
            sha256 = "source";
            script;
            env = last.D10ir.Plan.env;
            prefix = sent;
            deps = bin_roots;
          }

(* Statically explode every buildable node's pre-baked archive into
   [<output>/sources/<sha>/] so the emitted Makefile has no fetch
   phase. The cache is content-addressed: same sha => same tree, so we
   skip if the target dir already looks populated.

   The d10 cache may not have every archive: [oi build] short-circuits
   on cached layers and never pulls their underlying source. So before
   unpacking, prefetch any missing [<sha>.tar.zst] from the registry
   so this command works against a partially-populated cache. *)
let unpack_sources ~harness ~registry ~(plan : D10ir.Plan.t) ~output =
  let { Harness.fs; proc_mgr; clock; cache; http_session; _ } = harness in
  let cache_root = Oi.Cache.root_s cache in
  let ext = Hashtbl.create 16 in
  List.iter
    (fun h -> Hashtbl.replace ext (D10ir.Layer_hash.to_string h) ())
    plan.external_layers;
  let buildable =
    List.filter
      (fun (n : D10ir.Plan.node) ->
        not (Hashtbl.mem ext (D10ir.Layer_hash.to_string n.layer_hash)))
      plan.nodes
  in
  let archive_for (n : D10ir.Plan.node) =
    Filename.concat cache_root
      (Filename.concat "d10ir" ("archives" / (n.archive.sha256 ^ ".tar.zst")))
  in
  let missing =
    List.filter_map
      (fun (n : D10ir.Plan.node) ->
        if Sys.file_exists (archive_for n) then None else Some n.archive.sha256)
      buildable
    |> List.sort_uniq compare
  in
  (if missing <> [] then
     let summary =
       D10ir.Registry.prefetch
         ~clock:(clock :> _ Eio.Time.clock_ty Eio.Resource.t)
         ~fs ~session:http_session ~cache_root ~remote:(`Http_remote registry)
         missing
     in
     if summary.missing <> [] then
       Oi.Error.fail_config_error
         "oi dist makefile: %d source archive(s) missing locally and not on %s:\n\
         \  %s\n\
          Run [oi repo bump] on the overlay to bake them, or point \
          [--registry] at a registry that publishes them."
         (List.length summary.missing)
         registry
         (String.concat "\n  " summary.missing));
  let sources_dir = output / "sources" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / sources_dir);
  List.iter
    (fun (n : D10ir.Plan.node) ->
      let dst = sources_dir / n.archive.sha256 in
      let populated =
        match Sys.readdir dst with
        | entries -> Array.length entries > 0
        | exception _ -> false
      in
      if not populated then
        D10ir.Direct.unpack_archive ~proc_mgr ~fs ~plan_dir:output
          ~archive_root:plan.D10ir.Plan.archive_root n ~build_dir:dst)
    buildable

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

let emit_dockerfiles ~harness ~refresh ~output ~label ~(plan : D10ir.Plan.t) =
  let { Harness.fs; sys; cache; data_dir; platform; _ } = harness in
  let needs_host_ocaml = plan.D10ir.Plan.external_layers <> [] in
  let per_distro =
    Docker.compute_per_distro_depexts ~fs ~sys ~cache ~data_dir ~refresh
      ~platform
  in
  Registry_docker.write_file (output / ".dockerignore") "src/\ndest/\n";
  let paths =
    List.map
      (fun (d, overlay_depexts) ->
        let path = output / Registry_docker.one_distro_filename d in
        Registry_docker.write_dockerfile path
          (Registry_docker.dockerfile_makefile ~label ~needs_host_ocaml
             ~overlay_depexts d);
        (path, List.length overlay_depexts))
      per_distro
  in
  Oi.Say.step "Wrote %d standalone Dockerfile(s)" (List.length paths);
  List.iter
    (fun (path, n) ->
      let suffix = if n = 0 then "" else Fmt.str "  (%d depexts)" n in
      Oi.Say.info "%s%a" path Oi.Style.pp_dim_string suffix)
    paths

let run_makefile ~harness ~refresh ~registry ~use_registry ~with_repos
    ~with_deps ~toolchain_override ~targets ~output =
  if output = "" then
    Oi.Error.fail_config_error "oi dist makefile: -o DIR is required";
  let output = absolutize_output output in
  let { Harness.fs; sys; clock; _ } = harness in
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
      unpack_sources ~harness ~registry ~plan ~output;
      Makefile_export.emit plan ~output ~binaries ~bin_roots ?local ();
      if binaries <> [] then
        Oi.Say.field "binaries" "%s" (String.concat " " binaries);
      emit_dockerfiles ~harness ~refresh ~output
        ~label:(String.concat ", " targets)
        ~plan;
      Oi.Say.ok
        "wrote portable Makefile + per-distro Dockerfiles to %s (run: make, or \
         docker build -f Dockerfile.<distro> .)"
        output
