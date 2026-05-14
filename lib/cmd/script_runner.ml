let ( / ) = Filename.concat
let ocaml_name = OpamPackage.Name.of_string "ocaml"

(* Drop the [ocaml] dependency: it's provided by the toolchain prefix
   the caller already assembled, not a layer to build. *)
let dep_names_for_build all_deps =
  List.filter_map
    (fun (d : Oi.Project.Script.dep) ->
      if OpamPackage.Name.equal d.name ocaml_name then None else Some d.name)
    all_deps

let build_request ~toolchain ~conf ~constraints dep_names :
    Oi.Build_pipeline.request =
  {
    targets =
      [
        Group
          {
            tokens = List.map OpamPackage.Name.to_string dep_names;
            handles = [];
          };
      ];
    with_repos = [];
    pins = [];
    extra_repos = [];
    constraints;
    toolchain_override = None;
    toolchain;
    conf;
    local_packages_dir = None;
    project_root = None;
    force_source = false;
    with_test = false;
    refresh = false;
  }

(* Build the extra deps under a fresh switch so the HTTP session and the
   pipeline state are scoped to this build only, then return a richer
   prefix that overlays the new layers onto [prefix]. *)
let build_extra_deps_prefix ~sys ~fs ~proc_mgr ~clock ~os_key ~conf ~cache
    ~data_dir ?toolchain ?source_remote ~constraints dep_names =
  Eio.Switch.run @@ fun sw ->
  let http_session = D10.Sysops.Http.with_session ~sw sys (fun s -> s) in
  let pipeline_env : Oi.Build_pipeline.env =
    { proc_mgr; fs; clock; sys; os_key; cache; data_dir; http_session }
  in
  let req = build_request ~toolchain ~conf ~constraints dep_names in
  let solved = Oi.Build_pipeline.solve pipeline_env req in
  let _ : D10ir.Direct.result option =
    Oi.Build_pipeline.build pipeline_env
      {
        solved;
        layer_remote = None;
        source_remote;
        jobs = None;
        upload_archive_url = None;
        archive_sources = false;
        snapshot_reporepo = false;
      }
  in
  Oi.Pipeline.assemble_prefix ~sys ~fs ~clock ~cache ~os_key
    ~layer_hashes:(Oi.Build_pipeline.layer_hashes solved)

let resolve_prefix ~sys ~fs ~proc_mgr ~clock ~os_key ~prefix ~conf ~cache
    ~data_dir ?toolchain ?source_remote all_deps =
  let dep_names = dep_names_for_build all_deps in
  if dep_names = [] then prefix
  else
    let constraints = Oi.Project.Script.constraints all_deps in
    build_extra_deps_prefix ~sys ~fs ~proc_mgr ~clock ~os_key ~conf ~cache
      ~data_dir ?toolchain ?source_remote ~constraints dep_names

(* Hardlink-style copy of the freshly built [main.exe] up to
   [<run_dir>/main.exe] so subsequent invocations of the same script
   hit the cached path. *)
let cache_built_binary ~fs ~built ~cached_bin =
  if Workspace.path_exists fs built then begin
    let content = Eio.Path.load Eio.Path.(fs / built) in
    Eio.Path.save ~create:(`Or_truncate 0o755)
      Eio.Path.(fs / cached_bin)
      content
  end

let compile_and_exec ~fs ~proc_mgr ~cache ~prefix ~script_path ~all_deps
    ~run_dir_s ~cached_bin args =
  let build_dir = run_dir_s in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / build_dir);
  Oi.Project.Script.generate_project ~script:script_path ~deps:all_deps
    ~dir:build_dir;
  let build_env =
    Oi.Solver.Env.make_env ~prefix ~dune_cache_root:(Oi.Cache.dune_root cache)
      ()
  in
  Eio.Process.run proc_mgr ~env:build_env
    [ "/bin/sh"; "-c"; Fmt.str "cd %s && dune build main.exe 2>&1" build_dir ];
  let built = build_dir / "_build" / "default" / "main.exe" in
  cache_built_binary ~fs ~built ~cached_bin;
  let exe = if Workspace.path_exists fs cached_bin then cached_bin else built in
  exit (Subprocess.run proc_mgr ~env:build_env (exe :: args))

(* The [run.ml] caller has already built the [--with] deps via
   [Build_pipeline.build] and assembled them into [prefix]. Here we read
   the script file's [[\@\@\@opam …]] header, build any *additional*
   deps the script declares (re-running [Build_pipeline.build] with the
   merged dep set), reassemble a richer prefix, then compile the
   script and exec.

   All build paths inside this function go through [Build_pipeline.build],
   so the d10ir pipeline (Recipe_emitter → archive prefetch →
   D10ir.Direct.run) handles every layer build. *)
let run ~sys ~fs ~proc_mgr ~clock ~os_key ~prefix ~conf ~cache ~data_dir
    ?toolchain ?source_remote script_path cli_deps args =
  let file_deps = Oi.Project.Script.parse_deps_from_file ~fs script_path in
  let all_deps = Oi.Project.Script.dedup (file_deps @ cli_deps) in
  if all_deps = [] then
    Oi.Error.fail_msg
      "No dependencies found. Add [@@@opam pkg1 pkg2] to the first line or use \
       --with=pkg";
  let script_hash = Oi.Project.Script.script_hash script_path all_deps in
  let run_dir = Oi.Cache.run_dir cache ~hash:script_hash in
  let run_dir_s = Eio.Path.native_exn run_dir in
  let cached_bin = run_dir_s / "main.exe" in
  if Workspace.path_exists fs cached_bin then
    exit
      (Subprocess.run proc_mgr
         ~env:
           (Oi.Solver.Env.make_env ~prefix
              ~dune_cache_root:(Oi.Cache.dune_root cache) ())
         (cached_bin :: args))
  else
    let prefix =
      resolve_prefix ~sys ~fs ~proc_mgr ~clock ~os_key ~prefix ~conf ~cache
        ~data_dir ?toolchain ?source_remote all_deps
    in
    compile_and_exec ~fs ~proc_mgr ~cache ~prefix ~script_path ~all_deps
      ~run_dir_s ~cached_bin args
