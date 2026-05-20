let log_src = Logs.Src.create "oi.cmd.sync" ~doc:"oi sync command"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat
let short_hash h = String.sub h 0 (min 12 (String.length h))

(* Return the layer hash whose [layer.json] declares package name
   [want_name] (any version). Tools get assembled from only their own
   leaf layer — the transitive deps (ocaml, dune, ocamlfind…) are
   already present in the shared d10 cache but are not needed at
   runtime by a native-compiled tool binary, so we leave them out of
   [_oi/tools/] to keep it small and focused. *)
let leaf_hash_for ~fs ~cache ~os_key ~want_name hashes =
  let layers_dir = Oi.Cache.root_s cache / "layers" / os_key in
  let leaf hash =
    match
      D10.Layer.load_meta Eio.Path.(fs / layers_dir / hash / "layer.json")
    with
    | Some m -> (
        match OpamPackage.of_string_opt m.package with
        | Some p
          when OpamPackage.Name.to_string (OpamPackage.name p) = want_name ->
            Some hash
        | _ -> None)
    | None -> None
  in
  List.find_map leaf hashes

(* Static context shared by every [install_named] call in {!install_tools}.
   Grouped so the per-tool helper takes a single record instead of a
   ten-arg call signature. *)
type tools_ctx = {
  quiet : bool;
  refresh : bool option;
  jobs : int option;
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  sys : D10.Sysops.t;
  cache : Oi.Cache.t;
  data_dir : string;
  conf : Oi.Solver.Ctx.conf;
  os_key : string;
  session : D10.Sysops.Http.session;
  extra_repos : Oi.Project.extra_repo list;
  pins : Oi.Project.pin list;
  toolchain : Oi.Toolchain.info option;
  layer_remote : D10.Layer.remote option;
  source_remote : D10.Layer.remote option;
  cwd : string;
}

let say_step_ctx (c : tools_ctx) fmt =
  if c.quiet then Fmt.kstr (fun s -> Log.info (fun m -> m "%s" s)) fmt
  else Fmt.kstr (fun s -> Oi.Say.step "%s" s) fmt

let say_info_ctx (c : tools_ctx) fmt =
  if c.quiet then Fmt.kstr (fun s -> Log.info (fun m -> m "%s" s)) fmt
  else Fmt.kstr (fun s -> Oi.Say.info "%s" s) fmt

let warn_named name fmt =
  Fmt.kstr (fun s -> Oi.Say.warn "tool %s: %s" name s) fmt

(* Solve and build one tool [tool_name], returning the dep-closure layer
   hashes. Surfaces per-tool [Build_pipeline.build] phases (build solver
   context, solve, plan, fetch layers) and Execute's per-package
   progress so a long tool build doesn't look like a hang. In quiet
   mode the phase narration drops to [Log.info]. *)
let solve_one_tool (c : tools_ctx) ~tool_name ~constraints =
  let name = OpamPackage.Name.of_string tool_name in
  let pipeline_env : Oi.Build_pipeline.env =
    {
      proc_mgr = c.proc_mgr;
      fs = c.fs;
      clock = c.clock;
      sys = c.sys;
      os_key = c.os_key;
      cache = c.cache;
      data_dir = c.data_dir;
      http_session = c.session;
    }
  in
  let req : Oi.Build_pipeline.request =
    {
      targets = [ Plain (OpamPackage.Name.to_string name) ];
      with_repos = [];
      pins = c.pins;
      extra_repos = c.extra_repos;
      constraints;
      toolchain_override = None;
      toolchain = c.toolchain;
      conf = c.conf;
      local_packages_dir = None;
      project_root = Some c.cwd;
      force_source = false;
      with_test = false;
      refresh = Stdlib.Option.value ~default:false c.refresh;
    }
  in
  Progress_ui.with_ui ~target:tool_name
    ~clock:(c.clock :> _ Eio.Resource.t)
    ~enabled:((not c.quiet) && Tty.is_tty ())
  @@ fun reporter ->
  let solved = Oi.Build_pipeline.solve pipeline_env ~reporter req in
  let _ : D10ir.Direct.result option =
    Oi.Build_pipeline.build pipeline_env ~reporter
      {
        solved;
        layer_remote = c.layer_remote;
        source_remote = c.source_remote;
        jobs = c.jobs;
        upload_archive_url = None;
        archive_sources = false;
        snapshot_reporepo = false;
        doc_tools_dir = None;
      }
  in
  Oi.Build_pipeline.layer_hashes solved

(* Install one tool (resolve, solve, build, locate leaf), returning its
   leaf layer hash on success or [None] on any failure. Failures warn
   but do not propagate — other tools should still try to install. *)
let install_named (c : tools_ctx) ~tool_name ~constraints =
  try
    let hashes = solve_one_tool c ~tool_name ~constraints in
    match
      leaf_hash_for ~fs:c.fs ~cache:c.cache ~os_key:c.os_key
        ~want_name:tool_name hashes
    with
    | None ->
        warn_named tool_name "layer for leaf package not found";
        None
    | Some h ->
        say_info_ctx c "tool %s: %d dep(s) built, leaf layer %s" tool_name
          (List.length hashes - 1)
          (short_hash h);
        Some h
  with
  | Oi.Error.E e ->
      warn_named tool_name "%a" Oi.Error.pp e;
      None
  | exn ->
      warn_named tool_name "%s" (Printexc.to_string exn);
      None

let install_probed c (r : Oi.Project.Tool.result) =
  let constraints =
    match r.version with
    | None -> OpamPackage.Name.Map.empty
    | Some v ->
        OpamPackage.Name.Map.singleton
          (OpamPackage.Name.of_string r.spec.name)
          (`Eq, OpamPackage.Version.of_string v)
  in
  install_named c ~tool_name:r.spec.name ~constraints

(* Assemble the per-tool leaf layers into [cwd/_oi/tools/]. The tool
   binaries are linked statically against their dep closure (which lives
   in the shared cache), so only the leaf layer needs to end up in
   [_oi/tools]. *)
let assemble_tools_prefix (c : tools_ctx) ~leaves =
  let tools_dir = c.cwd / "_oi" / "tools" in
  Eio.Path.rmtree ~missing_ok:true Eio.Path.(c.fs / tools_dir);
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(c.fs / tools_dir);
  let d10 =
    Oi.Pipeline.d10 ~sys:c.sys ~fs:c.fs ~clock:c.clock ~cache:c.cache
      ~os_key:c.os_key
  in
  let unique = List.sort_uniq String.compare leaves in
  D10.Prefix.assemble d10 ~layer_hashes:unique ~dst:Eio.Path.(c.fs / tools_dir);
  say_step_ctx c "Tools assembled at %s (%d tool(s), %d leaf layer(s))"
    tools_dir (List.length leaves) (List.length unique);
  tools_dir

(* Solve and install every dev tool into [cwd/_oi/tools/]. The set is
   the union of:
   - tools listed in the active toolchain's [x-oi-toolchain-tools]
     field (always-on for that toolchain — odoc, merlin, lsp);
   - tools whose project-state trigger fires (mdx if dune-project uses
     it, ocamlformat if .ocamlformat is present), via [Project.Tool.probe].
   Each tool is its own independent solve so its dep closure never
   leaks into the main project's OCAMLLIB / OCAMLPATH. A tool that
   fails to solve (e.g. pinned to an older ocaml) warns and is
   skipped; other tools still install. Returns the assembled path if
   at least one tool made it in, or [None] if nothing to install. *)
let install_tools ?(quiet = false) ?refresh ?jobs ~proc_mgr ~fs ~clock ~sys
    ~cache ~data_dir ~conf ~os_key ~session ~extra_repos ~pins ?toolchain
    ?layer_remote ?source_remote ~cwd () =
  let c : tools_ctx =
    {
      quiet;
      refresh;
      jobs;
      proc_mgr;
      fs;
      clock;
      sys;
      cache;
      data_dir;
      conf;
      os_key;
      session;
      extra_repos;
      pins;
      toolchain;
      layer_remote;
      source_remote;
      cwd;
    }
  in
  let toolchain_tools =
    match (toolchain : Oi.Toolchain.info option) with
    | None -> []
    | Some info -> info.tools
  in
  let probed_hits = Oi.Project.Tool.(hits (probe ~fs cwd)) in
  match (toolchain_tools, probed_hits) with
  | [], [] ->
      say_info_ctx c "no dev tools to install";
      None
  | _ ->
      let from_toolchain =
        List.filter_map
          (fun n ->
            install_named c ~tool_name:n ~constraints:OpamPackage.Name.Map.empty)
          toolchain_tools
      in
      let from_probes = List.filter_map (install_probed c) probed_hits in
      let leaves = from_toolchain @ from_probes in
      if leaves = [] then None else Some (assemble_tools_prefix c ~leaves)

(* -- sync ---------------------------------------------------------------- *)

(* Load [cwd]/*.opam + URL-project overlays + [--with-repo] handles
   into one deduped handle list, then resolve the toolchain against it.
   This is the project-aware [tc_handles] formula every command should
   use; [oi sync] does it inline below, [oi exec] / [oi env] call this
   helper directly so they pick the same toolchain. *)
let resolve_project_toolchain ?(refresh = false) ?(skip_local = false)
    ?(with_repos = []) ?(with_deps = []) ~fs ~sys ~cache ~data_dir ~conf
    ~install ~override ~cwd () =
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
  let project_overlays =
    if skip_local then []
    else
      match Oi.Project.load ~fs cwd with
      | exception Sys_error _ -> []
      | exception Eio.Exn.Io _ -> []
      | p -> p.overlays
  in
  let _, url_project =
    Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh with_deps
  in
  let tc_handles =
    project_overlays @ url_project.overlays
    @ Target.handles_of_tokens with_repos
    |> List.sort_uniq String.compare
  in
  Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf ~install ~override
    ~handles:tc_handles ()

(* Run a full sync in [cwd]: solve the deps declared in *.opam files,
   build/fetch layers, assemble [cwd]/_oi/prefix, and (re)write .envrc.
   Returns the assembled prefix path and the resolved toolchain. When
   [quiet] is true, narration goes to Log.info (hidden at default
   verbosity); otherwise it prints to stdout. *)
type envrc_mode = [ `Skip | `Always | `Detect ]

let pp_envrc_mode ppf = function
  | `Skip -> Fmt.string ppf "skip"
  | `Always -> Fmt.string ppf "always"
  | `Detect -> Fmt.string ppf "detect"

(* True if [p] is an executable file we can run. *)
let executable_exists p =
  try
    Unix.access p [ Unix.X_OK ];
    true
  with Unix.Unix_error _ -> false

(* [d] is a non-empty PATH segment containing the [direnv] executable. *)
let dir_has_direnv d = d <> "" && executable_exists (d / "direnv")

(* True if [direnv] is on PATH. Resolved at sync time so users who add
   direnv mid-project pick it up on the next [oi sync] / [oi build]. *)
let direnv_on_path () =
  match Sys.getenv_opt "PATH" with
  | None | Some "" -> false
  | Some path -> String.split_on_char ':' path |> List.exists dir_has_direnv

let envrc_should_write = function
  | `Skip -> false
  | `Always -> true
  | `Detect -> direnv_on_path ()

(* Quiet-aware narration helpers used in {!run}. In [quiet] mode they
   drop to [Log.info]; otherwise they emit a stable [Oi.Say.*] line. *)
let say_step ~quiet fmt =
  if quiet then Fmt.kstr (fun s -> Log.info (fun m -> m "%s" s)) fmt
  else Fmt.kstr (fun s -> Oi.Say.step "%s" s) fmt

let say_info ~quiet fmt =
  if quiet then Fmt.kstr (fun s -> Log.info (fun m -> m "%s" s)) fmt
  else Fmt.kstr (fun s -> Oi.Say.info "%s" s) fmt

let say_field_list ~quiet label items =
  if quiet then
    begin if items <> [] then
      Log.info (fun m -> m "%s: %s" label (String.concat ", " items))
    end
  else Oi.Say.field_list label items

(* Build the human-readable error summary for a sync that returned no
   plan: each group's failure kind, suitable for [fail_config_error]. *)
let group_error_lines (groups : Oi.Build_pipeline.group_result list) =
  List.filter_map
    (fun (gr : Oi.Build_pipeline.group_result) ->
      match gr.error with
      | Ok () -> None
      | Error e ->
          let kind =
            match e with
            | Solve_failed { msg; log_path } ->
                Fmt.str "solve: %s (see %s)" msg log_path
            | Cycle _ -> "cycle"
            | Empty_after_strip -> "empty"
            | Elaborate_failed { msg } -> Fmt.str "elaborate: %s" msg
            | Emit_failed { msg } -> Fmt.str "emit: %s" msg
          in
          Fmt.kstr (fun s -> Some s) "%s — %s" gr.group.label kind)
    groups

(* Surface solve/build failures: a discarded build result lets a failed
   solve fall through to [Prefix.assemble ~layer_hashes:[]], which
   silently produces an empty [_oi/prefix] and then dies further
   downstream with the misleading "Executable dune not found". *)
let check_sync_outcome ~solved ~build_result =
  match build_result with
  | None ->
      let lines = group_error_lines solved.Oi.Build_pipeline.groups in
      Oi.Error.fail_config_error
        "project sync produced no executable plan:@\n\
        \  %s@\n\
         Re-run with --verbosity=debug for the full per-group trace."
        (String.concat "\n  " lines)
  | Some r when r.D10ir.Direct.failed > 0 ->
      let pp_fail (f : D10ir.Direct.failure) =
        Fmt.str "%s.%s @ %s — see %s" f.package.name f.package.version
          (D10ir.Direct.string_of_phase f.phase)
          f.log_path
      in
      Oi.Error.fail_config_error "project sync had %d build failure(s):@\n  %s"
        r.failed
        (String.concat "\n  " (List.map pp_fail r.failures))
  | Some _ -> ()

(* Resolved up-front state from the [run] entry: project + URL project
   load, deps, picked toolchain, merged extras, the [Build_pipeline.env],
   the [Build_pipeline.request], and the layer_remote / source_remote
   chosen by [--use-registry]. Pulled into a record so the lower-level
   helpers stay below the line-count / nesting limits. *)
type state = {
  project : Oi.Project.t;
  toolchain : Oi.Toolchain.info option;
  conf : Oi.Solver.Ctx.conf;
  all_extras : Oi.Project.extra_repo list;
  layer_remote : D10.Layer.remote option;
  source_remote : D10.Layer.remote option;
  pipeline_env : Oi.Build_pipeline.env;
  req : Oi.Build_pipeline.request;
}

(* All of the inputs to [Sync.run]: assembled into one record so the
   helpers split out of the giant body each take one parameter rather
   than 17. *)
type run_inputs = {
  quiet : bool;
  refresh : bool;
  skip_local : bool;
  with_repos : string list;
  with_deps : string list;
  jobs : int option;
  toolchain : string option;
  envrc_mode : envrc_mode;
  with_test : bool;
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  sys : D10.Sysops.t;
  platform : Osrel.t;
  os_key : string;
  cache : Oi.Cache.t;
  data_dir : string;
  registry : string;
  use_registry : Oi.Use_registry.t;
  session : D10.Sysops.Http.session;
  cwd : string;
}

(* Build the solver-input [names] (deps from *.opam + extras from --with +
   URL-project roots, with toolchain compiler roots stripped). [oi-docs] is
   injected here so voodoo docs are produced by default — consumers don't
   have to opt in by adding the conf-package to their depends.
   {!Solver.find_opam_file_in} adds a matching depopt to the compiler's
   opam in-memory, so once oi-docs lands in the solve it folds into every
   package's {!D10.Layer.hash} via the compiler's transitive closure. *)
let oi_docs_name = OpamPackage.Name.of_string "oi-docs"

let build_root_names ~(project : Oi.Project.t) ~(url_project : Oi.Project.Url.t)
    ~extra_cli ~toolchain_override ~toolchain =
  let extra_names =
    List.filter_map
      (fun (d : Oi.Project.Script.dep) ->
        if OpamPackage.Name.to_string d.name = "ocaml" then None
        else Some d.name)
      extra_cli
  in
  let url_names = List.map OpamPackage.Name.of_string url_project.roots in
  let project_names = List.map OpamPackage.Name.of_string project.deps in
  let all = project_names @ extra_names @ url_names in
  let with_docs =
    if List.exists (OpamPackage.Name.equal oi_docs_name) all then all
    else oi_docs_name :: all
  in
  with_docs
  |> Oi.Pipeline.strip_compiler_roots_for_override ~override:toolchain_override
       ~toolchain

(* Load *.opam metadata + classify [--with]-derived deps. Errors out
   when nothing buildable was found. *)
let load_project_and_deps (i : run_inputs) =
  let project =
    if i.skip_local then Oi.Project.empty else Oi.Project.load ~fs:i.fs i.cwd
  in
  let extra_cli, url_project =
    Oi.Pipeline.classify_with_args ~fs:i.fs ~sys:i.sys ~cache:i.cache
      ~refresh:i.refresh i.with_deps
  in
  if project.deps = [] && extra_cli = [] && url_project.roots = [] then
    Oi.Error.fail_config_error "No .opam files found in %s." i.cwd;
  say_step ~quiet:i.quiet "Sync %s" i.cwd;
  say_field_list ~quiet:i.quiet "deps" project.deps;
  say_field_list ~quiet:i.quiet "with-deps" url_project.roots;
  (project, extra_cli, url_project)

(* Pick the toolchain, then derive the [with_repos] handle set that
   matches it and the merged extra-repo list. *)
let resolve_overlays_and_toolchain (i : run_inputs) ~(project : Oi.Project.t)
    ~(url_project : Oi.Project.Url.t) =
  let conf =
    Oi.Pipeline.conf ~platform:i.platform ~ocaml_version:Workspace.ocaml_version
  in
  let candidate_overlays = project.overlays @ url_project.overlays in
  let tc_handles =
    candidate_overlays @ Target.handles_of_tokens i.with_repos
    |> List.sort_uniq String.compare
  in
  let toolchain =
    Oi.Pipeline.pick_toolchain ~fs:i.fs ~sys:i.sys ~data_dir:i.data_dir ~conf
      ~install:true ~override:i.toolchain ~handles:tc_handles ()
  in
  let conf, _ = Oi.Pipeline.solver_inputs toolchain conf in
  let project_overlays =
    Oi.Pipeline.filter_compatible_overlays
      ~reporepo_path:(Terms.reporepo_path ()) ~override:i.toolchain ~toolchain
      candidate_overlays
  in
  say_field_list ~quiet:i.quiet "overlays" project_overlays;
  let with_repos = project_overlays @ i.with_repos in
  let all_extras =
    Target.merge_extras
      ~cli:(Target.cli_extra_repos ~fs:i.fs ~sys:i.sys ?toolchain with_repos)
      ~project:(project.extra_repos @ url_project.extra_repos)
  in
  let extra_repo_labels =
    List.map
      (fun (e : Oi.Project.extra_repo) -> Fmt.str "%s (%s)" e.name e.url)
      all_extras
  in
  say_field_list ~quiet:i.quiet "extra-repos" extra_repo_labels;
  (toolchain, conf, with_repos, all_extras)

(* Build the [Build_pipeline.env] from harness capabilities. *)
let pipeline_env_of_inputs (i : run_inputs) : Oi.Build_pipeline.env =
  {
    proc_mgr = i.proc_mgr;
    fs = i.fs;
    clock = i.clock;
    sys = i.sys;
    os_key = i.os_key;
    cache = i.cache;
    data_dir = i.data_dir;
    http_session = i.session;
  }

(* Resolve everything needed to issue a [Build_pipeline.solve]: load
   project metadata, classify --with extras, pick toolchain, merge
   extra repos, and build the request. *)
let prepare_state (i : run_inputs) : state =
  let project, extra_cli, url_project = load_project_and_deps i in
  let toolchain, conf, with_repos, all_extras =
    resolve_overlays_and_toolchain i ~project ~url_project
  in
  let { Terms.layer_remote; source_remote } =
    Terms.remotes_of ~url:i.registry ~mode:i.use_registry
  in
  let extra_constraints = Oi.Project.Script.constraints extra_cli in
  let names =
    build_root_names ~project ~url_project ~extra_cli
      ~toolchain_override:i.toolchain ~toolchain
  in
  let local_packages_dir =
    match project.packages_dir with
    | Some _ -> project.packages_dir
    | None -> url_project.packages_dir
  in
  let pipeline_env = pipeline_env_of_inputs i in
  let req : Oi.Build_pipeline.request =
    {
      targets =
        [
          Group
            { tokens = List.map OpamPackage.Name.to_string names; handles = [] };
        ];
      (* The project's overlay handles (from [x-repos: ["@HANDLE"]] in
         every *.opam, plus any [--with-repo @HANDLE] CLI flag) must
         flow through to [Build_pipeline] so the solver's
         [packages_dirs_for_group] resolves them into overlay paths.
         Passing [[]] here was the root cause of "No known
         implementations at all" for overlay-only packages ([nox-tty],
         [requests], …): [all_extras] still pinned the handles'
         [local_packages_dir]s, but [solve_uncached] only reads
         [req.with_repos] when computing [global_handles]. *)
      with_repos;
      pins = project.pins @ url_project.pins;
      extra_repos = all_extras;
      constraints = extra_constraints;
      toolchain_override = i.toolchain;
      toolchain;
      conf;
      local_packages_dir;
      project_root = Some i.cwd;
      force_source = false;
      with_test = i.with_test;
      refresh = i.refresh;
    }
  in
  {
    project;
    toolchain;
    conf;
    all_extras;
    layer_remote;
    source_remote;
    pipeline_env;
    req;
  }

(* Solve + build the project request, surfacing failures and returning
   the per-package layer hashes for the upcoming [Prefix.assemble].
   [doc_tools_dir] is the absolute path to [_oi/tools/] from a preceding
   [install_tools] pass; when [None] the doc step no-ops even if
   [oi-docs] is in the solve. *)
let solve_and_build ?(doc_tools_dir : string option) (i : run_inputs)
    (s : state) =
  Progress_ui.with_ui ~target:i.cwd
    ~clock:(i.clock :> _ Eio.Resource.t)
    ~enabled:((not i.quiet) && Tty.is_tty ())
  @@ fun reporter ->
  let solved = Oi.Build_pipeline.solve s.pipeline_env ~reporter s.req in
  let build_result =
    Oi.Build_pipeline.build s.pipeline_env ~reporter
      {
        solved;
        layer_remote = s.layer_remote;
        source_remote = s.source_remote;
        jobs = i.jobs;
        upload_archive_url = None;
        archive_sources = false;
        snapshot_reporepo = false;
        doc_tools_dir;
      }
  in
  check_sync_outcome ~solved ~build_result;
  Oi.Build_pipeline.layer_hashes solved

(* Drop a fresh [.envrc] alongside the project. Caller has already
   decided we should write one based on {!envrc_should_write}. *)
let write_envrc (i : run_inputs) (s : state) ~prefix ~tools =
  let envrc_path = Eio.Path.(i.fs / i.cwd / ".envrc") in
  let dune_cache_root = Oi.Cache.dune_root i.cache in
  let tc_ctx = Option.map Oi.Toolchain.opam_ctx_of_info s.toolchain in
  let envrc =
    Oi.Solver.Env.envrc_content ?toolchain:tc_ctx ~prefix ?tools
      ~dune_cache_root ()
  in
  (try Eio.Path.unlink envrc_path with Eio.Exn.Io _ -> ());
  Eio.Path.save ~create:(`Exclusive 0o644) envrc_path envrc;
  say_step ~quiet:i.quiet "Writing .envrc";
  say_info ~quiet:i.quiet "run 'direnv allow' to activate"

let run_with_inputs (i : run_inputs) =
  Oi.Pipeline.init_opam_root ~fs:i.fs ~data_dir:i.data_dir;
  ignore
    (Oi.Source.Reporepo.ensure_base ~fs:i.fs ~sys:i.sys ~data_dir:i.data_dir
       ~refresh:i.refresh ());
  let s = prepare_state i in
  (* Install dev tools FIRST so [_oi/tools/] exists before the main solve
     runs. When the project's depends carry [oi-docs], the build pipeline
     stages tool binaries (odoc_driver_voodoo, odoc, odoc-md, sherlodoc)
     from this path into each package's build and runs voodoo as a
     post-install step. Tool install failures stay best-effort
     ([install_named] swallows them); a missing tool path just means the
     doc step no-ops. *)
  say_step ~quiet:i.quiet "Installing dev tools";
  let tools =
    install_tools ~quiet:i.quiet ?refresh:(Some i.refresh) ?jobs:i.jobs
      ~proc_mgr:i.proc_mgr ~fs:i.fs ~clock:i.clock ~sys:i.sys ~cache:i.cache
      ~data_dir:i.data_dir ~conf:s.conf ~os_key:i.os_key ~session:i.session
      ~extra_repos:s.all_extras ~pins:s.project.pins ?toolchain:s.toolchain
      ?layer_remote:s.layer_remote ?source_remote:s.source_remote ~cwd:i.cwd ()
  in
  let layer_hashes = solve_and_build ?doc_tools_dir:tools i s in
  let oi_dir = i.cwd / "_oi" in
  let prefix = oi_dir / "prefix" in
  Eio.Path.rmtree ~missing_ok:true Eio.Path.(i.fs / prefix);
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(i.fs / oi_dir);
  let d10 =
    Oi.Pipeline.d10 ~sys:i.sys ~fs:i.fs ~clock:i.clock ~cache:i.cache
      ~os_key:i.os_key
  in
  say_step ~quiet:i.quiet "Assembling project prefix";
  D10.Prefix.assemble d10 ~layer_hashes ~dst:Eio.Path.(i.fs / prefix);
  if envrc_should_write i.envrc_mode then write_envrc i s ~prefix ~tools
  else
    Log.info (fun m ->
        m "Skipping .envrc (--envrc=%a)" pp_envrc_mode i.envrc_mode);
  say_step ~quiet:i.quiet "Prefix assembled at %s (%d packages)" prefix
    (List.length layer_hashes);
  (prefix, s.toolchain)

let run ?(quiet = false) ?(refresh = false) ?(skip_local = false)
    ?(with_repos = []) ?(with_deps = []) ?jobs ?(toolchain : string option)
    ?(envrc_mode = `Detect) ?(with_test = false) ~proc_mgr ~fs ~clock ~sys
    ~platform ~os_key ~cache ~data_dir ~registry ~use_registry ~session ~cwd ()
    =
  run_with_inputs
    {
      quiet;
      refresh;
      skip_local;
      with_repos;
      with_deps;
      jobs;
      toolchain;
      envrc_mode;
      with_test;
      proc_mgr;
      fs;
      clock;
      sys;
      platform;
      os_key;
      cache;
      data_dir;
      registry;
      use_registry;
      session;
      cwd;
    }

(* True if [cwd]/_oi/prefix is missing, or any *.opam in [cwd] has been
   modified more recently than the prefix directory. *)
let needs_sync ~cwd ~prefix =
  match Unix.stat prefix with
  | exception Unix.Unix_error _ -> true
  | st ->
      let prefix_mtime = st.Unix.st_mtime in
      let opam_files =
        try
          Sys.readdir cwd |> Array.to_list
          |> List.filter (fun f ->
              Filename.check_suffix f ".opam"
              && Filename.chop_suffix f ".opam" <> "")
        with Sys_error _ -> []
      in
      List.exists
        (fun f ->
          try (Unix.stat (cwd / f)).Unix.st_mtime > prefix_mtime
          with Unix.Unix_error _ -> false)
        opam_files

(* Cmdliner term for $(b,--envrc=skip|always|detect). Shared with
   $(b,oi build). *)
let envrc_mode_arg =
  let modes =
    Cmdliner.Arg.enum
      [ ("skip", `Skip); ("always", `Always); ("detect", `Detect) ]
  in
  Cmdliner.Arg.(
    value & opt modes `Detect
    & info ~docv:"MODE"
        ~doc:
          "When to write $(b,.envrc): $(b,skip), $(b,always), or $(b,detect) \
           (default — write only if $(b,direnv) is on PATH)."
        [ "envrc" ])
