open Cmdliner

let log_src = Logs.Src.create "oi.cmd.run" ~doc:"oi run command"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

(* Flags collapsed into a record so call sites can't accidentally swap
   booleans (E350). [Terms]-derived [Term.t] builds one of these. *)
type flags = {
  refresh : bool;
  locked : bool;
  skip_local : bool;
  dry_run : bool;
}

(* -- target prefix parsing --------------------------------------------- *)

type parsed_target = {
  target : string;
  binary_name : string;
  with_repos : string list;
  with_deps : string list;
  with_pins : Target.handle_pin list;
  target_pin : Target.handle_pin option;
}

(* [TARGET] and every [--with] token accept the [@handle/pkg[constraint]]
   shortcut. The handle is routed into [with_repos] so the overlay
   joins the solve; the stripped package spec takes its place; each
   handle_pin is then pinned to the overlay's version below. *)
let parse_target ~target ~with_repos ~with_deps =
  let target, with_repos, with_deps, target_pin =
    match Target.split_handle_prefix target with
    | None -> (target, with_repos, with_deps, None)
    | Some (h, pkg_spec) ->
        let pkg, user_constr = OpamFormula.atom_of_string pkg_spec in
        let pin = { Target.handle = h; pkg; user_constr } in
        ( OpamPackage.Name.to_string pkg,
          with_repos @ [ h ],
          with_deps @ [ pkg_spec ],
          Some pin )
  in
  let with_deps, with_repos, with_pins =
    Target.extract_handle_pins ~with_repos with_deps
  in
  { target; binary_name = target; with_repos; with_deps; with_pins; target_pin }

(* -- fast cache -------------------------------------------------------- *)

(* Fast-path: if we've run these exact args before and the binary still
   exists, skip all expensive work (opam init, reporepo, toolchain
   resolution, pin materialization, solving). *)
type fast_cache_entry = string * string * (string * bool) option

let is_script_target s =
  Filename.check_suffix s ".ml"
  || String.starts_with ~prefix:"http://" s
  || String.starts_with ~prefix:"https://" s

let fast_cache_key ~dry_run ~refresh ~os_key ~binary_name ~toolchain_override
    ~registry ~skip_local ~with_deps ~with_repos ~is_script =
  if dry_run || refresh || is_script then None
  else
    Some
      (Digest.to_hex
         (Digest.string
            (String.concat "\x00"
               ([
                  os_key;
                  binary_name;
                  Stdlib.Option.value ~default:"" toolchain_override;
                  registry;
                  (if skip_local then "skip-local" else "with-local");
                ]
               @ List.sort String.compare with_deps
               @ [ "\x01" ]
               @ List.sort String.compare with_repos))))

let fast_cache_path ~cache_root key =
  cache_root / "run-cache" / String.sub key 0 2 / (key ^ ".marshal")

let store_fast_cache ~fs ~cache_root ~fast_key ~bin_path ~prefix ~tc_ctx =
  match fast_key with
  | None -> ()
  | Some key -> (
      try
        let path = fast_cache_path ~cache_root key in
        let dir = Filename.dirname path in
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dir);
        let entry : fast_cache_entry =
          ( bin_path,
            prefix,
            Option.map
              (fun (tc : Oi.Solver.Ctx.toolchain) ->
                (tc.install_prefix, tc.relocatable))
              tc_ctx )
        in
        let tmp = path ^ ".tmp" in
        Out_channel.with_open_bin tmp (fun oc -> Marshal.to_channel oc entry []);
        Sys.rename tmp path
      with Sys_error _ -> ())

let load_fast_cache ~cache_root key : fast_cache_entry option =
  try
    Some
      (In_channel.with_open_bin (fast_cache_path ~cache_root key) (fun ic ->
           (Marshal.from_channel ic : fast_cache_entry)))
  with Sys_error _ -> None

let tc_ctx_of_info tc_info : Oi.Solver.Ctx.toolchain option =
  match tc_info with
  | None -> None
  | Some (install_prefix, relocatable) ->
      Some
        {
          Oi.Solver.Ctx.install_prefix;
          hash = "";
          relocatable;
          packages = OpamPackage.Set.empty;
          root_names = OpamPackage.Name.Set.empty;
        }

let try_fast_cache_exec ~proc_mgr ~cache_root ~dune_cache_root ~fast_key ~args =
  match fast_key with
  | None -> ()
  | Some key -> (
      match load_fast_cache ~cache_root key with
      | Some (bin_path, prefix, tc_info) when Sys.file_exists bin_path ->
          let tc_ctx = tc_ctx_of_info tc_info in
          let env =
            Oi.Solver.Env.make_env ?toolchain:tc_ctx ~prefix ~dune_cache_root ()
          in
          exit (Subprocess.run proc_mgr ~env (bin_path :: args))
      | _ -> ())

(* -- project + URL extras --------------------------------------------- *)

(* Project-extras + URL-project merged context. The setup ([opam init,
   reporepo ensure_base, classify, project load, toolchain pick]) is
   shared between the script and binary code paths. *)
type ctx = {
  fs : Eio.Fs.dir_ty Eio.Path.t;
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  sys : D10.Sysops.t;
  os_key : string;
  cache : Oi.Cache.t;
  data_dir : string;
  http_session : D10.Sysops.Http.session;
  cache_root : string;
  dune_cache_root : string;
  conf : Oi.Solver.Ctx.conf;
  layer_remote : D10.Layer.remote option;
  source_remote : D10.Layer.remote option;
  refresh : bool;
  dry_run : bool;
  jobs : int option;
  save_d10ir : string option;
  toolchain : Oi.Toolchain.info option;
  toolchain_override : string option;
  with_repos : string list;
  project_pins : Oi.Project.pin list;
  all_extras : Oi.Project.extra_repo list;
  extra_constraints : OpamFormula.version_constraint OpamPackage.Name.Map.t;
  local_packages_dir : string option;
  extra_deps : Oi.Project.Script.dep list;
  url_project : Oi.Project.Url.t;
  cwd : Eio.Fs.dir_ty Eio.Path.t;
  registry : string;
  fast_key : string option;
}

let load_project_extras ~fs ~skip_local cwd_s =
  if skip_local then ([], [], [], None)
  else
    match Oi.Project.load ~fs cwd_s with
    | exception Sys_error _ -> ([], [], [], None)
    | exception Eio.Exn.Io _ -> ([], [], [], None)
    | p -> (p.extra_repos, p.pins, p.overlays, p.packages_dir)

let resolve_toolchain ~fs ~sys ~data_dir ~conf ~toolchain_override ~pt
    ~project_overlays =
  let tc_handles =
    Target.pin_handles (Stdlib.Option.to_list pt.target_pin @ pt.with_pins)
    @ Target.handles_of_tokens pt.with_repos
    @ project_overlays
    |> List.sort_uniq String.compare
  in
  Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf ~install:true
    ~override:toolchain_override ~handles:tc_handles ()

type project_inputs = {
  project_extras : Oi.Project.extra_repo list;
  project_pins : Oi.Project.pin list;
  project_overlays : string list;
  local_packages_dir : string option;
  cwd : Eio.Fs.dir_ty Eio.Path.t;
}

let collect_project_inputs ~fs ~skip_local ~url_project =
  let cwd_s, cwd = Workspace.resolved_cwd fs in
  let project_extras, project_pins, project_overlays, project_packages_dir =
    load_project_extras ~fs ~skip_local cwd_s
  in
  let local_packages_dir =
    match project_packages_dir with
    | Some _ -> project_packages_dir
    | None -> url_project.Oi.Project.Url.packages_dir
  in
  {
    project_extras = project_extras @ url_project.extra_repos;
    project_pins = project_pins @ url_project.pins;
    project_overlays = project_overlays @ url_project.overlays;
    local_packages_dir;
    cwd;
  }

let merged_extra_constraints ~fs ~data_dir ~refresh ~cli_extras ~handle_pins
    ~base =
  let handle_constraints =
    Target.handle_pin_constraints ~fs ~data_dir ~refresh ~cli_extras handle_pins
  in
  OpamPackage.Name.Map.union (fun a _ -> a) handle_constraints base

(* Resolve all the inputs that depend on the [Target.cli_extra_repos] lookup
   path: drop project overlays whose toolchain tag disagrees with
   [toolchain_override], extend with the user-supplied [--with-repo] /
   [@h/pkg] handles, materialise the resulting extra-repo list, and
   merge handle-pin version constraints into [base_constraints]. *)
let merge_repos_and_constraints ~fs ~sys ~data_dir ~refresh ~toolchain
    ~toolchain_override ~(pt : parsed_target) ~pi ~base_constraints =
  let project_overlays =
    Oi.Pipeline.filter_compatible_overlays
      ~reporepo_path:(Terms.reporepo_path ()) ~override:toolchain_override
      ~toolchain pi.project_overlays
  in
  let with_repos = project_overlays @ pt.with_repos in
  let cli_extras = Target.cli_extra_repos ~fs ~sys ?toolchain with_repos in
  let all_extras =
    Target.merge_extras ~cli:cli_extras ~project:pi.project_extras
  in
  let handle_pins = Stdlib.Option.to_list pt.target_pin @ pt.with_pins in
  let extra_constraints =
    merged_extra_constraints ~fs ~data_dir ~refresh ~cli_extras ~handle_pins
      ~base:base_constraints
  in
  (with_repos, all_extras, extra_constraints)

let build_ctx_from_harness (h : Harness.env) (flags : flags) registry
    use_registry toolchain_override pt save_d10ir jobs ~fast_key ~data_dir =
  let {
    Harness.proc_mgr;
    fs;
    clock;
    sys;
    platform;
    os_key;
    cache;
    http_session;
    _;
  } =
    h
  in
  let refresh = flags.refresh && not flags.locked in
  let use_registry =
    if flags.locked then Oi.Use_registry.Never else use_registry
  in
  let dune_cache_root = Oi.Cache.dune_root cache in
  let cache_root = Oi.Cache.root_s cache in
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
  let conf =
    Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  let { Terms.layer_remote; source_remote } =
    Terms.remotes_of ~url:registry ~mode:use_registry
  in
  let extra_deps, url_project =
    Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh pt.with_deps
  in
  let base_constraints = Oi.Project.Script.constraints extra_deps in
  let pi =
    collect_project_inputs ~fs ~skip_local:flags.skip_local ~url_project
  in
  let toolchain =
    resolve_toolchain ~fs ~sys ~data_dir ~conf ~toolchain_override ~pt
      ~project_overlays:pi.project_overlays
  in
  let with_repos, all_extras, extra_constraints =
    merge_repos_and_constraints ~fs ~sys ~data_dir ~refresh ~toolchain
      ~toolchain_override ~pt ~pi ~base_constraints
  in
  {
    fs;
    proc_mgr;
    clock;
    sys;
    os_key;
    cache;
    data_dir;
    http_session;
    cache_root;
    dune_cache_root;
    conf;
    layer_remote;
    source_remote;
    refresh;
    dry_run = flags.dry_run;
    jobs;
    save_d10ir;
    toolchain;
    toolchain_override;
    with_repos;
    project_pins = pi.project_pins;
    all_extras;
    extra_constraints;
    local_packages_dir = pi.local_packages_dir;
    extra_deps;
    url_project;
    cwd = pi.cwd;
    registry;
    fast_key;
  }

(* -- recipe persistence + binary lookup ------------------------------- *)

let recipe_stem_token token =
  String.map
    (fun c ->
      if
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
        || c = '.' || c = '-' || c = '_'
      then c
      else '_')
    token

let save_solved_recipes ~fs ~dir (solved : Oi.Build_pipeline.solved) ~mk_stem =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dir);
  List.iter
    (fun (gr : Oi.Build_pipeline.group_result) ->
      match gr.recipe with
      | None -> ()
      | Some recipe ->
          let stem = mk_stem gr in
          let dst = Filename.concat dir (stem ^ ".d10ir.json") in
          D10ir.Plan.save Eio.Path.(fs / dst) recipe)
    solved.groups

let solve_stem (gr : Oi.Build_pipeline.group_result) =
  match gr.group.tokens with t :: _ -> recipe_stem_token t | [] -> "recipe"

let print_dry_run_plan (solved : Oi.Build_pipeline.solved) =
  (match solved.merged with
  | None -> Fmt.pr "(no packages to build)@."
  | Some merged ->
      List.iter
        (fun (n : D10ir.Plan.node) ->
          Fmt.pr "%s.%s  %s@." n.package.name n.package.version
            (D10ir.Layer_hash.to_string n.layer_hash))
        merged.nodes);
  exit 0

(* [bin/] contents of the layer whose package matches [want_name].
   Walks [hashes], reads each [layer.json], and lists the matching
   layer's [fs/bin/]. Empty when no such layer exists or it ships
   nothing in [bin/]. *)
let layer_binaries ~fs ~cache ~os_key ~hashes ~want_name =
  let layers_dir = Oi.Cache.root_s cache / "layers" / os_key in
  let owns_target hash =
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
  match List.find_map owns_target hashes with
  | None -> []
  | Some hash -> (
      try
        Eio.Path.read_dir Eio.Path.(fs / layers_dir / hash / "fs" / "bin")
        |> List.sort String.compare
      with Eio.Exn.Io _ -> [])

(* -- build-result interpretation --------------------------------------- *)

let group_error_kind = function
  | Oi.Build_pipeline.Solve_failed _ -> "solve"
  | Cycle _ -> "cycle"
  | Empty_after_strip -> "empty"
  | Elaborate_failed _ -> "elaborate"
  | Emit_failed _ -> "emit"

let fail_no_executable_plan (solved : Oi.Build_pipeline.solved) =
  let group_msgs =
    List.filter_map
      (fun (gr : Oi.Build_pipeline.group_result) ->
        match gr.error with
        | Ok () -> None
        | Error e ->
            Fmt.kstr
              (fun s -> Some s)
              "%s: %s" gr.group.label (group_error_kind e))
      solved.groups
  in
  Oi.Error.fail_config_error
    "build pipeline produced no executable plan (%s). Re-run with \
     --verbosity=debug for the per-group trace."
    (String.concat ", " group_msgs)

(* Direct.run was handed an empty plan. Two possibilities:
   (a) Warm cache — every solved package was marked [Binary] by
       [Plan.elaborate] because its layer is already in the local d10
       cache; recipe emit produced zero source nodes. Nothing wrong.
   (b) Recipe emit dropped packages whose layers don't actually exist
       on disk. Real bug; surface a precise diagnostic.
   Distinguish by probing every package the solve claims via
   [D10.Layer.succeeded]. *)
let missing_layer ~d10_cfg (pp : Oi.Plan.package_plan) =
  if D10.Layer.succeeded d10_cfg ~hash:pp.layer_hash then None
  else Some (pp.pkg, pp.layer_hash)

let missing_layers_in_group ~d10_cfg (gr : Oi.Build_pipeline.group_result) =
  match gr.exec_plan with
  | None -> []
  | Some (p : Oi.Plan.t) -> List.filter_map (missing_layer ~d10_cfg) p.packages

let missing_layers_for_solved ~sys ~fs ~clock ~cache ~os_key
    (solved : Oi.Build_pipeline.solved) =
  let d10_cfg =
    Oi.Pipeline.d10 ~sys ~fs ~clock:(clock :> D10.Config.clk) ~cache ~os_key
  in
  List.concat_map (missing_layers_in_group ~d10_cfg) solved.groups

let fail_empty_plan_with_missing xs =
  let summary =
    List.map
      (fun (pkg, h) ->
        Fmt.str "  %s (%s)" pkg (String.sub h 0 (min 12 (String.length h))))
      xs
    |> String.concat "\n"
  in
  Oi.Error.fail_config_error
    "build pipeline ran with an empty d10ir plan, but %d package(s) claim \
     cached layers that aren't in the local d10 cache:@\n\
     %s@\n\
     Likely a recipe-emit bug. Re-run with $(b,--refresh) to force a fresh \
     solve, or $(b,oi clean --layers) followed by a normal build to repopulate \
     them."
    (List.length xs) summary

let fail_build_failures (r : D10ir.Direct.result) =
  let pp_fail (f : D10ir.Direct.failure) =
    Fmt.str "%s.%s @ %s: %s — see %s" f.package.name f.package.version
      (D10ir.Direct.string_of_phase f.phase)
      f.error f.log_path
  in
  if r.failures <> [] then begin
    let summary = List.map pp_fail r.failures |> String.concat "\n  " in
    Oi.Error.fail_config_error
      "build failed: %d node(s) failed, %d skipped.@\n  %s" r.failed r.skipped
      summary
  end
  else
    Oi.Error.fail_config_error
      "build failed: 0 nodes built, %d skipped (likely a dep chain broke \
       upstream). Re-run with --verbosity=debug for the per-node trace."
      r.skipped

let check_empty_plan ~sys ~fs ~clock ~cache ~os_key solved =
  match missing_layers_for_solved ~sys ~fs ~clock ~cache ~os_key solved with
  | [] -> ()
  | xs -> fail_empty_plan_with_missing xs

let interpret_build_result ~sys ~fs ~clock ~cache ~os_key solved = function
  | None -> fail_no_executable_plan solved
  | Some (r : D10ir.Direct.result)
    when r.failed = 0 && r.skipped = 0 && r.built = 0 && r.cached = 0 ->
      check_empty_plan ~sys ~fs ~clock ~cache ~os_key solved
  | Some r when r.failed = 0 && r.skipped = 0 -> ()
  | Some r -> fail_build_failures r

(* -- solve and exec helpers ------------------------------------------- *)

let pipeline_env_of_ctx ctx : Oi.Build_pipeline.env =
  {
    proc_mgr = ctx.proc_mgr;
    fs = ctx.fs;
    clock = ctx.clock;
    sys = ctx.sys;
    os_key = ctx.os_key;
    cache = ctx.cache;
    data_dir = ctx.data_dir;
    http_session = ctx.http_session;
  }

let solve_request ctx ~names : Oi.Build_pipeline.request =
  {
    targets =
      [
        Group
          { tokens = List.map OpamPackage.Name.to_string names; handles = [] };
      ];
    (* Overlay handles from [--with=@H/PKG] / [--with-repo=@H] / project
       [x-repos] must flow into the request so the solver's
       [packages_dirs_for_group] picks up their [packages/] trees. *)
    with_repos = ctx.with_repos;
    pins = ctx.project_pins;
    extra_repos = ctx.all_extras;
    constraints = ctx.extra_constraints;
    toolchain_override = ctx.toolchain_override;
    toolchain = ctx.toolchain;
    conf = ctx.conf;
    local_packages_dir = ctx.local_packages_dir;
    project_root = None;
    force_source = false;
    with_test = false;
    refresh = ctx.refresh;
  }

let drive_solve_and_build ~ctx ~target ~req =
  let pipeline_env = pipeline_env_of_ctx ctx in
  Progress_ui.with_ui ~target
    ~clock:(ctx.clock :> _ Eio.Resource.t)
    ~enabled:(Tty.is_tty ())
  @@ fun reporter ->
  let solved = Oi.Build_pipeline.solve pipeline_env ~reporter req in
  if ctx.dry_run then print_dry_run_plan solved;
  (match ctx.save_d10ir with
  | None -> ()
  | Some dir -> save_solved_recipes ~fs:ctx.fs ~dir solved ~mk_stem:solve_stem);
  let build_result =
    Oi.Build_pipeline.build pipeline_env ~reporter
      {
        solved;
        layer_remote = ctx.layer_remote;
        source_remote = ctx.source_remote;
        jobs = ctx.jobs;
        upload_archive_url = None;
        archive_sources = false;
        snapshot_reporepo = false;
      }
  in
  interpret_build_result ~sys:ctx.sys ~fs:ctx.fs ~clock:ctx.clock
    ~cache:ctx.cache ~os_key:ctx.os_key solved build_result;
  Oi.Build_pipeline.layer_hashes solved

let exec_in_prefix ctx ~prefix ~tc_ctx ~bin_path ~args =
  let env_vars () =
    Oi.Solver.Env.make_env ?toolchain:tc_ctx ~prefix
      ~dune_cache_root:ctx.dune_cache_root ()
  in
  store_fast_cache ~fs:ctx.fs ~cache_root:ctx.cache_root ~fast_key:ctx.fast_key
    ~bin_path ~prefix ~tc_ctx;
  exit (Subprocess.run ctx.proc_mgr ~env:(env_vars ()) (bin_path :: args))

(* Non-relocatable toolchains keep their compiler binaries (ocamlc,
   ocamlfind, ...) at the fixed toolchain prefix rather than in the
   consumer prefix; [Opam_ctx.create] marks those packages as already
   installed so they're not rebuilt into the consumer side. Look there
   before declaring the binary missing. *)
let toolchain_bin_path ~fs ~binary_name = function
  | Some (info : Oi.Toolchain.info) when not info.relocatable ->
      let p = info.install_prefix / "bin" / binary_name in
      if Workspace.path_exists fs p then Some p else None
  | _ -> None

(* For [@handle/pkg], list the binaries that pkg's own layer ships
   (not the whole prefix). For plain targets we don't yet know which
   package owns the binary, so fall back to the whole prefix [bin/]. *)
let bins_for_error ~ctx ~target_pin ~prefix ~layer_hashes =
  match target_pin with
  | Some (pin : Target.handle_pin) ->
      let want_name = OpamPackage.Name.to_string pin.pkg in
      layer_binaries ~fs:ctx.fs ~cache:ctx.cache ~os_key:ctx.os_key
        ~hashes:layer_hashes ~want_name
  | None -> (
      try
        Eio.Path.read_dir Eio.Path.(ctx.fs / prefix / "bin")
        |> List.sort String.compare
      with Eio.Exn.Io _ -> [])

let solve_and_exec ~ctx ~binary_name ~target_pin ~unfound_bins ~args pkg_names =
  (* The solver dedups internally but the log line is noisy if [target]
     and [extra_names] overlap. Pre-dedup so the message reads cleanly. *)
  let pkg_names = List.sort_uniq String.compare pkg_names in
  Log.info (fun m ->
      m "Solving for packages: %s" (String.concat ", " pkg_names));
  let names =
    List.map OpamPackage.Name.of_string pkg_names
    |> Oi.Pipeline.strip_compiler_roots_for_override
         ~override:ctx.toolchain_override ~toolchain:ctx.toolchain
  in
  let req = solve_request ctx ~names in
  let layer_hashes = drive_solve_and_build ~ctx ~target:binary_name ~req in
  Log.info (fun m -> m "Got %d layer hashes" (List.length layer_hashes));
  let prefix =
    Oi.Pipeline.assemble_prefix ~sys:ctx.sys ~fs:ctx.fs ~clock:ctx.clock
      ~cache:ctx.cache ~os_key:ctx.os_key ~layer_hashes
  in
  Log.info (fun m -> m "Assembled prefix at %s" prefix);
  let tc_ctx = Option.map Oi.Toolchain.opam_ctx_of_info ctx.toolchain in
  let bin = prefix / "bin" / binary_name in
  Log.info (fun m -> m "Looking for binary: %s" bin);
  if Workspace.path_exists ctx.fs bin then begin
    Log.info (fun m -> m "Found binary, executing");
    exec_in_prefix ctx ~prefix ~tc_ctx ~bin_path:bin ~args
  end;
  match toolchain_bin_path ~fs:ctx.fs ~binary_name ctx.toolchain with
  | Some p ->
      Log.info (fun m -> m "Found %s in toolchain prefix: %s" binary_name p);
      exec_in_prefix ctx ~prefix ~tc_ctx ~bin_path:p ~args
  | None ->
      let bins = bins_for_error ~ctx ~target_pin ~prefix ~layer_hashes in
      unfound_bins := bins;
      Log.info (fun m ->
          m "Available binaries: %s"
            (if bins = [] then "(none)" else String.concat ", " bins));
      false

(* -- script branch ----------------------------------------------------- *)

(* HTTP(S) script URLs: fetch to a fresh tmp file keeping the [.ml]
   suffix, then treat the local copy as the script path for the rest
   of the run. Run caching keys on the script's content hash, so the
   nondeterministic path doesn't defeat the build cache. *)
let fetch_script_url ~fs ~sys target =
  let is_url s =
    String.starts_with ~prefix:"http://" s
    || String.starts_with ~prefix:"https://" s
  in
  if not (is_url target) then target
  else begin
    let local = Filename.temp_file "oi-script-" ".ml" in
    Log.info (fun m -> m "Fetching %s to %s" target local);
    if not (D10.Sysops.Http.fetch sys ~url:target ~dst:Eio.Path.(fs / local))
    then Oi.Error.fail_not_found target "failed to fetch %s" target;
    local
  end

let script_solve_request ctx ~dep_opam_names ~constraints :
    Oi.Build_pipeline.request =
  {
    targets =
      [
        Group
          {
            tokens = List.map OpamPackage.Name.to_string dep_opam_names;
            handles = [];
          };
      ];
    with_repos = ctx.with_repos;
    pins = ctx.project_pins;
    extra_repos = ctx.all_extras;
    constraints;
    toolchain_override = ctx.toolchain_override;
    toolchain = ctx.toolchain;
    conf = ctx.conf;
    local_packages_dir = ctx.local_packages_dir;
    project_root = None;
    force_source = false;
    with_test = false;
    refresh = ctx.refresh;
  }

let script_solve ~ctx ~req ~target =
  Progress_ui.with_ui ~target
    ~clock:(ctx.clock :> _ Eio.Resource.t)
    ~enabled:(Tty.is_tty ())
  @@ fun reporter ->
  let pipeline_env = pipeline_env_of_ctx ctx in
  let solved = Oi.Build_pipeline.solve pipeline_env ~reporter req in
  if ctx.dry_run then print_dry_run_plan solved;
  (match ctx.save_d10ir with
  | None -> ()
  | Some dir ->
      save_solved_recipes ~fs:ctx.fs ~dir solved ~mk_stem:(fun _ -> "recipe"));
  let _ : D10ir.Direct.result option =
    Oi.Build_pipeline.build pipeline_env ~reporter
      {
        solved;
        layer_remote = ctx.layer_remote;
        source_remote = ctx.source_remote;
        jobs = ctx.jobs;
        upload_archive_url = None;
        archive_sources = false;
        snapshot_reporepo = false;
      }
  in
  Oi.Build_pipeline.layer_hashes solved

let solve_script_deps ~ctx ~target ~dep_opam_names ~constraints =
  if dep_opam_names = [] then []
  else
    let req = script_solve_request ctx ~dep_opam_names ~constraints in
    script_solve ~ctx ~req ~target

let script ~(ctx : ctx) ~target ~args =
  if not (Workspace.path_exists ctx.cwd target) then
    Oi.Error.fail_not_found target "file not found: %s" target;
  let all_script_deps =
    Oi.Project.Script.parse_deps_from_file ~fs:ctx.fs target @ ctx.extra_deps
  in
  let ocaml_name = OpamPackage.Name.of_string "ocaml" in
  let dep_opam_names =
    List.filter_map
      (fun (d : Oi.Project.Script.dep) ->
        if OpamPackage.Name.equal d.name ocaml_name then None else Some d.name)
      all_script_deps
  in
  let constraints = Oi.Project.Script.constraints all_script_deps in
  let dep_opam_names =
    Oi.Pipeline.strip_compiler_roots_for_override
      ~override:ctx.toolchain_override ~toolchain:ctx.toolchain dep_opam_names
  in
  let layer_hashes =
    solve_script_deps ~ctx ~target ~dep_opam_names ~constraints
  in
  if ctx.dry_run && dep_opam_names = [] then exit 0;
  let prefix =
    Oi.Pipeline.assemble_prefix ~sys:ctx.sys ~fs:ctx.fs ~clock:ctx.clock
      ~cache:ctx.cache ~os_key:ctx.os_key ~layer_hashes
  in
  Script_runner.run ~sys:ctx.sys ~fs:ctx.fs ~proc_mgr:ctx.proc_mgr
    ~clock:ctx.clock ~os_key:ctx.os_key ~prefix ~conf:ctx.conf ~cache:ctx.cache
    ~data_dir:ctx.data_dir ?toolchain:ctx.toolchain
    ?source_remote:ctx.source_remote target ctx.extra_deps args

(* -- binary branch ----------------------------------------------------- *)

(* Step-fallback wrapper: only catches solve-time failures
   ([No_solution]) so the caller can move on to the next lookup
   strategy. Anything else — a build failure, a fetch failure, a
   config error — must propagate, otherwise the user sees a misleading
   "binary not found" message after a real error further down the
   stack. *)
let try_step label f =
  try f ()
  with Oi.Error.E e as exn -> (
    match Oi.Error.kind e with
    | K_no_solution | K_not_found ->
        Log.info (fun m -> m "%s failed: %a" label Oi.Error.pp e);
        false
    | _ -> Stdlib.raise exn)

(* Materialise [packages_dirs] once, lazily — both the "is [target] a
   package?" precheck and the dash-prefix search consult it, so we
   share one Pin.materialize + Repo.ensure_extra + Reporepo.ensure_base
   pass. The toolchain's own [info.packages_dirs] takes the place of the
   default base when set (matches what Build_pipeline.build does). *)
let packages_dirs_lazy ctx =
  lazy
    (let pin_dir =
       Oi.Source.Pin.materialize ~fs:ctx.fs ~sys:ctx.sys ~cache:ctx.cache
         ~refresh:ctx.refresh ctx.project_pins
     in
     Stdlib.Option.to_list ctx.local_packages_dir
     @ Stdlib.Option.to_list pin_dir
     @ Oi.Source.Repo.ensure_many ~fs:ctx.fs ~data_dir:ctx.data_dir
         ~refresh:ctx.refresh ctx.all_extras
     @
     match ctx.toolchain with
     | Some (info : Oi.Toolchain.info) -> info.packages_dirs
     | None ->
         Oi.Source.Reporepo.ensure_base ~fs:ctx.fs ~sys:ctx.sys
           ~data_dir:ctx.data_dir ~refresh:ctx.refresh ())

let package_exists_in ~packages_dirs name =
  List.exists (fun dir -> Sys.file_exists (dir / name)) packages_dirs

(* Step 0a: solve [target] as a package name alongside [--with] deps.
   Skipped when [target] isn't a known package in any configured repo —
   that would just waste a solver pass and produce a confusing "no
   known implementations" diagnostic. *)
let try_solve_target_as_pkg ~packages_dirs ~target ~solve_with_extras =
  if not (package_exists_in ~packages_dirs target) then begin
    Log.info (fun m ->
        m "Skipping solve %s — not a package name in any configured repo" target);
    false
  end
  else
    try_step (Fmt.str "solve %s" target) (fun () ->
        solve_with_extras [ target ])

(* Explicit [@handle/pkg] target: never silently fall through to the
   layer-index lookup. The user named the source of truth; substituting
   a different package would be wrong. The error tells them which
   executables the package does install and how to run one. *)
let fail_overlay_pin_no_binary ~binary_name ~unfound_bins
    (pin : Target.handle_pin) =
  let qualified = "@" ^ pin.handle ^ "/" ^ OpamPackage.Name.to_string pin.pkg in
  let suggestion =
    match !unfound_bins with
    | [] ->
        " The package solved but installed no executables — check the \
         overlay's opam file."
    | bins ->
        Fmt.str
          " The package installs: %s.@,To run one of them: oi run --with=%s %s"
          (String.concat ", " bins) qualified (List.hd bins)
  in
  Oi.Error.fail_not_found binary_name "%s solved but does not install bin/%s.%s"
    qualified binary_name suggestion

(* Dash-split prefixes, longest-first: "a-b-c" → ["a-b-c"; "a-b"; "a"]. *)
let dash_prefixes name =
  String.split_on_char '-' name
  |> List.fold_left
       (fun acc part ->
         let prev = match acc with [] -> "" | p :: _ -> p ^ "-" in
         (prev ^ part) :: acc)
       []

(* Phase budget for the overall bar: [oi run]'s flow only fires
   [Build_pipeline.build]'s phases (no [Sync] / [install_tools] / dune
   subprocess), so ~8 phases on cold cache, fewer when the layer cache
   hit short-circuits the source path. *)
let with_preflight_bar ~clock f =
  Preflight_bar.Preflight.with_bar ~clock ~total_steps:10 f

(* Step 1: layer-index lookup. Solving for [target] verbatim didn't
   yield [bin/<target>] — either [target] isn't a package name, or the
   package that owns the binary is named differently (e.g.
   [ocluster-admin] is shipped by [ocluster]). Ask the index for the
   provider, then re-solve under the new name. *)
let lookup_layer_index ~ctx ~env ~binary_name =
  let clk = (Eio.Stdenv.clock env :> D10.Config.clk) in
  with_preflight_bar ~clock:ctx.clock
  @@ fun ~on_phase ~on_text:_ ~preflight_done ~shared_display:_ ->
  let r =
    Layer_index.package_of_binary ~on_phase ~sys:ctx.sys ~fs:ctx.fs ~clock:clk
      ~session:ctx.http_session ~cache:ctx.cache ~os_key:ctx.os_key
      ~registry:ctx.registry binary_name
  in
  preflight_done ();
  r

let try_solve_of_index ~binary_name ~solve_with_extras index_result =
  match index_result with
  | Some (pkg_name, _) when pkg_name <> binary_name ->
      Log.info (fun m ->
          m "Index: bin/%s provided by package %s" binary_name pkg_name);
      try_step (Fmt.str "solve %s" pkg_name) (fun () ->
          solve_with_extras [ pkg_name ])
  | _ -> false

(* Step 2: Try target name and dash-split prefixes. Skip any prefix
   already in [extra_names] (Step 0 solved that already) and skip any
   prefix that doesn't exist as a package in any configured repo — a
   missing package name cannot possibly provide the binary, and
   attempting to solve for it wastes a full solver run. *)
let try_solve_dash_prefixes ~packages_dirs ~target ~extra_names
    ~solve_with_extras =
  let prefixes =
    dash_prefixes target
    |> List.filter (fun p -> not (List.mem p extra_names))
    |> List.filter (package_exists_in ~packages_dirs)
  in
  if prefixes = [] then
    Oi.Error.fail_not_found target "no package provides bin/%s" target
  else begin
    Log.info (fun m -> m "Trying packages: %s" (String.concat ", " prefixes));
    let found =
      List.exists
        (fun name ->
          try_step (Fmt.str "solve %s (dash-prefix)" name) (fun () ->
              solve_with_extras [ name ]))
        prefixes
    in
    if not found then
      Oi.Error.fail_not_found target "no package provides bin/%s" target
  end

let binary ~ctx ~env ~pt ~unfound_bins ~args =
  let ocaml_name = OpamPackage.Name.of_string "ocaml" in
  let extra_names =
    List.filter_map
      (fun (d : Oi.Project.Script.dep) ->
        if OpamPackage.Name.equal d.name ocaml_name then None
        else Some (Oi.Project.Script.name_s d))
      ctx.extra_deps
    @ ctx.url_project.roots
  in
  let solve_with_extras pkg_names =
    solve_and_exec ~ctx ~binary_name:pt.binary_name ~target_pin:pt.target_pin
      ~unfound_bins ~args (pkg_names @ extra_names)
  in
  let packages_dirs = Lazy.force (packages_dirs_lazy ctx) in
  let from_target =
    try_solve_target_as_pkg ~packages_dirs ~target:pt.target ~solve_with_extras
  in
  let from_with =
    if from_target then from_target
    else
      try_step "fallback solve (extras + toolchain roots)" (fun () ->
          solve_and_exec ~ctx ~binary_name:pt.binary_name
            ~target_pin:pt.target_pin ~unfound_bins ~args extra_names)
  in
  (match pt.target_pin with
  | Some pin when not from_with ->
      fail_overlay_pin_no_binary ~binary_name:pt.binary_name ~unfound_bins pin
  | _ -> ());
  if from_with then ()
  else begin
    let index_result =
      lookup_layer_index ~ctx ~env ~binary_name:pt.binary_name
    in
    let from_index =
      try_solve_of_index ~binary_name:pt.binary_name ~solve_with_extras
        index_result
    in
    if not from_index then
      try_solve_dash_prefixes ~packages_dirs ~target:pt.target ~extra_names
        ~solve_with_extras
  end

(* -- top-level impl --------------------------------------------------- *)

let impl (c : Terms.common) (flags : flags) registry use_registry
    toolchain_override target with_deps with_repos jobs save_d10ir args =
  Harness.run @@ fun ~sw env ->
  let pt = parse_target ~target ~with_repos ~with_deps in
  let is_script = is_script_target pt.target in
  let h =
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  let cache_root = Oi.Cache.root_s h.cache in
  let dune_cache_root = Oi.Cache.dune_root h.cache in
  let refresh = flags.refresh && not flags.locked in
  let fast_key =
    fast_cache_key ~dry_run:flags.dry_run ~refresh ~os_key:h.os_key
      ~binary_name:pt.binary_name ~toolchain_override ~registry
      ~skip_local:flags.skip_local ~with_deps:pt.with_deps
      ~with_repos:pt.with_repos ~is_script
  in
  try_fast_cache_exec ~proc_mgr:h.proc_mgr ~cache_root ~dune_cache_root
    ~fast_key ~args;
  let ctx =
    build_ctx_from_harness h flags registry use_registry toolchain_override pt
      save_d10ir jobs ~fast_key ~data_dir:c.data_dir
  in
  let target = fetch_script_url ~fs:ctx.fs ~sys:ctx.sys pt.target in
  if Filename.check_suffix target ".ml" then script ~ctx ~target ~args
  else
    let unfound_bins = ref [] in
    binary ~ctx ~env ~pt:{ pt with target } ~unfound_bins ~args

(* -- cmdliner term + cmd --------------------------------------------- *)

let target =
  Arg.(
    required
    & pos 0 (some string) None
    & info ~docv:"TARGET"
        ~doc:
          "Binary name, $(b,@HANDLE/PKG) overlay shortcut, $(b,.ml) script \
           path, or $(b,https://) URL pointing at a $(b,.ml) script \
           ($(b,http://) also accepted)."
        [])

let dry_run =
  Arg.(
    value & flag & info ~doc:"Print the build plan and exit." [ "n"; "dry-run" ])

let args =
  Arg.(
    value & pos_right 0 string []
    & info ~docv:"ARG" ~doc:"Forwarded to $(b,TARGET)." [])

let save_d10ir =
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"DIR"
        ~doc:
          "Write the d10ir recipe for each solve group to \
           $(b,DIR/<root>.d10ir.json). Does not skip the build."
        [ "save-d10ir" ])

let man_targets =
  [
    `S Manpage.s_description;
    `P
      "Resolve $(b,TARGET)'s dependencies into the shared cache and exec \
       $(b,TARGET). Arguments after $(b,TARGET) are forwarded unchanged.";
    `P "$(b,TARGET) is one of:";
    `I
      ( "$(b,name)",
        "Binary name. Looked up in the layer index; dash-prefixes fall back \
         ($(b,ocluster-admin) tries $(b,ocluster))." );
    `I ("$(b,@HANDLE/PKG)", "Take $(b,PKG) from the named overlay.");
    `I ("$(b,path/to/script.ml)", "Local OCaml script.");
    `I
      ( "$(b,https://...ml)",
        "Remote OCaml script ($(b,http://) also accepted). Refetched each run; \
         cached by content hash." );
  ]

let man_binaries =
  [
    `S "BINARIES";
    `Pre
      "  oi run utop\n\
      \  oi run ocamlformat -- --help\n\
      \  oi run --with=crockford roguedoi";
  ]

let man_scripts =
  [
    `S "SCRIPTS";
    `P "Declare deps on the first line:";
    `Pre "  [@@@opam fmt cmdliner lwt>=5.0]";
    `P
      "Each token is an opam package with optional constraint ($(b,>=), \
       $(b,>), $(b,<=), $(b,<), $(b,=)) and optional findlib sub-library \
       ($(b,ppx_deriving.show)). $(b,ppx_*) packages are wired as \
       preprocessors.";
    `Pre
      "  oi run my_script.ml\n\
      \  oi run my_script.ml --with=tls -- arg1 arg2\n\
      \  oi run https://gist.example.com/hello.ml";
  ]

let man_overlays =
  [
    `S "OVERLAYS";
    `P
      "$(i,@HANDLE/PKG) on $(b,TARGET) or $(b,--with) takes $(b,PKG) from an \
       overlay. Bare $(i,@HANDLE) on $(b,--with-repo) stacks the whole \
       overlay. $(b,x-repos: [\"@HANDLE\"]) in a project's $(b,*.opam) applies \
       it automatically.";
    `Pre "  oi run @avsm/owntracks\n  oi run --with=@avsm/crockford roguedoi";
    `S "PROJECT EXTRAS";
    `P "Read from the cwd (or a $(b,--with=URL) clone):";
    `I
      ( "$(b,*.opam)",
        "$(b,depends:), $(b,pin-depends:), $(b,x-repos:) merge into the solve."
      );
    `I
      ( "$(b,packages/) + $(b,repo)",
        "Highest-priority opam-repository overlay for patched transitive deps."
      );
  ]

let man_toolchain =
  [
    `S "TOOLCHAIN";
    `P "Picked in order:";
    `I ("1.", "$(b,--toolchain=NAME).");
    `I
      ("2.", "$(b,x-oi-toolchain) on an in-scope $(b,@HANDLE). Conflicts error.");
    `I ("3.", "Reporepo's $(b,x-oi-default-toolchain).");
  ]

let man_urls =
  [
    `S "GIT URLS";
    `P
      "$(b,--with=URL) clones the repo and pins every root $(b,*.opam). \
       Schemes: $(b,http://), $(b,https://), $(b,git+), $(b,git@), \
       $(b,git://), $(b,ssh://). Append $(b,#REF) for a tag, branch, or \
       commit.";
    `Pre
      "  oi run --with=https://github.com/owner/project.git target\n\
      \  oi run --with=git+https://example.org/foo.git#branch foo";
    `S "VERSION CONSTRAINTS";
    `P "$(b,pkg.VERSION) or $(b,pkg=VERSION) on $(b,--with).";
    `Pre
      "  oi run --with=dune.3.20.0 -- dune --version\n\
      \  oi run --with=fmt>=0.9 my_script.ml";
    `S "DRY RUN";
    `P "$(b,-n) prints the plan and exits. Per-package tags:";
    `I ("$(b,binary)", "Cached locally.");
    `I ("$(b,remote)", "Fetchable from the registry.");
    `I ("$(b,source)", "Compiles from source.");
    `I ("$(b,virtual)", "No-op placeholder ($(b,conf-pkg-config) etc.).");
  ]

let info_run =
  Cmd.info "run" ~doc:"Run an opam-packaged binary or OCaml script"
    ~man:
      (man_targets @ man_binaries @ man_scripts @ man_overlays @ man_toolchain
     @ man_urls)

let info_oix =
  Cmd.info "oix" ~doc:"Run a binary from opam overlays"
    ~man:
      [
        `S Manpage.s_description;
        `P
          "Resolve $(b,TARGET), build its dependencies into the shared cache, \
           and exec it. Arguments after $(b,TARGET) are forwarded unchanged. \
           Equivalent to $(b,oi run --skip-local).";
        `Pre
          "  oix utop\n\
          \  oix patdiff a.txt b.txt\n\
          \  oix ocamlformat --check .\n\
          \  oix hello.ml\n\
          \  oix --with=ocamlformat.0.29.0 ocamlformat --check .\n\
          \  oix --with=tls --with=cohttp my-client\n\
          \  oix --toolchain=oxcaml utop\n\
          \  oix @avsm/owntracks\n\
          \  oix -n utop";
        `S "EXTRA DEPENDENCIES";
        `P
          "$(b,--with) accepts a bare name, opam atom ($(b,fmt>=0.9), \
           $(b,dune.3.20.0)), or git URL (every root $(b,*.opam) becomes a \
           pin). Repeatable.";
        `Pre
          "  oix --with=tls --with=cohttp my_client\n\
          \  oix --with=dune.3.20.0 dune --version\n\
          \  oix --with=https://github.com/owner/proj.git my-tool";
        `S "OVERLAYS";
        `P
          "$(b,@HANDLE/PKG) on $(b,TARGET) or $(b,--with) takes one package \
           from an overlay. $(b,--with-repo=@HANDLE) stacks the whole overlay.";
        `Pre "  oix @avsm/owntracks\n  oix --with-repo=@avsm crockford";
        `S "TOOLCHAIN";
        `P
          "$(b,--toolchain=NAME) pins the compiler. Otherwise an overlay's \
           $(b,x-oi-toolchain) wins, falling back to the reporepo default.";
        `S "SCRIPT FORMAT";
        `P
          "$(b,TARGET) may be a $(b,.ml) script path or an $(b,https://) URL \
           ($(b,http://) also accepted). Declare deps on the first line:";
        `Pre "  [@@@opam fmt cmdliner>=1.2.0 lwt]";
        `P
          "Each token is an opam package with optional constraint ($(b,>=), \
           $(b,>), $(b,<=), $(b,<), $(b,=)) and optional findlib sub-library \
           ($(b,ppx_deriving.show)). $(b,ppx_*) packages are wired as \
           preprocessors.";
        `S Manpage.s_see_also;
        `P "$(b,oi)(1).";
      ]

let flags_term ~skip_local =
  let mk refresh locked skip_local dry_run =
    { refresh; locked; skip_local; dry_run }
  in
  Term.(const mk $ Terms.refresh $ Terms.locked $ skip_local $ dry_run)

let term ~skip_local =
  Term.(
    const impl $ Terms.common $ flags_term ~skip_local $ Terms.registry
    $ Terms.use_registry $ Terms.toolchain $ target $ Terms.with_deps
    $ Terms.with_repos $ Terms.jobs $ save_d10ir $ args)

let cmd = Cmd.v info_run (term ~skip_local:Terms.skip_local)
let cmd_x = Cmd.v info_oix (term ~skip_local:(Term.const true))
