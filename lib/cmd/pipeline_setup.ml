type prepared = {
  env : Oi.Build_pipeline.env;
  request : Oi.Build_pipeline.request;
  layer_remote : D10.Layer.remote option;
  source_remote : D10.Layer.remote option;
  toolchain : Oi.Toolchain.info option;
  cwd : string;
}

(* Pull every [@handle] reference out of a parsed target list — used as a
   seed for toolchain detection. [Group { handles; _ }] carries an explicit
   handle list (the call site that builds [Group] already split it out);
   [Overlay_pkg]/[Overlay_all] are sugar for a one-handle Group. *)
let target_handles (targets : Oi.Build_pipeline.target list) : string list =
  List.concat_map
    (fun (t : Oi.Build_pipeline.target) ->
      match t with
      | Plain _ -> []
      | Group { handles; _ } -> handles
      | Overlay_pkg { handle; _ } -> [ handle ]
      | Overlay_all h -> [ h ])
    targets

(* Project metadata (if any [*.opam] in cwd) — pins / overlays /
   extras feed the solve. Sys / IO errors degrade to "no project"
   rather than aborting (a missing cwd, an unreadable dir): the
   command might still have a valid TARGET passed in. *)
type project_bits = {
  extras : Oi.Project.extra_repo list;
  pins : Oi.Project.pin list;
  overlays : string list;
  packages_dir : string option;
}

let empty_project_bits =
  { extras = []; pins = []; overlays = []; packages_dir = None }

let load_project_bits ~fs ~skip_local cwd_s : project_bits =
  if skip_local then empty_project_bits
  else
    match Oi.Project.load ~fs cwd_s with
    | exception Sys_error _ -> empty_project_bits
    | exception Eio.Exn.Io _ -> empty_project_bits
    | p ->
        {
          extras = p.extra_repos;
          pins = p.pins;
          overlays = p.overlays;
          packages_dir = p.packages_dir;
        }

let merge_project_bits ~(project : project_bits)
    ~(url_project : Oi.Project.Url.t) : project_bits =
  {
    extras = project.extras @ url_project.extra_repos;
    pins = project.pins @ url_project.pins;
    overlays = project.overlays @ url_project.overlays;
    packages_dir =
      (match project.packages_dir with
      | Some _ -> project.packages_dir
      | None -> url_project.packages_dir);
  }

let toolchain_handles ~extra_handles ~targets ~with_repos ~overlays =
  extra_handles @ target_handles targets
  @ Target.handles_of_tokens with_repos
  @ overlays
  |> List.sort_uniq String.compare

let resolve_toolchain ~fs ~sys ~data_dir ~conf ~toolchain_override
    ~extra_handles ~targets ~with_repos ~overlays =
  let tc_handles =
    toolchain_handles ~extra_handles ~targets ~with_repos ~overlays
  in
  Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf ~install:true
    ~override:toolchain_override ~handles:tc_handles ()

let merge_constraints extra with_deps =
  OpamPackage.Name.Map.union (fun a _ -> a) extra with_deps

let build_env_of (harness : Harness.env) : Oi.Build_pipeline.env =
  {
    proc_mgr = harness.proc_mgr;
    fs = harness.fs;
    clock = harness.clock;
    sys = harness.sys;
    os_key = harness.os_key;
    cache = harness.cache;
    data_dir = harness.data_dir;
    http_session = harness.http_session;
  }

(* Build the final request record after the resolved toolchain has been
   used to filter overlays and the constraint maps have been merged. *)
let build_request ~targets ~with_repos ~pins ~extra_repos ~constraints
    ~toolchain_override ~toolchain ~conf ~local_packages_dir ~refresh :
    Oi.Build_pipeline.request =
  {
    targets;
    with_repos;
    pins;
    extra_repos;
    constraints;
    toolchain_override;
    toolchain;
    conf;
    local_packages_dir;
    project_root = None;
    force_source = false;
    with_test = false;
    refresh;
  }

(* When the toolchain was auto-picked, drop project overlays tagged
   for a different one. When the user passed [--toolchain=NAME]
   ([toolchain_override = Some _]), keep every declared overlay
   verbatim — they explicitly overrode the project's preference, so
   we shouldn't second-guess by silently filtering. *)
let filter_overlays ~toolchain_override ~toolchain overlays =
  Oi.Pipeline.filter_compatible_overlays
    ~reporepo_path:(Terms.reporepo_path ()) ~override:toolchain_override
    ~toolchain overlays

let prepare ~(harness : Harness.env) ~refresh ~locked ~skip_local ~registry
    ~use_registry ~with_repos ~with_deps ~toolchain_override ~targets
    ?(extra_handles = []) ?(extra_pins = [])
    ?(extra_constraints = OpamPackage.Name.Map.empty) () : prepared =
  let { Harness.fs; sys; platform; cache; data_dir; _ } = harness in
  let refresh = refresh && not locked in
  let use_registry = if locked then Oi.Use_registry.Never else use_registry in
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
  let conf =
    Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  let { Terms.layer_remote; source_remote } =
    Terms.remotes_of ~url:registry ~mode:use_registry
  in
  (* URL projects (e.g. [--with=https://github.com/foo/bar#tag]) are
     cloned into the pin cache and contribute pins / solver roots /
     overlays / extra_repos. *)
  let extra_deps_loaded, url_project =
    Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh with_deps
  in
  let with_deps_constraints = Oi.Project.Script.constraints extra_deps_loaded in
  let cwd_s, _ = Workspace.resolved_cwd fs in
  let project =
    merge_project_bits
      ~project:(load_project_bits ~fs ~skip_local cwd_s)
      ~url_project
  in
  let toolchain =
    resolve_toolchain ~fs ~sys ~data_dir ~conf ~toolchain_override
      ~extra_handles ~targets ~with_repos ~overlays:project.overlays
  in
  let with_repos =
    filter_overlays ~toolchain_override ~toolchain project.overlays @ with_repos
  in
  let extra_repos =
    Target.merge_extras
      ~cli:(Target.cli_extra_repos ~fs ~sys ?toolchain with_repos)
      ~project:project.extras
  in
  let request =
    build_request ~targets ~with_repos ~pins:(project.pins @ extra_pins)
      ~extra_repos
      ~constraints:(merge_constraints extra_constraints with_deps_constraints)
      ~toolchain_override ~toolchain ~conf
      ~local_packages_dir:project.packages_dir ~refresh
  in
  {
    env = build_env_of harness;
    request;
    layer_remote;
    source_remote;
    toolchain;
    cwd = cwd_s;
  }
