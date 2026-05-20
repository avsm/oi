[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.aux_install"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ready_marker install_prefix = install_prefix / ".oi-toolchain-ready"

let ready install_prefix =
  Sys.file_exists (install_prefix / "bin" / "ocamlc")
  && Sys.file_exists (ready_marker install_prefix)

let mark_ready fs install_prefix =
  try
    Eio.Path.save ~create:(`Or_truncate 0o644)
      Eio.Path.(fs / ready_marker install_prefix)
      ""
  with Eio.Exn.Io _ -> ()

let host_conf ~(env : Build_pipeline.env) ~ocaml_version =
  let platform = Osrel.detect ~proc_mgr:env.proc_mgr ~fs:env.fs in
  Pipeline.conf ~platform ~ocaml_version

(* Build the [request] for the toolchain solve. The toolchain roots are
   the user-facing [Plain] targets; the resolved [info_for_install]
   carries [preinstalled_override = Some false] so [ocamlfind+ox]'s
   configure does NOT pass [-no-topfind] (the toolchain prefix is
   user-writable, so [topfind] gets installed there), AND empty
   [packages]/[root_names] so the inner solver does not virtually-install
   anything — every toolchain root must actually build. *)
let request_of ~info_for_install ~conf root_names =
  (* All toolchain roots must solve as ONE group so the joint version
     constraints propagate. With each as its own [Plain] target, the
     [ocaml] root alone is unconstrained → solver picks [ocaml.5.5.0],
     conflicting with [ocaml-variants.5.2.0+ox]'s pin. With them in a
     single [Group], the solver sees them all at once and picks the
     compatible [+ox] versions. *)
  let tokens = List.map OpamPackage.Name.to_string root_names in
  let targets =
    [
      Build_pipeline.Group
        { tokens; handles = info_for_install.Toolchain.dep_handles };
    ]
  in
  {
    Build_pipeline.targets;
    with_repos = info_for_install.Toolchain.dep_handles;
    pins = [];
    extra_repos = [];
    constraints = OpamPackage.Name.Map.empty;
    toolchain_override = Some info_for_install.Toolchain.handle;
    toolchain = Some info_for_install;
    conf;
    local_packages_dir = None;
    project_root = None;
    force_source = false;
    with_test = false;
    refresh = false;
  }

let check_build_outcome ~handle = function
  | None ->
      Log.info (fun m ->
          m
            "toolchain %s: all toolchain layers already cached, no source \
             build needed"
            handle)
  | Some (r : D10ir.Direct.result) when r.failed = 0 && r.skipped = 0 -> ()
  | Some (r : D10ir.Direct.result) when r.failures <> [] ->
      let f = List.hd r.failures in
      Error.fail_config_error
        "toolchain %s: install failed at %s.%s @ %s — see %s" handle
        f.package.name f.package.version
        (D10ir.Direct.string_of_phase f.phase)
        f.log_path
  | Some (r : D10ir.Direct.result) ->
      Error.fail_config_error
        "toolchain %s: install incomplete (%d failed, %d skipped); re-run with \
         --verbosity=debug for the per-node trace"
        handle r.failed r.skipped

(* The sub-build solves and constructs every root fresh, so we strip
   [packages] / [root_names] from the toolchain context — otherwise
   {!Solver.Ctx.create} would virtually-install them and the solver
   would treat them as already satisfied. The compiler's identity
   (handle / hash / install_prefix) still anchors layer caching.
   [preinstalled_override = Some false] tells [ocamlfind+ox]'s
   configure that the prefix is user-writable, so it installs
   [topfind] without the [-no-topfind] gate. *)
let info_for_install_of (info : Toolchain.info) : Toolchain.info =
  {
    info with
    preinstalled_override = Some false;
    packages = OpamPackage.Set.empty;
    root_names = OpamPackage.Name.Set.empty;
  }

(* [layer_remote = None] is deliberate: for non-relocatable compiler
   packages we never pull prebuilt binary layers from the remote
   registry. The compiler's [bin/ocamlc] bakes [--prefix=<staging>/
   lib/ocaml] into its configure-generated constants, so a registry
   layer baked on CI at [/cache/build/staging/<hash>/…] would stay
   pointing at a path that doesn't exist locally. Source archives
   via [source_remote] are fine — they're just tarballs of the
   source tree, no machine-specific paths to relocate. *)
let run_build ~env ~reporter ~source_remote ~install_prefix solved =
  Build_pipeline.build env ~reporter
    {
      solved;
      layer_remote = None;
      source_remote;
      jobs = None;
      upload_archive_url = None;
      archive_sources = false;
      snapshot_reporepo = false;
      install_to = Some install_prefix;
    }

let verify_installed ~handle ~install_prefix =
  if not (Sys.file_exists (install_prefix / "bin" / "ocamlc")) then
    Error.fail_config_error
      "toolchain %s: install completed but %s/bin/ocamlc is missing" handle
      install_prefix

let ensure ~(env : Build_pipeline.env) ?(reporter = Build_progress.null)
    ?(source_remote = None) (info : Toolchain.info) =
  if info.relocatable then ()
  else if ready info.install_prefix then
    Log.debug (fun m ->
        m "toolchain %s: install_prefix %s already populated" info.handle
          info.install_prefix)
  else
    let roots = OpamPackage.Name.Set.elements info.root_names in
    if roots = [] then
      Error.fail_config_error
        "toolchain %s: no roots declared in x-oi-toolchain-roots" info.handle;
    Say.step "Building toolchain %s into %s (%s)" info.handle
      info.install_prefix
      (String.concat ", " (List.map OpamPackage.Name.to_string roots));
    let conf = host_conf ~env ~ocaml_version:info.ocaml_version in
    let info_for_install = info_for_install_of info in
    let req = request_of ~info_for_install ~conf roots in
    let solved = Build_pipeline.solve env ~reporter req in
    let outcome =
      run_build ~env ~reporter ~source_remote
        ~install_prefix:info.install_prefix solved
    in
    check_build_outcome ~handle:info.handle outcome;
    verify_installed ~handle:info.handle ~install_prefix:info.install_prefix;
    mark_ready env.fs info.install_prefix;
    Say.ok "Toolchain %s installed at %s" info.handle info.install_prefix;
    Log.info (fun m ->
        m "toolchain %s installed at %s" info.handle info.install_prefix)
