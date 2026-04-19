[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

(** Construct a synthetic OpamSwitchState.t for building packages without using
    ~/.opam. *)

let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.opam_ctx"

module Log = (val Logs.src_log log_src : Logs.LOG)

type conf = {
  arch : string;
  os : string;
  os_distribution : string;
  os_version : string;
  os_family : string;
  ocaml_version : string;
  jobs : int;
}

let pp_conf fmt c =
  Fmt.pf fmt
    "@[<v>arch:            %s@,\
     os:              %s@,\
     os-distribution: %s@,\
     os-version:      %s@,\
     os-family:       %s@,\
     ocaml-version:   %s@,\
     jobs:            %d@]"
    c.arch c.os c.os_distribution c.os_version c.os_family c.ocaml_version
    c.jobs

type t = {
  mutable st : OpamStateTypes.rw OpamStateTypes.switch_state;
  conf : conf;
  prefix : string;
}

(* Scan a packages directory and load all opam files into a map *)
let load_opams_from_dir dir =
  let opams = ref OpamPackage.Map.empty in
  if Sys.file_exists dir then
    Array.iter
      (fun name_s ->
        let name_dir = dir / name_s in
        if Sys.is_directory name_dir then
          Array.iter
            (fun pkg_s ->
              let pkg_dir = name_dir / pkg_s in
              let opam_path = pkg_dir / "opam" in
              if Sys.file_exists opam_path then
                try
                  let opam =
                    OpamFile.OPAM.read
                      (OpamFile.make (OpamFilename.raw opam_path))
                  in
                  let pkg = OpamPackage.of_string pkg_s in
                  opams := OpamPackage.Map.add pkg opam !opams
                with _ ->
                  Log.debug (fun m -> m "Could not parse %s" opam_path))
            (Sys.readdir name_dir))
      (Sys.readdir dir);
  !opams

let _global_state :
    OpamStateTypes.unlocked OpamStateTypes.global_state option ref =
  ref None

let init_opam ~root =
  Unix.putenv "OPAMROOT" root;
  OpamSystem.init ();
  OpamCoreConfig.init ~yes:(Some true) ~verbose_level:0 ~debug_level:0
    ~disp_status_line:`Never ();
  OpamFormatConfig.init ();
  OpamClientConfig.init ();
  let root_dir = OpamFilename.Dir.of_string root in
  OpamStateConfig.update ~no_depexts:true ~root_dir ();
  (* Write a minimal config file so OpamGlobalState.load works.
     This is the equivalent of 'opam init --bare' with no remotes. *)
  OpamFilename.mkdir root_dir;
  let config_path = OpamPath.config root_dir in
  if not (OpamFile.exists config_path) then begin
    let config =
      OpamFile.Config.empty
      |> OpamFile.Config.with_opam_version (OpamVersion.of_string "2.0")
      |> OpamFile.Config.with_opam_root_version OpamFile.Config.root_version
    in
    OpamFile.Config.write config_path config
  end;
  (* Load the global state — this auto-detects platform variables
     (os, arch, os-distribution, os-version, os-family) via
     OpamSysPoll.variables instead of hardcoding them. *)
  let gt = OpamGlobalState.load `Lock_none in
  _global_state := Some (OpamGlobalState.unlock gt)

let conf t = t.conf
let prefix t = t.prefix

(* TODO: These env vars are hardcoded because neither the relocatable
   compiler nor standard OCaml 5.x packages declare them via setenv:
   in their opam files. If the relocatable repo adds setenv: entries
   to relocatable-compiler.opam, this function can be replaced by
   calling OpamEnv.compute_updates on the switch state after
   mark_installed — opam will derive the env from the installed
   packages' setenv: fields automatically. *)
let switch_env ~prefix =
  [
    ("OPAM_SWITCH_PREFIX", prefix);
    ("OCAMLLIB", prefix / "lib" / "ocaml");
    ( "CAML_LD_LIBRARY_PATH",
      String.concat ":"
        [ prefix / "lib" / "stublibs"; prefix / "lib" / "ocaml" / "stublibs" ]
    );
    ("OCAMLFIND_CONF", prefix / "lib" / "findlib.conf");
    (* The relocatable ocamlfind installs findlib.conf with destdir=".",
       expecting OCAMLFIND_DESTDIR to be set at use-time. Without this,
       "ocamlfind install" (e.g. zarith's `make install`) writes files
       relative to the build cwd rather than into the prefix, and the
       layer-diff captures nothing. *)
    ("OCAMLFIND_DESTDIR", prefix / "lib");
    ("OCAMLPATH", String.concat ":" [ prefix / "lib"; prefix / "lib" / "ocaml" ]);
    ("OCAMLTOP_INCLUDE_PATH", prefix / "lib" / "toplevel");
    ("OCAML_TOPLEVEL_PATH", prefix / "lib" / "toplevel");
  ]

let create ~prefix ~packages_dirs ~conf =
  let loaded_gt =
    match !_global_state with
    | Some gt -> gt
    | None -> Fmt.failwith "Opam_ctx.init_opam must be called before create"
  in

  (* Canonicalize the prefix to resolve symlinks (e.g. /var -> /private/var
     on macOS) so that paths match what the relocatable compiler reports
     via ocamlc -where. *)
  let prefix = try Unix.realpath prefix with Unix.Unix_error _ -> prefix in
  (* Set opam root so that root/switch_name = prefix.
     This way OpamPath.Switch.root returns the build prefix. *)
  let switch_parent = Filename.dirname prefix in
  let switch_name = Filename.basename prefix in
  let root = OpamFilename.Dir.of_string switch_parent in
  let switch = OpamSwitch.of_string switch_name in

  OpamFilename.mkdir root;
  OpamFilename.mkdir (OpamFilename.Dir.of_string prefix);
  let meta_dir = OpamFilename.Dir.of_string (prefix / ".opam-switch") in
  OpamFilename.mkdir meta_dir;
  List.iter
    (fun sub ->
      OpamFilename.mkdir
        (OpamFilename.Dir.of_string (prefix / ".opam-switch" / sub)))
    [ "build"; "install"; "config"; "sources"; "packages" ];

  let config =
    OpamFile.Config.empty
    |> OpamFile.Config.with_opam_version (OpamVersion.of_string "2.0")
    |> OpamFile.Config.with_opam_root_version OpamFile.Config.root_version
    |> OpamFile.Config.with_installed_switches [ switch ]
  in
  let switch_config =
    let sc = OpamFile.Switch_config.empty in
    {
      sc with
      env =
        List.map
          (fun (k, v) ->
            OpamTypesBase.env_update_resolved k Eq v ~comment:"switch env")
          (switch_env ~prefix);
    }
  in

  OpamStateConfig.update ~root_dir:root ();

  let all_opams =
    List.fold_left
      (fun acc dir ->
        OpamPackage.Map.union (fun a _ -> a) acc (load_opams_from_dir dir))
      OpamPackage.Map.empty packages_dirs
  in
  let all_packages = OpamPackage.keys all_opams in

  (* Reuse the global state from init_opam — it has platform variables
     auto-detected via OpamSysPoll (os, arch, etc.) — but override the
     root and config to point at our build prefix. Add oi-specific
     variables (sys-ocaml-version, jobs, etc.) that OpamSysPoll doesn't
     provide. *)
  let global_variables =
    let open OpamTypes in
    let var s value desc =
      (OpamVariable.of_string s, (lazy (Some value), desc))
    in
    List.fold_left
      (fun m (k, v) -> OpamVariable.Map.add k v m)
      loaded_gt.global_variables
      [
        var "sys-ocaml-version" (S conf.ocaml_version) "OCaml version";
        var "sys-ocaml-arch" (S conf.arch) "OCaml arch";
        var "sys-ocaml-cc" (S "cc") "OCaml C compiler";
        var "sys-ocaml-libc" (S "libc") "OCaml C library";
        var "jobs" (S (string_of_int conf.jobs)) "Parallel jobs";
        var "make" (S "make") "Make command";
        var "prefix" (S prefix) "Switch prefix";
        (* Override the auto-detected platform variables from OpamSysPoll
           with values from [conf]. This lets [--os=linux] (etc.) on a
           macOS host actually influence depext filter evaluation and
           dependency formula filtering. *)
        var "os" (S conf.os) "Operating system";
        var "os-distribution" (S conf.os_distribution) "OS distribution";
        var "os-family" (S conf.os_family) "OS family";
        var "os-version" (S conf.os_version) "OS version";
        var "arch" (S conf.arch) "Architecture";
      ]
  in
  let gt : OpamStateTypes.unlocked OpamStateTypes.global_state =
    { loaded_gt with root; config; global_variables }
  in

  (* Minimal repos state — packages come from our repos dirs, not opam's
     repo tracking *)
  let rt : OpamStateTypes.unlocked OpamStateTypes.repos_state =
    {
      repos_lock = OpamSystem.lock_none;
      repos_global = gt;
      repositories = OpamRepositoryName.Map.empty;
      repos_definitions = OpamRepositoryName.Map.empty;
      repo_opams = OpamRepositoryName.Map.empty;
      repos_tmp = Hashtbl.create 0;
    }
  in

  let st : OpamStateTypes.rw OpamStateTypes.switch_state =
    {
      switch_lock = OpamSystem.lock_none;
      switch_global = gt;
      switch_repos = rt;
      switch;
      switch_invariant = OpamFormula.Empty;
      compiler_packages = OpamPackage.Set.empty;
      switch_config;
      repos_package_index = all_opams;
      opams = all_opams;
      conf_files = OpamPackage.Name.Map.empty;
      packages = all_packages;
      sys_packages = lazy OpamPackage.Map.empty;
      available_packages =
        lazy
          (OpamSwitchState.compute_available_packages gt switch switch_config
             ~pinned:OpamPackage.Set.empty ~opams:all_opams);
      pinned = OpamPackage.Set.empty;
      installed = OpamPackage.Set.empty;
      installed_opams = OpamPackage.Map.empty;
      installed_roots = OpamPackage.Set.empty;
      reinstall = lazy OpamPackage.Set.empty;
      invalidated = lazy OpamPackage.Set.empty;
      overwrote_opams = OpamPackage.Map.empty;
    }
  in
  { st; conf; prefix }

let switch_state t =
  (* Coerce rw to unlocked for the solver *)
  (Obj.magic t.st : OpamStateTypes.unlocked OpamStateTypes.switch_state)

let resolve t opam = OpamPackageVar.resolve ~opam t.st

let platform_env t v =
  (* Resolve from global_variables only, so that dep-type variables
     (build, post, with-test, etc.) remain unresolved — they must be
     left for OpamFilter.filter_deps to handle. Using the full
     OpamPackageVar.resolve would prematurely evaluate {build}
     conditions and break dep_names filtering. *)
  let s = OpamVariable.Full.to_string v in
  match
    OpamVariable.Map.find_opt (OpamVariable.of_string s)
      t.st.switch_global.global_variables
  with
  | Some ((lazy value), _) -> value
  | None -> None

let compilation_env t opam =
  let build_env =
    List.map
      (fun env ->
        OpamEnv.resolve_separator_and_format
          (OpamEnv.env_expansion ~opam t.st env))
      (OpamFile.OPAM.build_env opam)
  in
  let updates =
    [
      OpamTypesBase.env_update_resolved "CDPATH" Eq "" ~comment:"sanitize";
      OpamTypesBase.env_update_resolved "MAKEFLAGS" Eq "" ~comment:"sanitize";
      OpamTypesBase.env_update_resolved "MAKELEVEL" Eq "" ~comment:"sanitize";
      (* OCaml reads OCAML_TOPLEVEL_PATH (underscored) to locate topfind;
         if a host opam switch is active it points at that switch's toplevel
         dir, so [ocaml pkg/pkg.ml] (topkg) loads a findlib_top.cma built
         against a different OCaml and errors with "disagree over interface
         Misc". Pin it to our prefix. *)
      OpamTypesBase.env_update_resolved "OCAML_TOPLEVEL_PATH" Eq
        (t.prefix / "lib" / "toplevel")
        ~comment:"sanitize";
      OpamTypesBase.env_update_resolved "OPAM_PACKAGE_NAME" Eq
        (OpamPackage.Name.to_string (OpamFile.OPAM.name opam))
        ~comment:"build env";
      OpamTypesBase.env_update_resolved "OPAM_PACKAGE_VERSION" Eq
        (OpamPackage.Version.to_string (OpamFile.OPAM.version opam))
        ~comment:"build env";
      OpamTypesBase.env_update_resolved "OPAMCLI" Eq "2.0"
        ~comment:"opam CLI version";
    ]
    @ build_env
  in
  OpamEnv.get_full ~set_opamroot:true ~set_opamswitch:true ~force_path:true t.st
    ~updates
  |> OpamTypesBase.env_array

let resolve_substs t opam =
  let env = OpamPackageVar.resolve ~opam t.st in
  let vars = Hashtbl.create 32 in
  let record_var v =
    let full = OpamVariable.Full.of_string v in
    let value =
      match env full with
      | Some (OpamTypes.S s) -> s
      | Some (OpamTypes.B b) -> string_of_bool b
      | Some (OpamTypes.L l) -> String.concat " " l
      | None -> ""
    in
    Hashtbl.replace vars v value;
    value
  in
  (* Resolve common variables that packages typically reference in .in files *)
  let _subst_basenames = OpamFile.OPAM.substs opam in
  List.iter
    (fun v -> ignore (record_var v))
    [
      "name";
      "version";
      "prefix";
      "lib";
      "bin";
      "share";
      "doc";
      "etc";
      "man";
      "sbin";
      "make";
      "jobs";
      "os";
      "arch";
      "ocaml:version";
      "ocaml:native";
      "ocaml:native-dynlink";
      "opam-version";
    ];
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) vars []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let resolve_commands t ~test ~doc ~dev_setup opam =
  let local =
    OpamVariable.Map.of_list
      [
        (OpamVariable.of_string "with-test", Some (OpamTypes.B test));
        (OpamVariable.of_string "with-doc", Some (OpamTypes.B doc));
        (OpamVariable.of_string "with-dev-setup", Some (OpamTypes.B dev_setup));
      ]
  in
  let env = OpamPackageVar.resolve ~opam ~local t.st in
  OpamFilter.commands env (OpamFile.OPAM.build opam)
  |> List.filter_map (function [] -> None | cmd -> Some cmd)

let mark_installed t pkg opam config =
  let name = OpamPackage.name pkg in
  t.st <-
    {
      t.st with
      installed = OpamPackage.Set.add pkg t.st.installed;
      installed_opams = OpamPackage.Map.add pkg opam t.st.installed_opams;
      conf_files =
        (match config with
        | Some c -> OpamPackage.Name.Map.add name c t.st.conf_files
        | None -> t.st.conf_files);
    }

let synthetic_config _t pkg _opam =
  (* For well-known compiler packages, hard-code the .config that the
     package would produce when built. This lets variables like
     [ocaml:native] resolve at plan time, before any builds happen. *)
  let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
  match name with
  | "ocaml" ->
      let var s = OpamVariable.of_string s in
      let s v = OpamTypes.S v in
      let b v = OpamTypes.B v in
      Some
        (OpamFile.Dot_config.create
           [
             (var "native", b true);
             (var "native-tools", b true);
             (var "native-dynlink", b true);
             (var "preinstalled", b false);
             (var "compiler", s "");
           ])
  | _ -> None
