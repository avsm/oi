open Cmdliner

let ( / ) = Filename.concat

let constr_to_op_ver (op, ver) =
  (OpamFormula.string_of_relop op, OpamPackage.Version.to_string ver)

let render_constraint = function
  | None -> ""
  | Some (op, ver) -> Fmt.str " %s %s" op ver

(* Pre-flight checks that must hold before we run any sync. Keeps us from
   spending 10 seconds on a repo refresh when [dune-project] won't accept
   the edit. *)
let validate_project ~fs ~cwd ~package =
  let dp = Oi.Project.Dune.load ~fs ~cwd in
  if not (Oi.Project.Dune.generate_opam_files dp) then
    Oi.Error.fail_config_error
      "dune-project does not have (generate_opam_files): oi add only supports \
       projects where dune owns the *.opam files";
  match (package, Oi.Project.Dune.package_names dp) with
  | Some p, names when not (List.mem p names) ->
      Oi.Error.fail_config_error
        "no (package (name %s) …) stanza in dune-project (declared: %s)" p
        (if names = [] then "none" else String.concat ", " names)
  | Some _, _ | _, [ _ ] | _, [] -> ()
  | None, many ->
      Oi.Error.fail_config_error
        "multiple packages in dune-project (%s); re-run with -p PKG to pick one"
        (String.concat ", " many)

(* Phase 2: edit and save dune-project. Reload in case something touched it
   during the sync (shouldn't, but cheap to be defensive). *)
let edit_dune_project ~fs ~cwd ~package ~dep_name ~op_ver =
  let dp = Oi.Project.Dune.load ~fs ~cwd in
  let dp' =
    Oi.Project.Dune.add_dependency dp ?package ~name:dep_name
      ~constraint_:op_ver ()
  in
  Oi.Project.Dune.save ~fs dp';
  Fmt.pr "Updated dune-project: added %s%s@." dep_name
    (render_constraint op_ver)

(* Phase 3: regenerate *.opam via dune build inside the assembled prefix.
   Dune itself comes from [_oi/prefix/bin]. *)
let regenerate_opam_files ~proc_mgr ~fs ~cache ~cwd =
  let prefix = cwd / "_oi" / "prefix" in
  let tools = Workspace.tools_dir_for ~cwd in
  let env =
    Oi.Solver.Env.make_env ~prefix ?tools
      ~dune_cache_root:(Oi.Cache.dune_root cache) ()
  in
  Fmt.pr "Running dune build to regenerate *.opam...@.";
  Eio.Switch.run @@ fun sw ->
  let child =
    Eio.Process.spawn ~sw proc_mgr ~env
      ~cwd:Eio.Path.(fs / cwd)
      [ prefix / "bin" / "dune"; "build" ]
  in
  match Eio.Process.await child with
  | `Exited 0 -> ()
  | `Exited n ->
      Oi.Error.fail_msg
        "dune build exited with code %d; dune-project was updated but *.opam \
         regeneration failed"
        n
  | `Signaled n -> Oi.Error.fail_msg "dune build killed by signal %d" n

(* Eio resources passed into [run_add]. Grouped into a record so the
   four-phase orchestration doesn't thread eight labelled args. *)
type env = {
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  sys : D10.Sysops.t;
  platform : Osrel.t;
  os_key : string;
  cache : Oi.Cache.t;
  http_session : D10.Sysops.Http.session;
}

(* Orchestrate the four phases: validate, solve, edit, regen, re-sync.
   Sync runs first so a failed solve leaves project files untouched. *)
let run_add ~env:e ~data_dir ~cwd ~refresh ~registry ~use_registry ~with_repos
    ~toolchain ~package ~pkg_spec =
  validate_project ~fs:e.fs ~cwd ~package;
  let dep = Oi.Project.Script.parse_cli_dep pkg_spec in
  let dep_name = OpamPackage.Name.to_string dep.name in
  let op_ver = Stdlib.Option.map constr_to_op_ver dep.constraint_ in
  Fmt.pr "Solving %s%s into the project...@." dep_name
    (render_constraint op_ver);
  ignore
    (Sync.run ~refresh ~with_repos ~with_deps:[ pkg_spec ] ?toolchain
       ~proc_mgr:e.proc_mgr ~fs:e.fs ~clock:e.clock ~sys:e.sys
       ~platform:e.platform ~os_key:e.os_key ~cache:e.cache ~data_dir ~registry
       ~use_registry ~session:e.http_session ~cwd ());
  edit_dune_project ~fs:e.fs ~cwd ~package ~dep_name ~op_ver;
  regenerate_opam_files ~proc_mgr:e.proc_mgr ~fs:e.fs ~cache:e.cache ~cwd;
  Fmt.pr "Re-syncing to pick up regenerated *.opam...@.";
  ignore
    (Sync.run ~quiet:true ~refresh:false ~with_repos ~with_deps:[] ?toolchain
       ~proc_mgr:e.proc_mgr ~fs:e.fs ~clock:e.clock ~sys:e.sys
       ~platform:e.platform ~os_key:e.os_key ~cache:e.cache ~data_dir ~registry
       ~use_registry ~session:e.http_session ~cwd ());
  Fmt.pr "Done.@."

let pkg_spec_term =
  Arg.(
    required
    & pos 0 (some string) None
    & info ~docv:"PKG"
        ~doc:
          "Opam package to add. Plain name ($(b,fmt)) lets the solver pick; \
           dotted form ($(b,fmt.0.9.5)) or relop ($(b,fmt>=0.9)) pins a \
           version."
        [])

let package_term =
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"NAME"
        ~doc:
          "Target $(b,\\(package …\\)) stanza in $(b,dune-project). Required \
           when more than one package is declared."
        [ "p"; "package" ])

let cmd_info =
  Cmd.info "add" ~doc:"Add a dependency to the current project"
    ~man:
      [
        `S Manpage.s_description;
        `P
          "Add $(i,PKG) to $(b,dune-project), regenerate $(b,*.opam), and \
           re-sync the prefix. Solve runs first; on failure, no files are \
           touched.";
        `P
          "Requires $(b,\\(generate_opam_files\\)) in $(b,dune-project). Use \
           $(b,-p NAME) to pick a stanza when the project declares several \
           packages.";
      ]

let cmd =
  let run (c : Terms.common) refresh registry use_registry with_repos toolchain
      package pkg_spec =
    Harness.run @@ fun ~sw env ->
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
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    let cwd, _ = Workspace.resolved_cwd fs in
    let env =
      { proc_mgr; fs; clock; sys; platform; os_key; cache; http_session }
    in
    run_add ~env ~data_dir:c.data_dir ~cwd ~refresh ~registry ~use_registry
      ~with_repos ~toolchain ~package ~pkg_spec
  in
  Cmd.v cmd_info
    Term.(
      const run $ Terms.common $ Terms.refresh $ Terms.registry
      $ Terms.use_registry $ Terms.with_repos $ Terms.toolchain $ package_term
      $ pkg_spec_term)
