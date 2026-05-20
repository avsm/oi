open Cmdliner

let ( / ) = Filename.concat

(* Inputs the [oi env] one-shot prefix build needs from the command line
   plus the resolved toolchain. Grouped into a record so the helpers stay
   readable and avoid long positional arg lists. *)
type oneshot_inputs = {
  harness : Harness.env;
  refresh : bool;
  with_repos : string list;
  with_deps : string list;
  jobs : int option;
  tc_info : Oi.Toolchain.info option;
  conf : Oi.Solver.Ctx.conf;
  cwd_s : string;
}

(* Resolve --with-* into the (cli_deps, url_project) split used downstream:
   plain package names go into [extra_cli], URL-shaped tokens promote into
   an in-memory "URL project" carrying its own pins / overlays / roots. *)
let oneshot_extras (i : oneshot_inputs) =
  let { harness = { fs; sys; cache; _ }; refresh; with_repos; with_deps; _ } =
    i
  in
  let extra_cli, url_project =
    Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh with_deps
  in
  let url_overlays =
    Oi.Pipeline.filter_compatible_overlays
      ~reporepo_path:(Terms.reporepo_path ()) ~toolchain:i.tc_info
      url_project.overlays
  in
  let extras =
    Target.merge_extras
      ~cli:
        (Target.cli_extra_repos ~fs ~sys ?toolchain:i.tc_info
           (with_repos @ url_overlays))
      ~project:url_project.extra_repos
  in
  let extra_constraints = Oi.Project.Script.constraints extra_cli in
  let extra_names =
    List.filter_map
      (fun (d : Oi.Project.Script.dep) ->
        if OpamPackage.Name.to_string d.name = "ocaml" then None
        else Some d.name)
      extra_cli
    @ List.map OpamPackage.Name.of_string url_project.roots
  in
  (url_project, extras, extra_constraints, extra_names)

let oneshot_request (i : oneshot_inputs) ~url_project ~extras ~extra_constraints
    ~extra_names : Oi.Build_pipeline.request =
  let names = "ocaml" :: List.map OpamPackage.Name.to_string extra_names in
  {
    targets = [ Group { tokens = names; handles = [] } ];
    with_repos = [];
    pins = url_project.Oi.Project.Url.pins;
    extra_repos = extras;
    constraints = extra_constraints;
    toolchain_override = None;
    toolchain = i.tc_info;
    conf = i.conf;
    local_packages_dir = url_project.packages_dir;
    project_root = Some i.cwd_s;
    force_source = false;
    with_test = false;
    refresh = i.refresh;
  }

(* Build a one-shot opam prefix without touching the on-disk [_oi/prefix].
   Used when --skip-local / --with-* / --toolchain are passed, or when the
   project simply hasn't been solved yet. *)
let build_oneshot_prefix (i : oneshot_inputs) =
  let {
    Harness.proc_mgr;
    fs;
    clock;
    sys;
    os_key;
    cache;
    http_session;
    data_dir;
    _;
  } =
    i.harness
  in
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  ignore
    (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh:i.refresh ());
  let url_project, extras, extra_constraints, extra_names = oneshot_extras i in
  let pipeline_env : Oi.Build_pipeline.env =
    { proc_mgr; fs; clock; sys; os_key; cache; data_dir; http_session }
  in
  let req =
    oneshot_request i ~url_project ~extras ~extra_constraints ~extra_names
  in
  let solved = Oi.Build_pipeline.solve pipeline_env req in
  let _ : D10ir.Direct.result option =
    Oi.Build_pipeline.build pipeline_env
      {
        solved;
        layer_remote = None;
        source_remote = None;
        jobs = i.jobs;
        upload_archive_url = None;
        archive_sources = false;
        snapshot_reporepo = false;
        install_to = None;
      }
  in
  Oi.Pipeline.assemble_prefix ~sys ~fs ~clock ~cache ~os_key
    ~layer_hashes:(Oi.Build_pipeline.layer_hashes solved)

(* Pick the prefix: reuse [_oi/prefix] when it exists and the invocation
   asks for no extras; otherwise build a one-shot prefix. *)
let resolve_prefix (i : oneshot_inputs) ~skip_local ~toolchain ~cwd ~oi_prefix =
  let want_extras =
    skip_local || i.with_repos <> [] || i.with_deps <> [] || toolchain <> None
  in
  if (not want_extras) && Workspace.path_exists cwd "_oi/prefix" then oi_prefix
  else build_oneshot_prefix i

(* Print [export VAR="value"] for every variable, splicing the user's
   current PATH onto the prefix bin dirs. *)
let print_exports ~vars ~path_prefix =
  let current_path =
    try Sys.getenv "PATH" with Not_found -> "/usr/bin:/bin"
  in
  List.iter
    (fun (k, v) ->
      let v = if k = "PATH" then path_prefix ^ ":" ^ current_path else v in
      Fmt.pr "export %s=\"%s\"@." k v)
    vars

(* Compute the project's PATH prefix: project-local tools/bin (if any),
   then the assembled opam prefix bin. *)
let path_prefix_for ~cwd_s ~prefix =
  let tools = Workspace.tools_dir_for ~cwd:cwd_s in
  match tools with
  | None -> prefix / "bin"
  | Some t -> (t / "bin") ^ ":" ^ (prefix / "bin")

let run (c : Terms.common) ~refresh ~skip_local ~with_repos ~with_deps ~jobs
    ~toolchain =
  Harness.run @@ fun ~sw env ->
  let harness =
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  let { Harness.fs; sys; cache; _ } = harness in
  let data_dir = c.data_dir in
  let dune_cache_root = Oi.Cache.dune_root cache in
  (* Detect _oi/ project directory. A pre-existing _oi/prefix is
     reused as-is UNLESS the user passes --with-repo or --with, which
     demands a fresh solve to honour the additions. [--skip-local]
     forces a one-shot prefix and ignores any existing [_oi/prefix]. *)
  let cwd_s, cwd = Workspace.resolved_cwd fs in
  let oi_prefix = cwd_s / "_oi" / "prefix" in
  let conf =
    Oi.Pipeline.conf ~platform:harness.platform
      ~ocaml_version:Workspace.ocaml_version
  in
  (* Use the same project-aware resolve [oi sync] uses, so [oi env] picks
     the toolchain the existing prefix was actually built with (project
     [x-repos:] declarations and all). *)
  let tc_info =
    Sync.resolve_project_toolchain ~refresh ~skip_local ~with_repos ~with_deps
      ~fs ~sys ~cache ~data_dir ~conf ~install:true ~override:toolchain
      ~cwd:cwd_s ()
  in
  let conf, tc_ctx = Oi.Pipeline.solver_inputs tc_info conf in
  let oneshot =
    { harness; refresh; with_repos; with_deps; jobs; tc_info; conf; cwd_s }
  in
  let prefix = resolve_prefix oneshot ~skip_local ~toolchain ~cwd ~oi_prefix in
  let vars =
    Oi.Solver.Env.env_vars ?toolchain:tc_ctx ~prefix ~dune_cache_root ()
  in
  print_exports ~vars ~path_prefix:(path_prefix_for ~cwd_s ~prefix)

let man =
  [
    `S Manpage.s_description;
    `P
      "Print $(b,export) statements for $(b,PATH), $(b,OCAMLLIB), and the \
       other OCaml variables pointing at $(b,_oi/prefix/). Non-$(b,direnv) \
       equivalent of $(b,.envrc).";
    `Pre "  eval \"\\$(oi env)\"";
    `P
      "Reuses $(b,_oi/prefix/) when it exists and no extras are requested. \
       With $(b,--with), $(b,--with-repo), $(b,--toolchain), \
       $(b,--skip-local), or a missing prefix, builds a one-shot prefix \
       in-process; $(b,.envrc) and dev tools are not updated. Use $(b,oi build \
       --deps-only) for that.";
    `S "TOOLCHAIN";
    `P "Active toolchain, in order:";
    `I ("1.", "$(b,--toolchain=NAME).");
    `I
      ( "2.",
        "$(b,x-oi-toolchain) on any in-scope $(b,@HANDLE) — from \
         $(b,--with-repo=@h), $(b,--with=@h/pkg), or the project's \
         $(b,x-repos:). Conflicting tags error out." );
    `I ("3.", "Reporepo entry flagged $(b,x-oi-default-toolchain: true).");
  ]

let cmd =
  let info =
    Cmd.info "env" ~doc:"Print shell exports for the project environment" ~man
  in
  let term =
    let go c refresh skip_local with_repos with_deps jobs toolchain =
      run c ~refresh ~skip_local ~with_repos ~with_deps ~jobs ~toolchain
    in
    Term.(
      const go $ Terms.common $ Terms.refresh $ Terms.skip_local
      $ Terms.with_repos $ Terms.with_deps $ Terms.jobs $ Terms.toolchain)
  in
  Cmd.v info term
