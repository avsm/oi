open Cmdliner

let ( / ) = Filename.concat

(* ---- Helpers ---------------------------------------------------------- *)

let recipe_path_of_dir dir = dir / "recipe.json"

let load_recipe dir =
  let path = recipe_path_of_dir dir in
  if not (Sys.file_exists path) then Fmt.failwith "no recipe.json in %s" dir;
  let s =
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let n = in_channel_length ic in
        really_input_string ic n)
  in
  match D10ir.Plan.of_string s with
  | Ok plan -> plan
  | Error e -> Fmt.failwith "decoding %s: %s" path e

(* Add or replace the x-d10-archive extension on an opam file in
   place. Returns whether a change was actually written. *)
let opam_set_x_d10_archive ~path ~sha =
  let opam_file = OpamFile.make (OpamFilename.raw path) in
  let opam = OpamFile.OPAM.read opam_file in
  if Oi.Keys.read_string_ext Oi.Keys.d10_archive opam = Some sha then `Already
  else
    let v : OpamParserTypes.FullPos.value =
      {
        pelem = OpamParserTypes.FullPos.String sha;
        pos = OpamTypesBase.pos_null;
      }
    in
    let exts =
      OpamStd.String.Map.add Oi.Keys.d10_archive v
        (OpamFile.OPAM.extensions opam)
    in
    let opam' = OpamFile.OPAM.with_extensions exts opam in
    OpamFile.OPAM.write opam_file opam';
    `Added

(* Strip [patches:] and [extra-files:] from an opam file in place,
   and remove the sibling [files/] subdirectory that holds the patch
   blobs and extra-files content. Called after bake/restore once
   [x-d10-archive] is set: the baked archive already contains the
   patched + extras-included source tree, so the reporepo entry has
   no further use for them. The [files/] dir is the bulk of a baked
   reporepo's on-disk size, so reclaiming it matters. Returns whether
   anything was actually rewritten / removed. *)
let opam_strip_patches_extras ~fs ~opam_path =
  let opam_file = OpamFile.make (OpamFilename.raw opam_path) in
  let opam = OpamFile.OPAM.read opam_file in
  let had_patches = OpamFile.OPAM.patches opam <> [] in
  let had_extras = OpamFile.OPAM.extra_files opam <> None in
  let files_dir = Filename.dirname opam_path / "files" in
  let had_files_dir = Sys.file_exists files_dir in
  if not (had_patches || had_extras || had_files_dir) then `Already
  else begin
    if had_patches || had_extras then begin
      let opam =
        opam
        |> OpamFile.OPAM.with_patches []
        |> OpamFile.OPAM.with_extra_files_opt None
      in
      OpamFile.OPAM.write opam_file opam
    end;
    if had_files_dir then
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / files_dir);
    `Stripped
  end

(* ---- ir emit ---------------------------------------------------------- *)

(* Inputs to [build_request] grouped into a record because we extract them
   incrementally and pass the result to multiple helpers. *)
type emit_inputs = {
  names : OpamPackage.Name.t list;
  with_repos : string list;
  all_extras : Oi.Project.extra_repo list;
  extra_constraints : OpamFormula.version_constraint OpamPackage.Name.Map.t;
  toolchain : Oi.Toolchain.info option;
}

(* Extract target/with-deps inputs that the solver needs. Handles all pin
   extraction, constraint merging, and toolchain selection. *)
let prepare_emit_inputs ~fs ~sys ~cache ~data_dir ~refresh ~conf
    ~toolchain_override ~with_repos ~with_deps ~target =
  let extra_deps, url_project =
    Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh with_deps
  in
  let extra_constraints = Oi.Project.Script.constraints extra_deps in
  let with_repos = url_project.overlays @ with_repos in
  let targets, with_repos, target_pins =
    Target.extract_handle_pins ~with_repos [ target ]
  in
  let _with_deps_strs, with_repos, with_pins =
    Target.extract_handle_pins ~with_repos
      (List.map
         (fun (d : Oi.Project.Script.dep) -> OpamPackage.Name.to_string d.name)
         extra_deps)
  in
  let handle_pins = target_pins @ with_pins in
  let tc_handles =
    Target.pin_handles handle_pins @ Target.handles_of_tokens with_repos
    |> List.sort_uniq String.compare
  in
  let toolchain =
    Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf ~install:true
      ~override:toolchain_override ~handles:tc_handles ()
  in
  let cli_extras = Target.cli_extra_repos ~fs ~sys ?toolchain with_repos in
  let all_extras = Target.merge_extras ~cli:cli_extras ~project:[] in
  let handle_constraints =
    Target.handle_pin_constraints ~fs ~data_dir ~refresh ~cli_extras handle_pins
  in
  let extra_constraints =
    OpamPackage.Name.Map.union
      (fun a _ -> a)
      handle_constraints extra_constraints
  in
  let names =
    List.map OpamPackage.Name.of_string targets
    |> Oi.Pipeline.strip_compiler_roots_for_override
         ~override:toolchain_override ~toolchain
  in
  { names; with_repos; all_extras; extra_constraints; toolchain }

(* Build the solver request record from [emit_inputs]. *)
let build_request ~conf ~toolchain_override ~refresh
    { names; with_repos; all_extras; extra_constraints; toolchain } :
    Oi.Build_pipeline.request =
  {
    targets =
      [
        Group
          { tokens = List.map OpamPackage.Name.to_string names; handles = [] };
      ];
    (* Handles extracted from [@H/PKG] tokens must flow to the per-group
       solve via [with_repos]; [extra_repos] alone doesn't add their
       [packages/] trees. *)
    with_repos;
    pins = [];
    extra_repos = all_extras;
    constraints = extra_constraints;
    toolchain_override;
    toolchain;
    conf;
    local_packages_dir = None;
    project_root = None;
    force_source = true;
    with_test = false;
    refresh;
  }

(* Write each solved group's recipe to [out_dir/recipe.json]. [oi ir emit]
   takes a single TARGET today so the loop typically runs once. *)
let write_recipes ~fs ~out_dir (solved : Oi.Build_pipeline.solved) =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / out_dir);
  List.iter
    (fun (gr : Oi.Build_pipeline.group_result) ->
      match gr.recipe with
      | None -> ()
      | Some recipe ->
          let dst = Filename.concat out_dir "recipe.json" in
          D10ir.Plan.save Eio.Path.(fs / dst) recipe;
          Oi.Say.ok "emitted recipe to %s" dst)
    solved.groups

(* Mirrors the prep [oi run @handle/pkg] does before [Build_pipeline.build]: no
   project mode, just resolve toolchain, materialise overlays, and call
   the pipeline with [emit_recipe] set. *)
let emit_run (c : Terms.common) refresh registry use_registry with_repos
    with_deps toolchain_override out_dir target =
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
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  let data_dir = c.data_dir in
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
  let conf =
    Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  (* [oi ir emit] is solver-only — no fetch / build phases run, so
     the [Terms.layer_remote] / [source_remote] selection is unused
     here. *)
  let _ = registry in
  let _ = use_registry in
  let inputs =
    prepare_emit_inputs ~fs ~sys ~cache ~data_dir ~refresh ~conf
      ~toolchain_override ~with_repos ~with_deps ~target
  in
  if inputs.names = [] then
    Oi.Error.fail_config_error "oi ir emit: no target after pin extraction.";
  let pipeline_env : Oi.Build_pipeline.env =
    { proc_mgr; fs; clock; sys; os_key; cache; data_dir; http_session }
  in
  let req = build_request ~conf ~toolchain_override ~refresh inputs in
  let solved = Oi.Build_pipeline.solve pipeline_env req in
  write_recipes ~fs ~out_dir solved;
  0

let emit_target_term =
  Arg.(
    required
    & pos 0 (some string) None
    & info ~docv:"TARGET"
        ~doc:"Build target: package name, $(b,@HANDLE/PKG), or $(b,@HANDLE)"
        [])

let emit_out_dir_term =
  Arg.(
    required
    & opt (some string) None
    & info ~docv:"DIR"
        ~doc:"Write $(b,recipe.json) into $(i,DIR), creating it if absent"
        [ "o"; "out" ])

let emit_info =
  Cmd.info "emit" ~doc:"Solve, plan, and write a d10ir recipe"
    ~man:
      [
        `S Cmdliner.Manpage.s_description;
        `P
          "Solve for $(b,TARGET), fetch and patch every package's source, and \
           write $(b,recipe.json) into $(i,DIR). No compilation happens. \
           Replay with $(b,oi ir run DIR).";
        `P
          "$(b,TARGET) takes the same shapes as $(b,oi run): bare package, \
           $(b,@HANDLE/PKG), or URL-pinned via $(b,--with).";
      ]

let emit_cmd =
  let term =
    Term.(
      const
        (fun c refresh registry use_registry with_repos with_deps
             toolchain_override target out ->
          let code =
            emit_run c refresh registry use_registry with_repos with_deps
              toolchain_override out target
          in
          if code <> 0 then exit code)
      $ Terms.common $ Terms.refresh $ Terms.registry $ Terms.use_registry
      $ Terms.with_repos $ Terms.with_deps $ Terms.toolchain $ emit_target_term
      $ emit_out_dir_term)
  in
  Cmd.v emit_info term

(* ---- ir run ----------------------------------------------------------- *)

(* [run] stays oi-side because it wraps the d10ir executor in
   [Progress_ui] for the live progress bar. The plain-text variant in
   [D10ir_cmd.run_cmd] is available as a standalone CLI. *)
let run_run (c : Terms.common) dir parallel keep_staging =
  Harness.run @@ fun ~sw env ->
  let { Harness.fs; clock; sys; os_key; cache; proc_mgr; _ } =
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  let plan = load_recipe dir in
  let d10 =
    Oi.Pipeline.d10 ~sys ~fs ~clock:(clock :> D10.Config.clk) ~cache ~os_key
  in
  (match D10ir.Plan.validate ~d10 ~fs ~plan_dir:dir plan with
  | Ok () -> ()
  | Error e ->
      Oi.Say.error "%a" D10ir.Plan.pp_validate_error e;
      exit 1);
  let cfg =
    let base = D10ir.Config.default in
    let base = D10ir.Config.with_env_overrides base in
    {
      base with
      build_parallelism =
        (match parallel with Some p -> p | None -> base.build_parallelism);
      keep_staging = base.keep_staging || keep_staging;
    }
  in
  (* Drive the unified [Progress_ui]: it materialises the recipe's
     dep tree on [Plan_ready] and renders per-node status updates as
     [D10ir.Direct] events arrive. We synthesise [Plan_ready]
     ourselves since this command bypasses [Build_pipeline.build]. *)
  let result =
    Progress_ui.with_ui ~target:dir
      ~clock:(clock :> _ Eio.Resource.t)
      ~enabled:(Tty.is_tty ())
    @@ fun reporter ->
    reporter.event (Plan_ready plan);
    reporter.event (Phase_started { phase = Building; label = "ir run" });
    let direct_reporter : D10ir.Direct.reporter =
      { event = (fun e -> reporter.event (Build e)) }
    in
    let r =
      D10ir.Direct.run ~config:cfg ~d10 ~fs ~proc_mgr
        ~clock:(clock :> D10.Config.clk)
        ~reporter:direct_reporter ~plan_dir:dir plan
    in
    reporter.event (Build_summary r);
    reporter.event (Phase_done Building);
    r
  in
  if result.failed > 0 then
    Fmt.epr "@.%a@.@." D10ir.Direct.pp_failures result.failures;
  Oi.Say.step "ok %d  cached %d  failed %d  skipped %d" result.built
    result.cached result.failed result.skipped;
  if result.failed > 0 then 31 else 0

let run_cmd =
  let dir =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"DIR" ~doc:"Recipe directory" [])
  in
  let parallel =
    Arg.(
      value
      & opt (some int) None
      & info ~docv:"N"
          ~doc:
            "Build $(i,N) layers in parallel; overrides \
             $(b,OI_BUILD_PARALLELISM)"
          [ "j"; "jobs" ])
  in
  let keep =
    Arg.(
      value & flag
      & info ~doc:"Keep per-node staging directories after build (debug)"
          [ "keep-staging" ])
  in
  let term =
    Term.(
      const (fun c d j k ->
          let code = run_run c d j k in
          if code <> 0 then exit code)
      $ Terms.common $ dir $ parallel $ keep)
  in
  Cmd.v (Cmd.info "run" ~doc:"Execute a d10ir recipe") term

(* ---- group ------------------------------------------------------------ *)

(* [validate], [merge], [show] are pure d10ir operations, so they live
   in the [d10ir.cmd] library and are re-exposed here under the [oi ir]
   group with no oi-specific wrapping. [run] stays local because it's
   wrapped in [Progress_ui] for the live UI; [emit] needs the full oi
   pipeline (toolchain resolution, reporepo, solver). *)
let cmd =
  Cmd.group
    (Cmd.info "ir" ~doc:"Inspect, validate, and execute d10ir recipes"
       ~man:
         [
           `S Cmdliner.Manpage.s_description;
           `P
             "Operate on serialised d10ir recipes ($(b,recipe.json) + \
              archives). Emit a recipe with $(b,oi ir emit), inspect with \
              $(b,oi ir show), check archives and dep layers with $(b,oi ir \
              validate), then replay with $(b,oi ir run).";
         ])
    [
      emit_cmd;
      run_cmd;
      D10ir_cmd.validate_cmd;
      D10ir_cmd.show_cmd;
      D10ir_cmd.merge_cmd;
    ]
