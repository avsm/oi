open Cmdliner

let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.cmd.build"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* End-of-run per-target summary table. One row per CLI target, in the
   order requested. Targets that share a solve group repeat the
   group-level counts. *)
type group_counts = { n_pkgs : int; n_built : int; n_cached : int }

(* End-state of one solve group's build, populated after
   [Build_pipeline.build] returns. Replaces the old polymorphic
   variant + 5-tuple shape with named fields. *)
type group_outcome =
  | Group_ok of group_counts
  | Group_failed of {
      counts : group_counts;
      msg : string;
      per_pkg_logs : (string * string) list;
    }

module Summary = struct
  type result =
    | Skipped of { msg : string; log_path : string }
    | Ok of group_counts
    | Failed of {
        counts : group_counts;
        msg : string;
        per_pkg_logs : (string * string) list;
      }

  let result_for ~solve_failures ~target_group ~group_results t =
    match Hashtbl.find_opt solve_failures t with
    | Some (msg, log_path) -> Skipped { msg; log_path }
    | None -> (
        match Hashtbl.find_opt target_group t with
        | None -> Skipped { msg = "unknown"; log_path = "" }
        | Some gi -> (
            match Hashtbl.find_opt group_results gi with
            | None -> Skipped { msg = "group not built"; log_path = "" }
            | Some (Group_ok counts) -> Ok counts
            | Some (Group_failed { counts; msg; per_pkg_logs }) ->
                Failed { counts; msg; per_pkg_logs }))

  let tally rows =
    List.fold_left
      (fun (o, f, s) (_, _, r) ->
        match r with
        | Ok _ -> (o + 1, f, s)
        | Failed _ -> (o, f + 1, s)
        | Skipped _ -> (o, f, s + 1))
      (0, 0, 0) rows

  let status_col = function
    | Ok _ -> Fmt.str "%a" Oi.Style.pp_ok_string "ok"
    | Failed _ -> Fmt.str "%a" Oi.Style.pp_error_string "fail"
    | Skipped _ -> Fmt.str "%a" Oi.Style.pp_warn_string "skip"

  let first_line s =
    match String.split_on_char '\n' s with [] -> "" | h :: _ -> h

  let detail_col = function
    | Ok { n_pkgs; n_built; n_cached } ->
        Fmt.str "%d pkg (%d built, %d cached)" n_pkgs n_built n_cached
    | Failed { counts = { n_pkgs; n_built; n_cached }; _ } ->
        Fmt.str "%d pkg (%d built, %d cached), build failed" n_pkgs n_built
          n_cached
    | Skipped { msg; _ } -> Fmt.str "skipped (%s)" (first_line msg)

  let print_row ~target_width ~handle_width (target, handle, r) =
    let styled_handle h =
      if h = "" then String.make handle_width ' '
      else
        let padded = Fmt.str "%-*s" handle_width h in
        Fmt.str "%a" Oi.Style.pp_info_string padded
    in
    if handle_width = 0 then
      Fmt.pr "  %-6s %-*s  %s@." (status_col r) target_width target
        (detail_col r)
    else
      Fmt.pr "  %-6s %s  %-*s  %s@." (status_col r) (styled_handle handle)
        target_width target (detail_col r);
    match r with
    | Failed { per_pkg_logs; _ } ->
        List.iter
          (fun (pkg, log_path) ->
            Fmt.pr "         %a %s: %s@." Oi.Style.pp_dim_string "↳ log" pkg
              log_path)
          per_pkg_logs
    | Skipped { log_path; _ } when log_path <> "" ->
        Fmt.pr "         %a %s@." Oi.Style.pp_dim_string "↳ solver log:"
          log_path
    | _ -> ()

  (* Style each [label N] pair brightly only when N > 0, so the eye
     skips past zeros and lands on whatever actually happened. *)
  let print_tally ~n_ok ~n_failed ~n_skipped =
    let pair styled_when_active label n =
      if n = 0 then Fmt.str "%a %d" Oi.Style.pp_dim_string label n
      else Fmt.str "%a %d" styled_when_active label n
    in
    Fmt.pr "  %s  %s  %s@."
      (pair Oi.Style.pp_ok_string "ok" n_ok)
      (pair Oi.Style.pp_error_string "failed" n_failed)
      (pair Oi.Style.pp_warn_string "skipped" n_skipped)

  let log_failures rows =
    List.iter
      (fun (target, _handle, r) ->
        match r with
        | Failed { msg; _ } -> Log.info (fun m -> m "%s: %s" target msg)
        | Skipped { msg; _ } when String.contains msg '\n' ->
            Log.info (fun m -> m "%s: %s" target msg)
        | _ -> ())
      rows

  let print_log_tail ~pkg ~log_path tail =
    Oi.Say.newline ();
    Fmt.kstr
      (Fmt.pr "%a %s %a@." Oi.Style.pp_error_string "── tail" pkg
         Oi.Style.pp_dim_string)
      "(%s)" log_path;
    Fmt.pr "%s@." tail

  let dump_one_failure ~seen (pkg, log_path) =
    if
      log_path = "" || Hashtbl.mem seen log_path
      || not (Sys.file_exists log_path)
    then ()
    else begin
      Hashtbl.replace seen log_path ();
      match Oi.Audit.tail_of_file ~lines:100 ~path:log_path () with
      | None -> ()
      | Some tail -> print_log_tail ~pkg ~log_path tail
    end

  (* CI tail dump: any decent CI UI swallows the log files we point at, so
     under [CI=…] we inline the last 100 lines of each failed package's log
     right into the transcript. *)
  let dump_ci_tails rows =
    if not (Terms.in_ci ()) then ()
    else
      let seen : (string, unit) Hashtbl.t = Hashtbl.create 16 in
      List.iter
        (fun (_target, _handle, r) ->
          match r with
          | Failed { per_pkg_logs; _ } ->
              List.iter (dump_one_failure ~seen) per_pkg_logs
          | _ -> ())
        rows
end

let print_build_summary ~targets ~target_handle ~solve_failures ~target_group
    ~group_results =
  let handle_for t =
    match Hashtbl.find_opt target_handle t with Some h -> "@" ^ h | None -> ""
  in
  let rows =
    List.map
      (fun t ->
        ( t,
          handle_for t,
          Summary.result_for ~solve_failures ~target_group ~group_results t ))
      targets
  in
  let n_ok, n_failed, n_skipped = Summary.tally rows in
  let target_width =
    List.fold_left (fun w (t, _, _) -> max w (String.length t)) 12 rows
  in
  let handle_width =
    List.fold_left (fun w (_, h, _) -> max w (String.length h)) 0 rows
  in
  Oi.Say.header "Build summary";
  List.iter (Summary.print_row ~target_width ~handle_width) rows;
  Oi.Say.newline ();
  Summary.print_tally ~n_ok ~n_failed ~n_skipped;
  Summary.log_failures rows;
  Summary.dump_ci_tails rows

(* -- Overlay-wide depext helpers ---------------------------------------- *)

(* Driver for [oi docker --all]'s depext pre-pass. One per reporepo
   handle, in priority order:

   - [Solve_groups]: the entry declares [x-root-packages] (overlay roots)
     or [x-oi-toolchain-roots] (toolchain definition). Each group is
     fed to the solver under the toolchain attached to the handle, and
     the solved closure drives depext computation. Toolchain handles
     contribute compiler-stack depexts (libgmp-dev for [zarith.+ox],
     etc.) that the host overlay's roots transitively depend on but
     don't always pull in directly.

   - [Walk_clone]: a non-toolchain overlay with neither declaration but
     a non-empty clone (i.e. [url <> ""]). [oi build --all] handles this
     case by fanning out to "every package in the overlay's clone" via
     [Overlay_all] expansion; we mirror that here by treating every
     package in [v2/<handle>/packages/] as its own depext source — no
     solve. Without this, an overlay like [@oxcaml] (no [x-root-packages]
     set) contributes zero depexts to the docker pre-install, and there
     is nothing left in [registry_docker.ml] to cover the gap (the
     hardcoded [extra_depexts] list has moved into the opam packages'
     own [depexts:] fields). *)
type overlay_input =
  | Solve_groups of { handle : string; groups : string list list }
  | Walk_clone of { handle : string }

let overlay_input_handle = function
  | Solve_groups { handle; _ } | Walk_clone { handle } -> handle

let overlay_inputs reporepo_entries =
  reporepo_entries
  |> List.map (fun (e : Oi.Source.Reporepo.entry) -> e.handle)
  |> List.sort_uniq String.compare
  |> List.filter_map (fun h ->
      if h = "default" then None
      else
        match Oi.Source.Reporepo.latest reporepo_entries ~handle:h with
        | None -> None
        | Some e when e.root_packages <> [] ->
            Some (Solve_groups { handle = h; groups = e.root_packages })
        | Some e when e.toolchain_roots <> [] ->
            Some (Solve_groups { handle = h; groups = e.toolchain_roots })
        | Some e when e.toolchain_name = None && e.url <> "" ->
            Some (Walk_clone { handle = h })
        | _ -> None)

let parse_pkg_version ~name_dir pv =
  let opam_path = name_dir / pv / "opam" in
  if not (Sys.file_exists opam_path) then None
  else try Some (OpamPackage.of_string pv) with Failure _ -> None

let versions_in_name_dir name_dir =
  if (not (Sys.file_exists name_dir)) || not (Sys.is_directory name_dir) then []
  else
    Sys.readdir name_dir |> Array.to_list
    |> List.filter_map (parse_pkg_version ~name_dir)

(* Enumerate every [name/name.version/opam] under [pkgs_dir] as
   [OpamPackage.t]. Used by the [Walk_clone] arm of {!overlay_inputs}:
   no solving, just "anything the overlay ships could land in the
   container, so its depexts must be installed up front". *)
let all_packages_in_dir pkgs_dir =
  if not (Sys.file_exists pkgs_dir) then []
  else
    Sys.readdir pkgs_dir |> Array.to_list
    |> List.concat_map (fun name -> versions_in_name_dir (pkgs_dir / name))

(* Resolve the effective [packages_dirs] for a single overlay handle:
   its own materialised v2/ tree plus every overlay it depends on (via
   the reporepo's [depends:] entries), followed by the base
   opam/relocatable trees. First-wins ordering. *)
let packages_dirs_for_overlay ~base_packages_dirs ~reporepo_entries handle =
  let roots = [ { Oi.Source.Reporepo.handle; version = None } ] in
  let transitive =
    try Oi.Source.Reporepo.resolve reporepo_entries ~roots |> List.rev
    with Oi.Error.E _ -> []
  in
  let reporepo_path = Terms.reporepo_path () in
  let overlay_dirs =
    List.filter_map
      (fun (e : Oi.Source.Reporepo.entry) ->
        if e.url = "" then None
        else
          Some
            (Oi.Source.Reporepo.assert_overlay_dir ~path:reporepo_path
               ~handle:e.handle))
      transitive
  in
  let seen = Hashtbl.create 8 in
  let dedup xs =
    List.filter
      (fun d ->
        if Hashtbl.mem seen d then false
        else begin
          Hashtbl.replace seen d ();
          true
        end)
      xs
  in
  dedup (overlay_dirs @ base_packages_dirs)

(* Gather the [(pkg_dirs, packages)] pairs that drive depext
   computation. Each pair is a closure over which {!Oi.Depexts.compute_for_conf}
   is later evaluated under each distro's filter context.

   - [Solve_groups]: each declared root group is fed to the solver
     under the handle's toolchain. Solves run once under [host_conf]
     and are reused across every per-distro depext evaluation, since
     opam filter variables (os, os-family, …) don't influence the
     solver picks here.
   - [Walk_clone]: every [opam] file in [v2/<handle>/packages/] is
     listed verbatim — no solve. The overlay's [packages_dirs] is still
     the toolchain-aware closure from {!packages_dirs_for_overlay} so
     {!Oi.Solver.find_opam_file} resolves each version against the right
     tree (overlay first, then base). *)
(* For toolchain-defining handles ([toolchain-oxcaml],
   [toolchain-ocaml-5-3], …) the entry carries its own
   [x-oi-toolchain-name], but [Pipeline.toolchain_names_of_handle]
   only consults [x-oi-toolchain] (use-site) and the
   [Toolchain.depends_of] reverse map (where the lookup key is the
   CLI name, not the reporepo handle). Without this fallback, those
   handles resolve to the default toolchain and the solve fails with
   compiler-version conflicts. *)
let toolchain_for_overlay ~fs ~sys ~data_dir ~host_conf ~override
    ~reporepo_entries handle =
  let auto_override =
    match override with
    | Some _ -> override
    | None -> (
        match Oi.Source.Reporepo.latest reporepo_entries ~handle with
        | Some e when e.toolchain_name <> None -> e.toolchain_name
        | _ -> None)
  in
  Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf:host_conf ~install:false
    ~override:auto_override ~handles:[ handle ] ()

let constraints_of_items items =
  List.fold_left
    (fun acc (name, c) ->
      match c with None -> acc | Some c -> OpamPackage.Name.Map.add name c acc)
    OpamPackage.Name.Map.empty items

let solve_one_group ~sys ~fs ~cache_root ~build_prefix ~pkg_dirs ~conf ~tc_ctx
    ~handle group =
  let ctx =
    Oi.Solver.Ctx.create ~prefix:build_prefix ~packages_dirs:pkg_dirs ~conf
      ?toolchain:tc_ctx ()
  in
  let items = List.map Target.parse_pkg_target group in
  let names = List.map fst items in
  let constraints = constraints_of_items items in
  match
    Oi.Solver.solve ~sys ~fs ~cache_root ctx ~packages_dirs:pkg_dirs
      ~constraints names
  with
  | Ok solved -> Some (pkg_dirs, solved)
  | Error msg ->
      Log.warn (fun m ->
          m "overlay depexts: %s group failed to solve: %s" handle msg);
      None

let solve_overlay_groups ~sys ~fs ~cache_root ~build_prefix ~pkg_dirs ~host_conf
    ~toolchain ~handle groups =
  let conf, tc_ctx = Oi.Pipeline.solver_inputs toolchain host_conf in
  List.filter_map
    (solve_one_group ~sys ~fs ~cache_root ~build_prefix ~pkg_dirs ~conf ~tc_ctx
       ~handle)
    groups

let walk_clone_pkgs ~path handle =
  match
    try Some (Oi.Source.Reporepo.assert_overlay_dir ~path ~handle)
    with Oi.Error.E _ -> None
  with
  | None -> []
  | Some d -> all_packages_in_dir d

let process_overlay_input ~fs ~sys ~cache_root ~build_prefix ~data_dir
    ~host_conf ~override ~reporepo_entries ~pkg_dirs_for ~path = function
  | Solve_groups { handle; groups } -> (
      let toolchain =
        try
          toolchain_for_overlay ~fs ~sys ~data_dir ~host_conf ~override
            ~reporepo_entries handle
        with Oi.Error.E _ ->
          Log.warn (fun m ->
              m
                "overlay depexts: %s toolchain resolution failed — skipping \
                 handle"
                handle);
          None
      in
      match toolchain with
      | None -> []
      | Some _ ->
          solve_overlay_groups ~sys ~fs ~cache_root ~build_prefix
            ~pkg_dirs:(pkg_dirs_for handle) ~host_conf ~toolchain ~handle groups
      )
  | Walk_clone { handle } ->
      let pkg_dirs = pkg_dirs_for handle in
      let pkgs = walk_clone_pkgs ~path handle in
      if pkgs = [] then []
      else begin
        Log.info (fun m ->
            m
              "overlay depexts: %s walking %d packages (no x-root-packages \
               declared)"
              handle (List.length pkgs));
        [ (pkg_dirs, pkgs) ]
      end

let gather_overlay_solves ~fs ~sys ~cache ~data_dir ~refresh ~host_conf
    ?override ?handle_filter () =
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  let base_packages_dirs =
    Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ()
  in
  let path = Terms.reporepo_path () in
  Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh ~path
    ~url:(Terms.reporepo_url ()) ();
  let reporepo_entries =
    try Oi.Source.Reporepo.load ~path with Sys_error _ | Failure _ -> []
  in
  let inputs =
    let all = overlay_inputs reporepo_entries in
    match handle_filter with
    | None -> all
    | Some h -> List.filter (fun i -> overlay_input_handle i = h) all
  in
  let cache_root = Oi.Cache.root_s cache in
  let build_prefix = cache_root / "build" / "prefix" in
  let pkg_dirs_for handle =
    packages_dirs_for_overlay ~base_packages_dirs ~reporepo_entries handle
  in
  List.concat_map
    (process_overlay_input ~fs ~sys ~cache_root ~build_prefix ~data_dir
       ~host_conf ~override ~reporepo_entries ~pkg_dirs_for ~path)
    inputs

let depexts_union ~conf solves =
  let all =
    List.fold_left
      (fun acc (pkg_dirs, solved) ->
        let entries =
          Oi.Depexts.compute_for_conf ~conf ~packages_dirs:pkg_dirs solved
        in
        List.fold_left
          (fun acc e -> OpamSysPkg.Set.union acc e.Oi.Depexts.sys_pkgs)
          acc entries)
      OpamSysPkg.Set.empty solves
  in
  OpamSysPkg.Set.elements all |> List.map OpamSysPkg.to_string

let compute_overlay_depexts_for_conf ~fs ~sys ~cache ~data_dir ~refresh ~conf
    ?override ?handle () =
  let solves =
    gather_overlay_solves ~fs ~sys ~cache ~data_dir ~refresh ~host_conf:conf
      ?override ?handle_filter:handle ()
  in
  depexts_union ~conf solves

let compute_overlay_depexts_per_distro ~fs ~sys ~cache ~data_dir ~refresh
    ~platform ~distros =
  let host_conf =
    Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  let solves =
    gather_overlay_solves ~fs ~sys ~cache ~data_dir ~refresh ~host_conf ()
  in
  List.map
    (fun distro ->
      let vars = Registry_docker.opam_vars_of_distro distro in
      let distro_conf =
        {
          host_conf with
          os = "linux";
          os_distribution = vars.os_distribution;
          os_family = vars.os_family;
          os_version = vars.os_version;
        }
      in
      (distro, depexts_union ~conf:distro_conf solves))
    distros

(* -- Single-target test mode ------------------------------------------- *)

(* Locate the layer whose package name matches [pkg_name]. The dotted
   prefix match keys on [name + "."], so the exact version found by the
   solver doesn't have to be hardcoded. *)
let find_target_layer ~fs ~cache ~os_key ~pkg_name layer_hashes =
  let layers_dir = Oi.Cache.root_s cache / "layers" / os_key in
  let prefix = pkg_name ^ "." in
  List.find_map
    (fun h ->
      match
        D10.Layer.load_meta Eio.Path.(fs / layers_dir / h / "layer.json")
      with
      | Some (m : D10.Layer.meta)
        when String.length m.package >= String.length prefix
             && String.sub m.package 0 (String.length prefix) = prefix ->
          Some (h, m.package)
      | _ -> None)
    layer_hashes

(* Inputs gathered after target-prefix parsing — the verbatim pieces
   each later phase ([toolchain pick], [extras], [solve request]) needs. *)
type target_test_inputs = {
  target : string;
  target_display : string;
  with_repos : string list;
  with_deps : string list;
  target_pin : Target.handle_pin option;
  with_pins : Target.handle_pin list;
}

let parse_target_test_inputs ~target ~with_repos ~with_deps =
  let target_display = target in
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
  { target; target_display; with_repos; with_deps; target_pin; with_pins }

let print_dry_run_test_plan target_display =
  Fmt.pr
    "@[<v>%a@,\
     @,\
    \  oi build %s@,\
    \  cd <build_dir>@,\
    \  dune runtest --profile=release@,\
     @]@."
    Oi.Style.pp_header_string "Would run:" target_display

(* Overlay handles must flow through so [solve_uncached] can
   resolve them into [packages/] dirs — [extra_repos] alone
   isn't read by the per-group solve. *)
let target_test_request ~names ~req_with_repos ~pins ~all_extras
    ~extra_constraints ~toolchain ~conf ~local_packages_dir ~refresh :
    Oi.Build_pipeline.request =
  {
    targets =
      [
        Group
          { tokens = List.map OpamPackage.Name.to_string names; handles = [] };
      ];
    with_repos = req_with_repos;
    pins;
    extra_repos = all_extras;
    constraints = extra_constraints;
    toolchain_override = None;
    toolchain;
    conf;
    local_packages_dir;
    project_root = None;
    force_source = false;
    with_test = true;
    refresh;
  }

let solve_and_build_target_test ~fs ~proc_mgr ~clock ~sys ~os_key ~cache
    ~data_dir ~session ~conf ~layer_remote ~source_remote ~target ~names
    ~req_with_repos ~pins ~all_extras ~extra_constraints ~toolchain
    ~local_packages_dir ~refresh ~jobs =
  let pipeline_env : Oi.Build_pipeline.env =
    {
      proc_mgr;
      fs;
      clock;
      sys;
      os_key;
      cache;
      data_dir;
      http_session = session;
    }
  in
  let req =
    target_test_request ~names ~req_with_repos ~pins ~all_extras
      ~extra_constraints ~toolchain ~conf ~local_packages_dir ~refresh
  in
  Progress_ui.with_ui ~target
    ~clock:(clock :> _ Eio.Resource.t)
    ~enabled:(Tty.is_tty ())
  @@ fun reporter ->
  let solved = Oi.Build_pipeline.solve pipeline_env ~reporter req in
  let _ : D10ir.Direct.result option =
    Oi.Build_pipeline.build pipeline_env ~reporter
      {
        solved;
        layer_remote;
        source_remote;
        jobs;
        upload_archive_url = None;
        archive_sources = false;
        snapshot_reporepo = false;
      }
  in
  Oi.Build_pipeline.layer_hashes solved

let validate_test_build_dir ~target build_dir =
  if not (Sys.file_exists build_dir) then
    Oi.Error.fail_not_found target
      "build dir %s missing (layer was cached without preserved source). Pass \
       --refresh to rebuild from source."
      build_dir;
  if not (Sys.file_exists (build_dir / "dune-project")) then
    Oi.Error.fail_config_error
      "%s has no dune-project; native opam test commands not yet supported."
      build_dir

let exec_dune_runtest ~proc_mgr ~env ~build_dir =
  let cmd = Fmt.str "cd %s && dune runtest --profile=release" build_dir in
  let ec = Subprocess.run proc_mgr ~env [ "/bin/sh"; "-c"; cmd ] in
  if ec <> 0 then begin
    Fmt.epr "%a (dune runtest exit %d)@." Oi.Style.pp_error_string "Test failed"
      ec;
    ec
  end
  else begin
    Fmt.pr "%a@." Oi.Style.pp_ok_string "Test successful";
    0
  end

let run_target_test_after_build ~target ~fs ~proc_mgr ~clock ~sys ~cache ~os_key
    ~toolchain layer_hashes =
  match find_target_layer ~fs ~cache ~os_key ~pkg_name:target layer_hashes with
  | None ->
      Oi.Error.fail_not_found target
        "no built layer matched %s; the solve may have substituted a different \
         package."
        target
  | Some (layer_hash, pkg_full) ->
      let short = String.sub layer_hash 0 (min 12 (String.length layer_hash)) in
      let build_dir =
        Oi.Cache.root_s cache / "build" / "_build" / (pkg_full ^ "-" ^ short)
      in
      validate_test_build_dir ~target build_dir;
      let prefix =
        Oi.Pipeline.assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes
      in
      let dune_cache_root = Oi.Cache.dune_root cache in
      let tc_ctx = Option.map Oi.Toolchain.opam_ctx_of_info toolchain in
      let env =
        Oi.Solver.Env.make_env ?toolchain:tc_ctx ~prefix ~dune_cache_root ()
      in
      Fmt.pr "@.%a %s@.%a %s@." Oi.Style.pp_header_string "Testing" pkg_full
        Oi.Style.pp_dim_string "→" build_dir;
      exec_dune_runtest ~proc_mgr ~env ~build_dir

type target_test_solve_inputs = {
  inp : target_test_inputs;
  url_project : Oi.Project.Url.t;
  toolchain : Oi.Toolchain.info option;
  all_extras : Oi.Project.extra_repo list;
  extra_constraints : OpamFormula.version_constraint OpamPackage.Name.Map.t;
  names : OpamPackage.Name.t list;
}

let prepare_target_test_solve ~fs ~sys ~cache ~data_dir ~refresh ~conf ~target
    ~with_repos ~with_deps ~toolchain =
  let inp = parse_target_test_inputs ~target ~with_repos ~with_deps in
  let extra_deps, url_project =
    Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh inp.with_deps
  in
  let handle_pins = Stdlib.Option.to_list inp.target_pin @ inp.with_pins in
  let tc_handles =
    Target.pin_handles handle_pins
    @ Target.handles_of_tokens inp.with_repos
    @ url_project.overlays
    |> List.sort_uniq String.compare
  in
  let toolchain =
    Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf ~install:true
      ~override:toolchain ~handles:tc_handles ()
  in
  let cli_extras = Target.cli_extra_repos ~fs ~sys ?toolchain inp.with_repos in
  let all_extras =
    Target.merge_extras ~cli:cli_extras ~project:url_project.extra_repos
  in
  let handle_constraints =
    Target.handle_pin_constraints ~fs ~data_dir ~refresh ~cli_extras handle_pins
  in
  let extra_constraints =
    OpamPackage.Name.Map.union
      (fun a _ -> a)
      handle_constraints
      (Oi.Project.Script.constraints extra_deps)
  in
  let names =
    [ OpamPackage.Name.of_string inp.target ]
    |> Oi.Pipeline.strip_compiler_roots_for_override ~override:None ~toolchain
  in
  { inp; url_project; toolchain; all_extras; extra_constraints; names }

(* Build [target]'s closure, then run [dune runtest --profile=release]
   inside the target's persisted build dir against the assembled
   consumer prefix. Backs [oi build PKG --test] / [oi build @h/PKG
   --test]. *)
let run_target_test ~target ~fs ~proc_mgr ~clock ~sys ~platform ~os_key ~cache
    ~data_dir ~registry ~use_registry ~session ?(refresh = false)
    ?(with_repos = []) ?(with_deps = []) ?jobs ?toolchain ?(dry_run = false) ()
    =
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
  let conf =
    Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  let { Terms.layer_remote; source_remote } =
    Terms.remotes_of ~url:registry ~mode:use_registry
  in
  let ts =
    prepare_target_test_solve ~fs ~sys ~cache ~data_dir ~refresh ~conf ~target
      ~with_repos ~with_deps ~toolchain
  in
  if dry_run then begin
    print_dry_run_test_plan ts.inp.target_display;
    0
  end
  else
    let layer_hashes =
      solve_and_build_target_test ~fs ~proc_mgr ~clock ~sys ~os_key ~cache
        ~data_dir ~session ~conf ~layer_remote ~source_remote
        ~target:ts.inp.target ~names:ts.names ~req_with_repos:ts.inp.with_repos
        ~pins:ts.url_project.pins ~all_extras:ts.all_extras
        ~extra_constraints:ts.extra_constraints ~toolchain:ts.toolchain
        ~local_packages_dir:ts.url_project.packages_dir ~refresh ~jobs
    in
    run_target_test_after_build ~target:ts.inp.target ~fs ~proc_mgr ~clock ~sys
      ~cache ~os_key ~toolchain:ts.toolchain layer_hashes

(* -- Mirror sync helper (shared by --archives-only and --every-version) -- *)

let format_bytes n =
  let f = Int64.to_float n in
  if Int64.compare n 1_073_741_824L >= 0 then
    Fmt.str "%.2fGB" (f /. 1_073_741_824.)
  else if Int64.compare n 1_048_576L >= 0 then Fmt.str "%.1fMB" (f /. 1_048_576.)
  else if Int64.compare n 1_024L >= 0 then Fmt.str "%.1fKB" (f /. 1_024.)
  else Fmt.str "%LdB" n

(* Fetch [archives] into the local mirror, with a single throttled
   progress line and a one-line summary. Returns 0 if every fetch
   succeeded (or was already cached), 1 if any failed — the
   warn-and-continue contract documented for [oi build --archives-only]. *)
let mirror_archives ~fs ~cache ~label archives =
  let archives = Oi.Source.Mirror.dedup_by_url archives in
  let total = List.length archives in
  Oi.Say.step "Mirroring %d source archive(s) (%s)" total label;
  let last_msg = ref "" in
  let on_progress ~fetched ~total ~current =
    let msg =
      match current with
      | None -> Fmt.str "fetched %d/%d" fetched total
      | Some c -> Fmt.str "fetched %d/%d  %s" fetched total c
    in
    if msg <> !last_msg then begin
      last_msg := msg;
      Oi.Say.progress msg
    end
  in
  let summary =
    Oi.Source.Mirror.fetch_archives ~fs ~cache ~on_progress archives
  in
  Oi.Say.progress_clear ();
  Oi.Say.step "Mirror sync complete";
  Oi.Say.info "fetched: %d  cached: %d  failed: %d  added: %s" summary.fetched
    summary.cached
    (List.length summary.failed)
    (format_bytes summary.bytes_added);
  List.iter
    (fun (url, msg) -> Oi.Say.warn "fetch failed %s: %s" url msg)
    summary.failed;
  if summary.failed = [] then 0 else 1

(* -- oi build helpers --------------------------------------------------- *)

(* Per-run accumulators populated inside [Progress_ui.with_ui] and read
   from the post-bar summary block on a clean terminal. Threaded by
   reference so the bucket loop can mutate as solves finish. *)
type acc = {
  mutable targets : string list;
  target_handle : (string, string) Hashtbl.t;
  solve_failures : (string, string * string) Hashtbl.t;
  target_group : (string, int) Hashtbl.t;
  group_results : (int, group_outcome) Hashtbl.t;
  mutable dist_mapping : (string * string * string) list;
}

let new_acc () =
  {
    targets = [];
    target_handle = Hashtbl.create 16;
    solve_failures = Hashtbl.create 8;
    target_group = Hashtbl.create 16;
    group_results = Hashtbl.create 16;
    dist_mapping = [];
  }

let detect_project_mode ~fs ~skip_local ~targets ~all =
  let cwd_s, _ = Workspace.resolved_cwd fs in
  let in_project =
    (not skip_local) && targets = [] && (not all)
    &&
      try
        Sys.readdir cwd_s
        |> Array.exists (fun f ->
            Filename.check_suffix f ".opam"
            && Filename.chop_suffix f ".opam" <> "")
      with Sys_error _ -> false
  in
  (in_project, cwd_s)

(* [oi build script.ml]: make the directory a real dune project so the
   user's editor / LSP works on the script in place AND [oi build] has an
   opam file to solve. Same dep-parsing logic as [oi run] (the first-line
   [[@@@opam …]] annotation); the script is left as-is (not copied to
   [main.ml]) and existing project files are never overwritten — we just
   say so. Returns [true] when the single target is a local [.ml] script
   (so the caller switches to a project build of the cwd), [false]
   otherwise. *)
let maybe_scaffold_script ~fs ~cwd_s ~targets =
  match targets with
  | [ t ] when Filename.check_suffix t ".ml" ->
      let path =
        if Filename.is_relative t then Filename.concat cwd_s t else t
      in
      if Sys.file_exists path && not (Sys.is_directory path) then begin
        let deps = Oi.Project.Script.parse_deps_from_file ~fs path in
        let created, kept =
          Oi.Project.Script.scaffold_for_script ~script:path ~deps ~dir:cwd_s
        in
        if created <> [] then
          Fmt.pr
            "%a scaffolded %s for %s — your editor / LSP and `oi build` can \
             now use this directory.@."
            Oi.Style.pp_ok_string "▸"
            (String.concat ", " created)
            (Filename.basename t);
        if kept <> [] then
          Fmt.pr
            "%a kept existing %s in %s — left untouched (oi won't overwrite \
             project files).@."
            Oi.Style.pp_accent_string "▸" (String.concat ", " kept) cwd_s;
        true
      end
      else false
  | _ -> false

(* Flag validation. Mode-specific dispatch happens after this; here we
   only reject combinations that can't possibly proceed: any flag whose
   result depends on solving when there's nothing to solve. *)
let validate_build_flags ~targets ~all ~project_mode ~depext_only ~archives_only
    ~every_version =
  let needs_spec what =
    Oi.Error.fail_config_error
      "oi build %s: no spec and no project (cwd has no *.opam). Pass a PKG, \
       @HANDLE, or --all."
      what
  in
  if archives_only && depext_only then
    Oi.Error.fail_config_error
      "oi build --archives-only: cannot combine with --depext (no build runs, \
       so there's nothing to depext)";
  let no_spec = targets = [] && (not all) && not project_mode in
  if no_spec && depext_only then needs_spec "--depext";
  if no_spec && archives_only then needs_spec "--archives-only";
  if every_version && not archives_only then
    Oi.Error.fail_config_error
      "oi build --every-version: only valid with --archives-only (it skips the \
       solver and walks every recorded reporepo opam file)"

let collect_every_version_archives ~fs ~sys ~refresh ~only ~skip =
  let path = Terms.reporepo_path () in
  Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh ~path
    ~url:(Terms.reporepo_url ()) ();
  let seen : (string, unit) Hashtbl.t = Hashtbl.create 4096 in
  let acc = ref [] in
  Oi.Source.Reporepo.iter_opam_files ~path ~include_handles:only
    ~skip_handles:skip (fun ~handle ~pkg ~version ~opam_path ->
      let pkg_label = Fmt.str "@%s/%s.%s" handle pkg version in
      Oi.Source.Mirror.archives_of_opam_file ~path:opam_path ~pkg:pkg_label
      |> List.iter (fun (a : Oi.Source.Mirror.archive) ->
          let key = OpamUrl.to_string a.url in
          if not (Hashtbl.mem seen key) then begin
            Hashtbl.add seen key ();
            acc := a :: !acc
          end));
  List.rev !acc

let run_project_mode ~fs ~proc_mgr ~clock ~sys ~platform ~os_key ~cache
    ~data_dir ~http_session ~registry ~use_registry ~refresh ~with_repos
    ~with_deps ~jobs ~toolchain_override ~depext_only ~envrc_mode ~dist ~cwd_s =
  if depext_only then
    Project_build.depexts ~fs ~sys ~platform ~cache ~data_dir ~refresh
      ~with_repos ~with_deps ?toolchain:toolchain_override ~cwd:cwd_s ()
  else
    Project_build.run ~action:`Build ~fs ~proc_mgr ~clock ~sys ~platform ~os_key
      ~cache ~data_dir ~registry ~use_registry ~session:http_session ~refresh
      ~with_repos ~with_deps ?jobs ?toolchain:toolchain_override ~envrc_mode
      ?dist ~cwd:cwd_s ()

(* When [--all] is set, walk every overlay in the reporepo and derive
   targets from each one:
   - skip [default] (ocaml/opam-repository) — its ~10k packages
     are never what [--all] should mean;
   - skip toolchain-definition entries ([x-oi-toolchain-name]
     set, url-less) — they're metadata views over other overlays,
     not buildable themselves;
   - if the overlay has [x-root-packages], emit one [@handle/pkg]
     per entry;
   - otherwise fall back to [@handle], which expands to every
     package the overlay's clone ships.
   [--only] restricts to named handles; [--skip] excludes them.
   [default] can still be included by explicitly listing it via
   [--only default]. *)
let handle_default_skipped ~only_set h =
  h = "default"
  && match only_set with None -> true | Some s -> not (List.mem h s)

let handle_included ~only_set ~skip_set h =
  (match only_set with None -> true | Some s -> List.mem h s)
  && not (List.mem h skip_set)

let groups_of_entry h (e : Oi.Source.Reporepo.entry) =
  if e.toolchain_name <> None then begin
    Log.info (fun m -> m "--all: skipping toolchain definition %s" h);
    []
  end
  else if e.root_packages = [] then begin
    Log.info (fun m ->
        m
          "--all: overlay %s has no x-root-packages, expanding to every \
           package in the overlay"
          h);
    [ [ "@" ^ h ] ]
  end
  else
    List.map
      (fun group -> List.map (fun p -> "@" ^ h ^ "/" ^ p) group)
      e.root_packages

let groups_for_handle ~entries ~only_set ~skip_set h =
  if handle_default_skipped ~only_set h then begin
    Log.info (fun m ->
        m "--all: skipping %s (pass --only default to include)" h);
    []
  end
  else if not (handle_included ~only_set ~skip_set h) then []
  else
    match Oi.Source.Reporepo.latest entries ~handle:h with
    | None -> []
    | Some e -> groups_of_entry h e

let expand_reporepo_target_groups ~fs ~sys ~refresh ~only ~skip =
  let path = Terms.reporepo_path () in
  Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh ~path
    ~url:(Terms.reporepo_url ()) ();
  let entries = Oi.Source.Reporepo.load ~path in
  let only_set =
    if only = [] then None else Some (List.sort_uniq compare only)
  in
  let skip_set = List.sort_uniq compare skip in
  let handles =
    List.map (fun (e : Oi.Source.Reporepo.entry) -> e.handle) entries
    |> List.sort_uniq String.compare
  in
  List.concat_map (groups_for_handle ~entries ~only_set ~skip_set) handles

(* Excluded names = packages owned by any reporepo toolchain
   definition. Skipped during [@handle] expansion so a non-relocatable
   toolchain's compiler doesn't get baked twice. *)
let spec_name spec =
  match String.index_opt spec '.' with
  | None -> spec
  | Some i -> String.sub spec 0 i

let toolchain_pkg_names_of_entry ~names (e : Oi.Source.Reporepo.entry) =
  if e.toolchain_name = None then ()
  else begin
    Stdlib.Option.iter
      (fun s -> names := spec_name s :: !names)
      e.toolchain_compiler;
    List.iter
      (fun group -> List.iter (fun s -> names := spec_name s :: !names) group)
      e.toolchain_roots
  end

let toolchain_pkg_names entries =
  let names = ref [] in
  List.iter (toolchain_pkg_names_of_entry ~names) entries;
  List.sort_uniq String.compare !names

let overlay_packages handle =
  let path = Terms.reporepo_path () in
  let entries = Oi.Source.Reporepo.load ~path in
  match Oi.Source.Reporepo.latest entries ~handle with
  | None -> Oi.Error.fail_config_error "no overlay %s in reporepo" handle
  | Some e ->
      let pkgs_dir =
        Oi.Source.Reporepo.overlay_packages_dir ~path ~handle:e.handle
      in
      if not (Sys.file_exists pkgs_dir) then
        Oi.Error.fail_config_error
          "overlay %s.%s is not materialised at %s; run 'oi repo bump %s' to \
           populate it (upstream %s)"
          handle e.version pkgs_dir handle e.url;
      let tc_names = toolchain_pkg_names entries in
      let all_names =
        Sys.readdir pkgs_dir |> Array.to_list
        |> List.filter (fun n -> Sys.is_directory (pkgs_dir / n))
        |> List.sort String.compare
      in
      let kept, dropped =
        List.partition (fun n -> not (List.mem n tc_names)) all_names
      in
      if dropped <> [] then
        Log.info (fun m ->
            m
              "Overlay %s: excluding %d toolchain package(s) from --all \
               expansion: %s"
              handle (List.length dropped)
              (String.concat ", " dropped));
      kept

(* Expand each raw group into package-name groups. A group containing
   just an [@handle] fallback (no [x-root-packages]) fans out into one
   singleton group per package the overlay ships. Groups composed of
   [@handle/pkg] or plain [pkg] tokens keep their shape so multi-package
   solve groups (compiler variants) survive intact. *)
let expand_raw_target_group ~target_handle raw_group =
  match List.map Target.parse_build_target raw_group with
  | [ Overlay_all h ] ->
      let ps = overlay_packages h in
      List.iter (fun p -> Hashtbl.replace target_handle p h) ps;
      Log.info (fun m ->
          m "Overlay %s: %d package(s) to build" h (List.length ps));
      List.map (fun p -> ([ p ], [ h ])) ps
  | classified ->
      let names =
        List.map
          (function
            | Target.Plain_target t -> t
            | Target.Overlay_pkg (h, pkg_spec) ->
                Hashtbl.replace target_handle pkg_spec h;
                pkg_spec
            | Target.Overlay_all h ->
                Oi.Error.fail_config_error
                  "@%s cannot appear inside a multi-package solve group; use \
                   @%s/PKG or list packages explicitly"
                  h h)
          classified
      in
      let handles =
        List.filter_map
          (function
            | Target.Plain_target _ -> None
            | Target.Overlay_pkg (h, _) | Target.Overlay_all h -> Some h)
          classified
        |> List.sort_uniq String.compare
      in
      [ (names, handles) ]

(* Group items by an optional string key, preserving input order
   within each bucket. Items with [key x = None] end up under the
   [None] bucket; keyed buckets sort alphabetically. *)
let partition_by ~key xs =
  let by_k : (string, _ list) Hashtbl.t = Hashtbl.create 4 in
  let none_bucket = ref [] in
  List.iter
    (fun x ->
      match key x with
      | Some k ->
          let cur = Stdlib.Option.value (Hashtbl.find_opt by_k k) ~default:[] in
          Hashtbl.replace by_k k (x :: cur)
      | None -> none_bucket := x :: !none_bucket)
    xs;
  let keyed =
    Hashtbl.fold (fun k vs acc -> (Some k, List.rev vs) :: acc) by_k []
    |> List.sort (fun (a, _) (b, _) -> compare a b)
  in
  match !none_bucket with [] -> keyed | xs -> (None, List.rev xs) :: keyed

let compute_buckets ~all ~toolchain_override ~depext_only ~archives_only
    ~reporepo_entries target_groups =
  let toolchain_of_handles handles =
    match handles with
    | [] -> None
    | h :: _ -> (
        match
          Oi.Pipeline.toolchain_names_of_handle (Lazy.force reporepo_entries) h
        with
        | [ n ] -> Some n
        | _ -> None)
  in
  let split_for_all =
    all && toolchain_override = None && (not depext_only) && not archives_only
  in
  if not split_for_all then [ (toolchain_override, target_groups) ]
  else
    let buckets =
      partition_by
        ~key:(fun (_, handles) -> toolchain_of_handles handles)
        target_groups
    in
    if List.length buckets <= 1 then [ (toolchain_override, target_groups) ]
    else begin
      let names =
        List.filter_map (fun (tc, _) -> tc) buckets |> List.sort String.compare
      in
      Log.info (fun m ->
          m
            "--all: splitting solve into %d toolchain bucket(s) (%s); pass \
             --toolchain=NAME to force a single bucket"
            (List.length buckets) (String.concat ", " names));
      buckets
    end

(* -- Per-bucket post-solve helpers -------------------------------------- *)

let audit_solve_failure ~fs ~cache ~os_key (gr : Oi.Build_pipeline.group_result)
    msg log_path =
  let layer_hash =
    let key =
      String.concat " "
        (gr.group.tokens @ List.map (fun h -> "@" ^ h) gr.group.handles)
    in
    Digest.to_hex (Digest.string key)
  in
  let tail = Oi.Audit.tail_of_file ~path:log_path () in
  let now = Unix.gettimeofday () in
  let context : Oi.Audit.context =
    {
      (Oi.Audit.default_context ()) with
      overlay =
        (match gr.group.handles with
        | [ h ] -> Some { D10.Overlay.handle = h; version = "" }
        | _ -> None);
      toolchain =
        Option.map (fun (i : Oi.Toolchain.info) -> i.handle) gr.toolchain;
    }
  in
  List.iter
    (fun target ->
      let event : Oi.Audit.event =
        {
          schema = 1;
          event_id = Oi.Audit.ulid ();
          invocation_id = Oi.Audit.invocation_id ();
          ts = now;
          os_key;
          target = Solve_key layer_hash;
          pkg = Oi.Identity.of_string target;
          outcome = Solve_failed { reason = msg };
          duration_s = 0.0;
          context;
          log = Some { text_path = log_path; tail };
        }
      in
      Oi.Audit.append ~fs ~cache_root:(Oi.Cache.root_s cache) event)
    gr.group.tokens

let record_solve_results ~fs ~cache ~os_key ~acc ~gi_offset
    (solved : Oi.Build_pipeline.solved) =
  List.iteri
    (fun gi (gr : Oi.Build_pipeline.group_result) ->
      let gi = gi + gi_offset in
      List.iter (fun t -> Hashtbl.replace acc.target_group t gi) gr.group.tokens;
      match gr.error with
      | Error (Solve_failed { msg; log_path }) ->
          List.iter
            (fun t -> Hashtbl.replace acc.solve_failures t (msg, log_path))
            gr.group.tokens;
          audit_solve_failure ~fs ~cache ~os_key gr msg log_path
      | _ -> ())
    solved.groups

let do_depext_per_bucket ~conf (solved : Oi.Build_pipeline.solved) =
  let group_conf, _ = Oi.Pipeline.solver_inputs solved.toolchain conf in
  let all =
    List.fold_left
      (fun acc (gr : Oi.Build_pipeline.group_result) ->
        if not (Result.is_ok gr.error) then acc
        else
          let entries =
            Oi.Depexts.compute_for_conf ~conf:group_conf
              ~packages_dirs:gr.pkgs_dir gr.pkgs
          in
          List.fold_left
            (fun acc e -> OpamSysPkg.Set.union acc e.Oi.Depexts.sys_pkgs)
            acc entries)
      OpamSysPkg.Set.empty solved.groups
  in
  OpamSysPkg.Set.iter (fun p -> Fmt.pr "%s@." (OpamSysPkg.to_string p)) all

let do_archives_only_per_bucket ~fs ~cache (solved : Oi.Build_pipeline.solved) =
  let archives =
    List.concat_map
      (fun (gr : Oi.Build_pipeline.group_result) ->
        if not (Result.is_ok gr.error) then []
        else
          Oi.Source.Mirror.collect_archives ~packages_dirs:gr.pkgs_dir gr.pkgs)
      solved.groups
  in
  mirror_archives ~fs ~cache ~label:"solved" archives

let record_cycle_failures ~acc ~gi_offset (solved : Oi.Build_pipeline.solved) =
  List.iteri
    (fun gi (gr : Oi.Build_pipeline.group_result) ->
      let gi = gi + gi_offset in
      match gr.error with
      | Error (Cycle cycles) ->
          let cycle_label =
            Fmt.str "%a" Oi.Plan.pp_cycles cycles |> String.trim
          in
          Log.warn (fun m ->
              m
                "Skipping group %s: dependency cycle in solved set:@\n\
                 %s@\n\
                 This is an upstream metadata bug — the cyclic packages all \
                 declare each other as [{= version}] dependencies. Re-bump the \
                 offending overlay once the upstream opam files are fixed."
                gr.group.label cycle_label);
          let n_pkgs = List.length gr.pkgs in
          Hashtbl.replace acc.group_results gi
            (Group_failed
               {
                 counts = { n_pkgs; n_built = 0; n_cached = 0 };
                 msg = Fmt.str "dependency cycle: %s" cycle_label;
                 per_pkg_logs = [];
               })
      | _ -> ())
    solved.groups

let recipe_stem token =
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

let save_d10ir_recipes ~fs ~dir (solved : Oi.Build_pipeline.solved) =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dir);
  List.iter
    (fun (gr : Oi.Build_pipeline.group_result) ->
      match gr.recipe with
      | None -> ()
      | Some recipe ->
          let stem =
            match gr.group.tokens with
            | t :: _ -> recipe_stem t
            | [] -> "recipe"
          in
          let dst = Filename.concat dir (stem ^ ".d10ir.json") in
          D10ir.Plan.save Eio.Path.(fs / dst) recipe;
          Logs.info (fun m -> m "Saved d10ir recipe to %s" dst))
    solved.groups

let audit_cached_layer ~fs ~cache ~os_key ~toolchain_handle ~audit_now ~pkgs_dir
    (p : Oi.Plan.package_plan) =
  if p.method_ <> Oi.Identity.Binary then ()
  else
    let opam_pkg =
      match OpamPackage.of_string_opt p.pkg with
      | Some op -> op
      | None -> OpamPackage.of_string (p.pkg ^ ".0.0")
    in
    let context : Oi.Audit.context =
      {
        (Oi.Audit.default_context ()) with
        overlay = Oi.Plan.overlay_of_pkg ~packages_dirs:pkgs_dir opam_pkg;
        toolchain = toolchain_handle;
      }
    in
    let event : Oi.Audit.event =
      {
        schema = 1;
        event_id = Oi.Audit.ulid ();
        invocation_id = Oi.Audit.invocation_id ();
        ts = audit_now;
        os_key;
        target = Layer p.layer_hash;
        pkg = Oi.Identity.of_opam opam_pkg;
        outcome = Cached;
        duration_s = 0.0;
        context;
        log = None;
      }
    in
    Oi.Audit.append ~fs ~cache_root:(Oi.Cache.root_s cache) event

let audit_cached_layers ~fs ~cache ~os_key (solved : Oi.Build_pipeline.solved) =
  let audit_now = Unix.gettimeofday () in
  let toolchain_handle =
    Option.map (fun (i : Oi.Toolchain.info) -> i.handle) solved.toolchain
  in
  List.iter
    (fun (gr : Oi.Build_pipeline.group_result) ->
      if not (Result.is_ok gr.error) then ()
      else
        match gr.exec_plan with
        | None -> ()
        | Some xp ->
            List.iter
              (audit_cached_layer ~fs ~cache ~os_key ~toolchain_handle
                 ~audit_now ~pkgs_dir:gr.pkgs_dir)
              xp.packages)
    solved.groups

let collect_group_failures (exec_plan : Oi.Plan.t)
    (result : D10ir.Direct.result) =
  let pkg_set : (string, unit) Hashtbl.t = Hashtbl.create 32 in
  List.iter
    (fun (p : Oi.Plan.package_plan) -> Hashtbl.replace pkg_set p.pkg ())
    exec_plan.packages;
  List.filter
    (fun (f : D10ir.Direct.failure) ->
      let pkg_str = Fmt.str "%s.%s" f.package.name f.package.version in
      Hashtbl.mem pkg_set pkg_str)
    result.failures

let record_failed_layers ~failed_layers (exec_plan : Oi.Plan.t) group_failures =
  List.iter
    (fun (f : D10ir.Direct.failure) ->
      let pkg_str = Fmt.str "%s.%s" f.package.name f.package.version in
      List.iter
        (fun (p : Oi.Plan.package_plan) ->
          if p.pkg = pkg_str then
            Hashtbl.replace failed_layers p.layer_hash f.log_path)
        exec_plan.packages)
    group_failures

let collect_per_pkg_logs ~failed_layers (exec_plan : Oi.Plan.t) =
  let seen = Hashtbl.create 16 in
  List.filter_map
    (fun (p : Oi.Plan.package_plan) ->
      match Hashtbl.find_opt failed_layers p.layer_hash with
      | Some path when path <> "" && not (Hashtbl.mem seen path) ->
          Hashtbl.replace seen path ();
          Some (p.pkg, path)
      | _ -> None)
    exec_plan.packages

let group_outcome_of (exec_plan : Oi.Plan.t) ~failed_layers group_failures =
  let count_by f = List.length (List.filter f exec_plan.packages) in
  let n_build =
    count_by (fun (p : Oi.Plan.package_plan) -> p.method_ = Source)
  in
  let n_cached =
    count_by (fun (p : Oi.Plan.package_plan) -> p.method_ = Binary)
  in
  let counts = { n_pkgs = n_build + n_cached; n_built = n_build; n_cached } in
  if group_failures = [] then Group_ok counts
  else
    let pkg_summary =
      match group_failures with
      | [ (f : D10ir.Direct.failure) ] ->
          Fmt.str "package %s.%s in phase %s" f.package.name f.package.version
            (D10ir.Direct.string_of_phase f.phase)
      | fs -> Fmt.str "%d packages" (List.length fs)
    in
    let output = Fmt.str "%a" D10ir.Direct.pp_failures group_failures in
    Group_failed
      {
        counts;
        msg = Fmt.str "build failed: %s\n%s" pkg_summary output;
        per_pkg_logs = collect_per_pkg_logs ~failed_layers exec_plan;
      }

let record_one_group_result ~acc ~failed_layers ~gi_offset ~result gi
    (gr : Oi.Build_pipeline.group_result) =
  let gi = gi + gi_offset in
  match gr.error with
  | Error (Cycle _) | Error _ -> ()
  | Ok () -> (
      match gr.exec_plan with
      | None -> ()
      | Some exec_plan ->
          let group_failures = collect_group_failures exec_plan result in
          record_failed_layers ~failed_layers exec_plan group_failures;
          let outcome =
            group_outcome_of exec_plan ~failed_layers group_failures
          in
          Hashtbl.replace acc.group_results gi outcome)

let record_build_results ~acc ~gi_offset (solved : Oi.Build_pipeline.solved) =
  function
  | None -> ()
  | Some (result : D10ir.Direct.result) ->
      let failed_layers : (string, string) Hashtbl.t = Hashtbl.create 64 in
      List.iteri
        (record_one_group_result ~acc ~failed_layers ~gi_offset ~result)
        solved.groups

(* [--dist=DIR]: copy [bin/] + [sbin/] + [share/] from each root
   layer's fs prefix into [DIR/{bin,sbin,share}/]. Skips roots whose
   layer never materialised; silently no-ops when [dist] is unset. *)
let collect_dist_artifacts ~acc ~cache_root ~os_key (dist, result_opt)
    (solved : Oi.Build_pipeline.solved) =
  match (dist, result_opt) with
  | Some dir, Some _ ->
      (try Unix.mkdir dir 0o755 with Unix.Unix_error (EEXIST, _, _) -> ());
      List.iter
        (fun h ->
          let layer_fs = cache_root / "layers" / os_key / h / "fs" in
          if Sys.file_exists layer_fs then
            let added = Dist.collect_install ~root:layer_fs ~dst:dir in
            acc.dist_mapping <- acc.dist_mapping @ added)
        (Oi.Build_pipeline.root_layer_hashes solved)
  | _ -> ()

let print_fetch_logs ~cache_root ~run_start_time =
  let logs_dir = Oi.Cache.Logs.dir ~cache_root in
  if not (Sys.file_exists logs_dir) then ()
  else
    let written_this_run p =
      try (Unix.stat p).Unix.st_mtime >= run_start_time
      with Unix.Unix_error _ -> false
    in
    let entries =
      try
        Sys.readdir logs_dir |> Array.to_list
        |> List.filter (fun n -> String.starts_with ~prefix:"fetch-" n)
        |> List.map (fun n -> logs_dir / n)
        |> List.filter written_this_run
        |> List.sort String.compare
      with Sys_error _ -> []
    in
    if entries <> [] then begin
      Fmt.pr "@.%a (%d):@." Oi.Style.pp_dim_string "transient fetch errors"
        (List.length entries);
      List.iter (fun p -> Fmt.pr "  %s@." p) entries
    end

let print_dist_artifacts dist_mapping =
  if dist_mapping = [] then ()
  else begin
    let by_sub = Hashtbl.create 4 in
    List.iter
      (fun (sub, _, _) ->
        Hashtbl.replace by_sub sub
          (1 + Stdlib.Option.value (Hashtbl.find_opt by_sub sub) ~default:0))
      dist_mapping;
    Fmt.pr "@.%a@." Oi.Style.pp_header_string "Dist artifacts:";
    List.iter
      (fun (_, name, dst) ->
        Fmt.pr "  %s %a %s@." name Oi.Style.pp_dim_string "→" dst)
      (List.filter
         (fun (sub, _, _) -> sub = "bin" || sub = "sbin")
         dist_mapping);
    match Hashtbl.find_opt by_sub "share" with
    | Some n when n > 0 ->
        Fmt.pr "  share/ %a %a@." Oi.Style.pp_dim_string
          (Fmt.str "(%d files)" n) Oi.Style.pp_dim_string "—"
    | _ -> ()
  end

(* Inputs shared by every bucket within one build invocation; computed
   once before the bucket loop and passed in to keep [run_one_bucket]
   from carrying 15+ labelled arguments. *)
type bucket_inputs = {
  pipeline_env : Oi.Build_pipeline.env;
  conf : Oi.Solver.Ctx.conf;
  layer_remote : D10.Layer.remote option;
  source_remote : D10.Layer.remote option;
  global_handles : string list;
  base_constraints : OpamFormula.version_constraint OpamPackage.Name.Map.t;
  extra_token_names : string list;
  pin_dir : string option;
  cache_root : string;
  archive_sources : bool;
  snapshot_reporepo : bool;
}

let bucket_req ~bi ~refresh ~override tgs : Oi.Build_pipeline.request =
  let bp_targets : Oi.Build_pipeline.target list =
    List.map
      (fun (toks, handles) : Oi.Build_pipeline.target ->
        Group { tokens = toks @ bi.extra_token_names; handles })
      tgs
  in
  {
    targets = bp_targets;
    with_repos = bi.global_handles;
    pins = [];
    extra_repos = [];
    constraints = bi.base_constraints;
    toolchain_override = override;
    toolchain = None;
    conf = bi.conf;
    local_packages_dir = bi.pin_dir;
    project_root = None;
    force_source = false;
    with_test = false;
    refresh;
  }

let do_depext_all ~fs ~sys ~cache ~data_dir ~refresh ~platform
    ~toolchain_override =
  let conf =
    Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  let names =
    compute_overlay_depexts_for_conf ~fs ~sys ~cache ~data_dir ~refresh ~conf
      ?override:toolchain_override ()
  in
  List.iter (fun n -> Fmt.pr "%s@." n) names

let target_label_of ~all targets =
  if all then "all"
  else match targets with [] -> "." | xs -> String.concat ", " xs

let fail_no_tokens ~all =
  if all then
    Oi.Error.fail_config_error
      "--all expanded to nothing in %s (all overlays filtered by \
       --skip/--only, or the reporepo only contains 'default')"
      (Terms.reporepo_path ())
  else
    Oi.Error.fail_config_error
      "no targets to build (pass PKG arguments or --all)"

let handles_of_parsed parsed =
  List.filter_map
    (function
      | Target.Plain_target _ -> None
      | Target.Overlay_pkg (h, _) | Target.Overlay_all h -> Some h)
    parsed
  |> List.sort_uniq String.compare

let extra_names_of_cli extra_cli (url_project : Oi.Project.Url.t) =
  List.filter_map
    (fun (d : Oi.Project.Script.dep) ->
      if OpamPackage.Name.to_string d.name = "ocaml" then None else Some d.name)
    extra_cli
  @ List.map OpamPackage.Name.of_string url_project.roots

(* Materialise the per-bucket inputs once inside [Progress_ui.with_ui].
   [acc] accumulates per-run results; [bi] carries the inputs every
   bucket shares (solver env, conf, remotes, handles, constraints). *)
(* Token-level classification: expand [--all] into reporepo groups,
   gather every overlay handle so the upcoming clone pre-pass sees them,
   then split into [global_handles] (apply to every solve) vs
   [token_handles] (per-group). *)
type token_classification = {
  token_groups : string list list;
  global_handles : string list;
  url_project : Oi.Project.Url.t;
  extra_cli : Oi.Project.Script.dep list;
  parsed : Target.build_target list;
}

let classify_build_tokens ~fs ~sys ~cache ~refresh ~with_repos ~with_deps
    ~ui_reporter ~all ~only ~skip targets =
  let reporepo_target_groups =
    if not all then []
    else expand_reporepo_target_groups ~fs ~sys ~refresh ~only ~skip
  in
  let token_groups =
    List.map (fun t -> [ t ]) targets @ reporepo_target_groups
  in
  let tokens = List.concat token_groups in
  if tokens = [] then fail_no_tokens ~all;
  let parsed = List.map Target.parse_build_target tokens in
  let with_repos =
    with_repos @ handles_of_parsed parsed |> List.sort_uniq String.compare
  in
  let extra_cli, url_project =
    Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh
      ~reporter:ui_reporter with_deps
  in
  let token_handles = handles_of_parsed parsed in
  let global_handles =
    let tks = List.sort_uniq String.compare token_handles in
    List.filter (fun h -> not (List.mem h tks)) with_repos
    @ url_project.overlays
  in
  { token_groups; global_handles; url_project; extra_cli; parsed }

let materialise_handles_and_pins ~fs ~sys ~cache ~data_dir ~refresh tk =
  let token_handles = handles_of_parsed tk.parsed in
  let all_handles =
    List.sort_uniq String.compare (tk.global_handles @ token_handles)
  in
  let cli_extras_records =
    Target.merge_extras
      ~cli:(Target.cli_extra_repos ~fs ~sys all_handles)
      ~project:tk.url_project.extra_repos
  in
  let _ : string list =
    Oi.Source.Repo.ensure_many ~fs ~data_dir ~refresh cli_extras_records
  in
  Oi.Source.Pin.materialize ~fs ~sys ~cache ~refresh tk.url_project.pins

let target_groups_of ~acc tk =
  let raw_target_groups =
    List.concat_map
      (expand_raw_target_group ~target_handle:acc.target_handle)
      tk.token_groups
  in
  let pkg_targets = List.concat_map fst raw_target_groups in
  if pkg_targets = [] && tk.url_project.roots = [] then
    Oi.Error.fail_config_error "no targets to build";
  raw_target_groups @ List.map (fun r -> ([ r ], [])) tk.url_project.roots

let prepare_buckets ~fs ~sys ~cache ~data_dir ~refresh ~platform ~registry
    ~use_registry ~with_repos ~with_deps ~toolchain_override ~depext_only
    ~archives_only ~ui_reporter ~proc_mgr ~clock ~os_key ~http_session ~acc ~all
    ~only ~skip ~cache_root targets =
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  ignore
    (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh
       ~reporter:ui_reporter ());
  let conf =
    Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  let { Terms.layer_remote; source_remote } =
    Terms.remotes_of ~url:registry ~mode:use_registry
  in
  let tk =
    classify_build_tokens ~fs ~sys ~cache ~refresh ~with_repos ~with_deps
      ~ui_reporter ~all ~only ~skip targets
  in
  let pin_dir =
    materialise_handles_and_pins ~fs ~sys ~cache ~data_dir ~refresh tk
  in
  let target_groups = target_groups_of ~acc tk in
  acc.targets <- List.concat_map fst target_groups;
  let reporepo_entries =
    lazy
      (try Oi.Source.Reporepo.load ~path:(Terms.reporepo_path ())
       with Sys_error _ | Failure _ -> [])
  in
  let buckets =
    compute_buckets ~all ~toolchain_override ~depext_only ~archives_only
      ~reporepo_entries target_groups
  in
  let extra_names = extra_names_of_cli tk.extra_cli tk.url_project in
  let bi : bucket_inputs =
    {
      pipeline_env =
        { proc_mgr; fs; clock; sys; os_key; cache; data_dir; http_session };
      conf;
      layer_remote;
      source_remote;
      global_handles = tk.global_handles;
      base_constraints = Oi.Project.Script.constraints tk.extra_cli;
      extra_token_names = List.map OpamPackage.Name.to_string extra_names;
      pin_dir;
      cache_root;
      archive_sources = false;
      snapshot_reporepo = false;
    }
  in
  (bi, buckets)

let run_one_bucket ~fs ~cache ~os_key ~ui_reporter ~bi ~acc ~refresh
    ~depext_only ~archives_only ~save_d10ir ~dist ~jobs ~upload_archive
    ~gi_offset_ref (override, bucket_groups) =
  let gi_offset = !gi_offset_ref in
  let req = bucket_req ~bi ~refresh ~override bucket_groups in
  let solved =
    Oi.Build_pipeline.solve bi.pipeline_env ~reporter:ui_reporter req
  in
  record_solve_results ~fs ~cache ~os_key ~acc ~gi_offset solved;
  let any_solved =
    List.exists
      (fun (gr : Oi.Build_pipeline.group_result) -> Result.is_ok gr.error)
      solved.groups
  in
  if not any_solved then Oi.Error.fail_msg "no packages solved successfully";
  if depext_only then begin
    do_depext_per_bucket ~conf:bi.conf solved;
    exit 0
  end;
  if archives_only then exit (do_archives_only_per_bucket ~fs ~cache solved);
  record_cycle_failures ~acc ~gi_offset solved;
  (match save_d10ir with
  | None -> ()
  | Some dir -> save_d10ir_recipes ~fs ~dir solved);
  audit_cached_layers ~fs ~cache ~os_key solved;
  let result_opt =
    Oi.Build_pipeline.build bi.pipeline_env ~reporter:ui_reporter
      {
        solved;
        layer_remote = bi.layer_remote;
        source_remote = bi.source_remote;
        jobs;
        upload_archive_url = upload_archive;
        archive_sources = bi.archive_sources;
        snapshot_reporepo = bi.snapshot_reporepo;
      }
  in
  record_build_results ~acc ~gi_offset solved result_opt;
  collect_dist_artifacts ~acc ~cache_root:bi.cache_root ~os_key
    (dist, result_opt) solved;
  gi_offset_ref := !gi_offset_ref + List.length solved.groups

(* -- oi build dispatcher ------------------------------------------------ *)

(* Capture every CLI input into a record so [run] sub-helpers
   accept one parameter instead of 20+. [refresh] and [use_registry]
   are pre-normalised against [--locked]. *)
type args = {
  refresh : bool;
  skip_local : bool;
  all : bool;
  only : string list;
  skip : string list;
  registry : string;
  use_registry : Oi.Use_registry.t;
  with_repos : string list;
  with_deps : string list;
  jobs : int option;
  toolchain_override : string option;
  depext_only : bool;
  envrc_mode : Sync.envrc_mode;
  archives_only : bool;
  every_version : bool;
  save_d10ir : string option;
  dist : string option;
  upload_archive : string option;
  archive_sources : bool;
  snapshot_reporepo : bool;
  targets : string list;
}

let run_progress_buckets ~fs ~sys ~cache ~data_dir ~refresh ~platform ~proc_mgr
    ~clock ~os_key ~http_session ~cache_root ~acc ~args =
  Progress_ui.with_ui
    ~target:(target_label_of ~all:args.all args.targets)
    ~clock:(clock :> _ Eio.Resource.t)
    ~enabled:(Tty.is_tty ())
    (fun ui_reporter ->
      let bi0, buckets =
        prepare_buckets ~fs ~sys ~cache ~data_dir ~refresh ~platform
          ~registry:args.registry ~use_registry:args.use_registry
          ~with_repos:args.with_repos ~with_deps:args.with_deps
          ~toolchain_override:args.toolchain_override
          ~depext_only:args.depext_only ~archives_only:args.archives_only
          ~ui_reporter ~proc_mgr ~clock ~os_key ~http_session ~acc ~all:args.all
          ~only:args.only ~skip:args.skip ~cache_root args.targets
      in
      let bi =
        {
          bi0 with
          archive_sources = args.archive_sources;
          snapshot_reporepo = args.snapshot_reporepo;
        }
      in
      let gi_offset_ref = ref 0 in
      List.iter
        (run_one_bucket ~fs ~cache ~os_key ~ui_reporter ~bi ~acc ~refresh
           ~depext_only:args.depext_only ~archives_only:args.archives_only
           ~save_d10ir:args.save_d10ir ~dist:args.dist ~jobs:args.jobs
           ~upload_archive:args.upload_archive ~gi_offset_ref)
        buckets)

(* Dispatch any spec-less or short-circuit flag combination first; if
   none apply, fall through and return [`Continue] so [run] runs
   the full multi-bucket flow. *)
let dispatch_shortcuts ~fs ~sys ~cache ~data_dir ~refresh ~platform ~proc_mgr
    ~clock ~os_key ~http_session ~project_mode ~cwd_s ~args =
  if args.every_version then
    exit
      (mirror_archives ~fs ~cache ~label:"every-version"
         (collect_every_version_archives ~fs ~sys ~refresh ~only:args.only
            ~skip:args.skip));
  if args.depext_only && args.all then begin
    do_depext_all ~fs ~sys ~cache ~data_dir ~refresh ~platform
      ~toolchain_override:args.toolchain_override;
    exit 0
  end;
  if project_mode then begin
    let ec =
      run_project_mode ~fs ~proc_mgr ~clock ~sys ~platform ~os_key ~cache
        ~data_dir ~http_session ~registry:args.registry
        ~use_registry:args.use_registry ~refresh ~with_repos:args.with_repos
        ~with_deps:args.with_deps ~jobs:args.jobs
        ~toolchain_override:args.toolchain_override
        ~depext_only:args.depext_only ~envrc_mode:args.envrc_mode
        ~dist:args.dist ~cwd_s
    in
    exit ec
  end

let mk_args refresh locked skip_local all only skip registry use_registry
    with_repos with_deps jobs toolchain_override depext_only envrc_mode
    archives_only every_version save_d10ir dist upload_archive archive_sources
    snapshot_reporepo targets =
  {
    refresh = refresh && not locked;
    skip_local;
    all;
    only;
    skip;
    registry;
    use_registry = (if locked then Oi.Use_registry.Never else use_registry);
    with_repos;
    with_deps;
    jobs;
    toolchain_override;
    depext_only;
    envrc_mode;
    archives_only;
    every_version;
    save_d10ir;
    dist;
    upload_archive;
    archive_sources;
    snapshot_reporepo;
    targets;
  }

let finalise_run ~cache_root ~(acc : acc) ~run_start_time =
  print_build_summary ~targets:acc.targets ~target_handle:acc.target_handle
    ~solve_failures:acc.solve_failures ~target_group:acc.target_group
    ~group_results:acc.group_results;
  print_fetch_logs ~cache_root ~run_start_time;
  print_dist_artifacts acc.dist_mapping

let run (c : Terms.common) refresh locked skip_local all only skip registry
    use_registry with_repos with_deps jobs toolchain_override depext_only
    envrc_mode archives_only every_version save_d10ir dist upload_archive
    archive_sources snapshot_reporepo targets =
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
  let args =
    mk_args refresh locked skip_local all only skip registry use_registry
      with_repos with_deps jobs toolchain_override depext_only envrc_mode
      archives_only every_version save_d10ir dist upload_archive archive_sources
      snapshot_reporepo targets
  in
  let project_mode, cwd_s =
    detect_project_mode ~fs ~skip_local:args.skip_local ~targets:args.targets
      ~all:args.all
  in
  (* [oi build script.ml]: scaffold dune-project/dune/<base>.opam (if
     absent) then build it as the cwd project — the generated opam gives
     project mode something to solve. *)
  let project_mode =
    project_mode || maybe_scaffold_script ~fs ~cwd_s ~targets:args.targets
  in
  validate_build_flags ~targets:args.targets ~all:args.all ~project_mode
    ~depext_only:args.depext_only ~archives_only:args.archives_only
    ~every_version:args.every_version;
  dispatch_shortcuts ~fs ~sys ~cache ~data_dir ~refresh:args.refresh ~platform
    ~proc_mgr ~clock ~os_key ~http_session ~project_mode ~cwd_s ~args;
  let run_start_time = Unix.time () in
  let cache_root = Oi.Cache.root_s cache in
  let acc = new_acc () in
  run_progress_buckets ~fs ~sys ~cache ~data_dir ~refresh:args.refresh ~platform
    ~proc_mgr ~clock ~os_key ~http_session ~cache_root ~acc ~args;
  finalise_run ~cache_root ~acc ~run_start_time

let targets_arg =
  Arg.(
    value & pos_all string []
    & info ~docv:"PKG"
        ~doc:"Build target(s). Omit in project mode or with $(b,--all)." [])

let all_arg =
  Arg.(
    value & flag
    & info
        ~doc:
          "Build every overlay's $(b,x-root-packages); fall back to the whole \
           overlay otherwise. Skips $(b,default) unless named in $(b,--only)."
        [ "all" ])

let only_arg =
  Arg.(
    value & opt_all string []
    & info ~docv:"HANDLE" ~doc:"Restrict $(b,--all) to $(i,HANDLE). Repeatable."
        [ "only" ])

let skip_arg =
  Arg.(
    value & opt_all string []
    & info ~docv:"HANDLE"
        ~doc:"Exclude $(i,HANDLE) from $(b,--all). Repeatable." [ "skip" ])

let depext_only_arg =
  Arg.(
    value & flag
    & info ~doc:"Solve only; print required system packages, one per line."
        [ "depext" ])

let archives_only_arg =
  Arg.(
    value & flag
    & info
        ~doc:
          "Fetch source archives into the local mirror; skip build and \
           install. Mutually exclusive with $(b,--depext)."
        [ "archives-only" ])

let every_version_arg =
  Arg.(
    value & flag
    & info
        ~doc:
          "With $(b,--archives-only), mirror every recorded version in the \
           reporepo (including $(b,default)). Honours $(b,--only) / \
           $(b,--skip)."
        [ "every-version" ])

let save_d10ir_arg =
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"DIR"
        ~doc:
          "Write each solve group's recipe to $(i,DIR) as \
           $(b,<root-pkg>.d10ir.json). Build still runs."
        [ "save-d10ir" ])

let dist_arg =
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"DIR"
        ~doc:
          "After a successful build, copy the $(b,bin/) and $(b,sbin/) \
           contents of every root layer (resolving symlinks) into \
           $(i,DIR/bin/) and $(i,DIR/sbin/). Used by the multi-stage $(b,oi \
           docker) image to surface installed binaries for the runtime stage's \
           $(b,COPY --from=build)."
        [ "dist" ])

let upload_archive_arg =
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"URL"
        ~doc:
          "After the build, mirror every freshly built layer to \
           $(i,URL)/$(b,<os_key>/layers/<hash>.tar.zst) plus its companion \
           $(b,.json) manifest, every d10ir source-manifest sidecar to \
           $(i,URL)/$(b,d10ir-archives/<sha>.json), and the per-invocation \
           build manifest to $(i,URL)/$(b,<os_key>/builds/...) via $(b,s3cmd \
           put). Assumes a working $(b,~/.s3cfg). Typical use: \
           $(b,--upload-archive=s3://oiu/)."
        [ "upload-archive" ])

let archive_sources_arg =
  Arg.(
    value & flag
    & info
        ~doc:
          "Also upload the consolidated source $(b,.tar.zst) tarballs to \
           $(i,URL)/$(b,d10ir-archives/<sha>.tar.zst) (in addition to the JSON \
           sidecars). Default off: the sidecar's resolved $(b,commit_sha) + \
           upstream URLs are enough for reproducibility while upstream git \
           remotes remain alive. Flip on for self-contained, \
           upstream-deletion-proof buckets."
        [ "archive-sources" ])

let snapshot_reporepo_arg =
  Arg.(
    value & flag
    & info
        ~doc:
          "After the build, tarball the local reporepo at the solve-time HEAD \
           commit and PUT to $(i,URL)/$(b,reporepo/<commit>.tar.zst). Default \
           off: a downstream consumer is expected to re-clone the reporepo \
           from its own URL. Flip on when the reporepo is private and the \
           bucket should be the canonical archival source."
        [ "snapshot-reporepo" ])

let cmd_info =
  Cmd.info "build" ~doc:"Build a project, package, overlay, or every overlay"
    ~man:
      [
        `S Manpage.s_description;
        `P
          "Solve and build $(i,PKG) into the layer cache. With no $(i,PKG), \
           build the cwd project: sync $(b,*.opam) deps and dev tools into \
           $(b,_oi/), then run $(b,dune build --profile=release).";
        `S "TARGETS";
        `I ("(none)", "Cwd $(b,*.opam) project.");
        `I ("$(b,PKG)", "Single package.");
        `I ("$(b,@HANDLE/PKG)", "Package from overlay $(i,HANDLE).");
        `I ("$(b,@HANDLE)", "Every package in overlay $(i,HANDLE).");
        `I ("$(b,--all)", "Every overlay's $(b,x-root-packages).");
        `S "PROJECT EXTRAS";
        `P "In project mode the cwd metadata feeds the solve:";
        `I
          ( "$(b,*.opam)",
            "$(b,depends:), $(b,pin-depends:), $(b,x-repos:) merge in." );
        `I
          ( "$(b,packages/) + $(b,repo)",
            "Project-local $(b,packages/) tree is layered as the \
             highest-priority opam repository." );
        `S Manpage.s_examples;
        `Pre
          "  oi build\n\
          \  oi build dune\n\
          \  oi build @avsm/owntracks\n\
          \  oi build @avsm\n\
          \  oi build --all --upload-archive=s3://oiu/\n\
          \  oi build --all --depext | sudo apt install -y -";
      ]

let cmd =
  Cmd.v cmd_info
    Term.(
      const run $ Terms.common $ Terms.refresh $ Terms.locked $ Terms.skip_local
      $ all_arg $ only_arg $ skip_arg $ Terms.registry $ Terms.use_registry
      $ Terms.with_repos $ Terms.with_deps $ Terms.jobs $ Terms.toolchain
      $ depext_only_arg $ Sync.envrc_mode_arg $ archives_only_arg
      $ every_version_arg $ save_d10ir_arg $ dist_arg $ upload_archive_arg
      $ archive_sources_arg $ snapshot_reporepo_arg $ targets_arg)

(* -- oi test ------------------------------------------------------------ *)

let test_project_mode_or_fail ~fs ~proc_mgr ~clock ~sys ~platform ~os_key ~cache
    ~data_dir ~registry ~use_registry ~http_session ~refresh ~with_repos
    ~with_deps ~jobs ~toolchain_override ~envrc_mode ~dry_run ~cwd_s
    ~project_mode =
  if not project_mode then
    Oi.Error.fail_config_error
      "oi test: no *.opam in %s. Run from a project, or pass a PKG / \
       @HANDLE/PKG."
      cwd_s;
  Project_build.run ~action:`Test ~fs ~proc_mgr ~clock ~sys ~platform ~os_key
    ~cache ~data_dir ~registry ~use_registry ~session:http_session ~refresh
    ~with_repos ~with_deps ?jobs ?toolchain:toolchain_override ~envrc_mode
    ~dry_run ~cwd:cwd_s ()

let dispatch_test ~fs ~proc_mgr ~clock ~sys ~platform ~os_key ~cache ~data_dir
    ~registry ~use_registry ~http_session ~refresh ~with_repos ~with_deps ~jobs
    ~toolchain_override ~envrc_mode ~dry_run ~cwd_s ~project_mode targets =
  match targets with
  | [] ->
      test_project_mode_or_fail ~fs ~proc_mgr ~clock ~sys ~platform ~os_key
        ~cache ~data_dir ~registry ~use_registry ~http_session ~refresh
        ~with_repos ~with_deps ~jobs ~toolchain_override ~envrc_mode ~dry_run
        ~cwd_s ~project_mode
  | [ target ] ->
      run_target_test ~target ~fs ~proc_mgr ~clock ~sys ~platform ~os_key ~cache
        ~data_dir ~registry ~use_registry ~session:http_session ~refresh
        ~with_repos ~with_deps ?jobs ?toolchain:toolchain_override ~dry_run ()
  | _ ->
      Oi.Error.fail_config_error
        "oi test takes at most one PKG / @HANDLE/PKG target."

let test_run (c : Terms.common) refresh skip_local registry use_registry
    with_repos with_deps jobs toolchain_override envrc_mode dry_run targets =
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
  let project_mode, cwd_s =
    detect_project_mode ~fs ~skip_local ~targets ~all:false
  in
  let ec =
    dispatch_test ~fs ~proc_mgr ~clock ~sys ~platform ~os_key ~cache ~data_dir
      ~registry ~use_registry ~http_session ~refresh ~with_repos ~with_deps
      ~jobs ~toolchain_override ~envrc_mode ~dry_run ~cwd_s ~project_mode
      targets
  in
  exit ec

let test_dry_run_arg =
  Arg.(
    value & flag
    & info ~doc:"Print the test command without running it." [ "n"; "dry-run" ])

let test_targets_arg =
  Arg.(
    value & pos_all string []
    & info ~docv:"PKG" ~doc:"Test target: $(b,PKG) or $(b,@HANDLE/PKG)." [])

let test_cmd_info =
  Cmd.info "test" ~doc:"Run a project's or a package's tests"
    ~man:
      [
        `S Manpage.s_description;
        `P
          "Build $(i,PKG) (same as $(b,oi build)) then run $(b,dune runtest) \
           in its build directory.";
        `P
          "With no $(i,PKG), test the cwd project: $(b,dune runtest \
           --profile=release) against the assembled $(b,_oi/) prefix.";
        `P "See $(b,oi docker --test) for a CI Dockerfile.";
        `S Manpage.s_examples;
        `Pre "  oi test\n  oi test @avsm/owntracks";
      ]

let test_cmd =
  Cmd.v test_cmd_info
    Term.(
      const test_run $ Terms.common $ Terms.refresh $ Terms.skip_local
      $ Terms.registry $ Terms.use_registry $ Terms.with_repos $ Terms.with_deps
      $ Terms.jobs $ Terms.toolchain $ Sync.envrc_mode_arg $ test_dry_run_arg
      $ test_targets_arg)
