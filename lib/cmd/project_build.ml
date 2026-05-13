let ( / ) = Filename.concat

(* Scan [_build/install/default/{bin,sbin}] under [cwd] and copy each
   entry into [cwd/dist/<basename>-<os_key>]. Returns the list of
   (source basename, destination path) pairs actually written, in the
   order they were discovered (bin first, then sbin). Missing
   sub-directories are skipped silently — a project without installed
   binaries simply yields []. *)
let collect_dist ~cwd ~os_key =
  let dist_root = cwd / "dist" in
  (try Unix.mkdir dist_root 0o755 with Unix.Unix_error (EEXIST, _, _) -> ());
  let install_root = cwd / "_build" / "install" / "default" in
  let scan sub =
    Dist.list_files (install_root / sub)
    |> List.map (fun (name, src) ->
        let dst = dist_root / Fmt.str "%s-%s" name os_key in
        Dist.copy_resolved ~src ~dst;
        (name, dst))
  in
  scan "bin" @ scan "sbin"

(* Read every *.opam file in [cwd] as [(name, opam)] pairs, sorted
   alphabetically. Files we cannot parse are silently dropped — the
   solve-side load (Project.load) already warns about them. *)
let read_opams ~cwd =
  let files = try Sys.readdir cwd |> Array.to_list with Sys_error _ -> [] in
  files
  |> List.filter (fun f ->
      Filename.check_suffix f ".opam" && Filename.chop_suffix f ".opam" <> "")
  |> List.sort String.compare
  |> List.filter_map (fun filename ->
      let path = cwd / filename in
      let name = Filename.chop_suffix filename ".opam" in
      try
        let opam = OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path)) in
        Some (name, opam)
      with Sys_error _ | Failure _ -> None)

(* Topo-sort sibling [*.opam] packages by their inter-package
   [depends:] edges; ties broken alphabetically. Cycles fall through
   in alphabetical order. The dune-project codepath bypasses this
   (dune does its own ordering). *)
let topo_sort_local opams =
  let local_set = List.map fst opams in
  let local_deps_of opam =
    OpamFormula.fold_left
      (fun acc (n, _) ->
        let ns = OpamPackage.Name.to_string n in
        if List.mem ns local_set then ns :: acc else acc)
      []
      (OpamFile.OPAM.depends opam)
  in
  let alpha = List.sort (fun (a, _) (b, _) -> String.compare a b) in
  let rec loop order = function
    | [] -> order
    | remaining ->
        let pending = List.map fst remaining in
        let ready, rest =
          List.partition
            (fun (_, deps) ->
              List.for_all (fun d -> not (List.mem d pending)) deps)
            remaining
        in
        if ready = [] then
          (* Cycle: bail with remaining packages in alphabetical order. *)
          order @ List.map fst (alpha rest)
        else loop (order @ List.map fst (alpha ready)) rest
  in
  loop [] (List.map (fun (n, op) -> (n, local_deps_of op)) opams)

(* Solve the project's dep closure without building. Returns
   [(packages_dirs, conf, pkgs)] for downstream depext / introspection.
   Replicates the solver setup [Sync.run] does internally up to the
   [Solver.solve] call, then stops. *)
let project_solve ~fs ~sys ~cache ~data_dir ~refresh ~platform ~with_repos
    ~with_deps ~toolchain_override ~cwd () =
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
  let project = Oi.Project.load ~fs cwd in
  let extra_cli, url_project =
    Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh with_deps
  in
  if project.deps = [] && extra_cli = [] && url_project.roots = [] then
    Oi.Error.fail_config_error "oi build: no *.opam files in %s" cwd;
  let conf =
    Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  let candidate_overlays = project.overlays @ url_project.overlays in
  let tc_handles =
    candidate_overlays @ Target.handles_of_tokens with_repos
    |> List.sort_uniq String.compare
  in
  let toolchain =
    Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf ~install:false
      ~override:toolchain_override ~handles:tc_handles ()
  in
  let conf, tc_ctx = Oi.Pipeline.solver_inputs toolchain conf in
  let project_overlays =
    Oi.Pipeline.filter_compatible_overlays
      ~reporepo_path:(Terms.reporepo_path ()) ~override:toolchain_override
      ~toolchain candidate_overlays
  in
  let with_repos = project_overlays @ with_repos in
  let cli_extras = Target.cli_extra_repos ~fs ~sys ?toolchain with_repos in
  let all_extras =
    Target.merge_extras ~cli:cli_extras
      ~project:(project.extra_repos @ url_project.extra_repos)
  in
  let extra_pkg_dirs =
    Oi.Source.Repo.ensure_many ~fs ~data_dir ~refresh all_extras
  in
  let pin_dir =
    Oi.Source.Pin.materialize ~fs ~sys ~cache ~refresh
      (project.pins @ url_project.pins)
  in
  let base_pkg_dirs =
    match toolchain with
    | None -> Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ()
    | Some i -> i.Oi.Toolchain.packages_dirs
  in
  let local_packages_dir =
    match project.packages_dir with
    | Some _ -> project.packages_dir
    | None -> url_project.packages_dir
  in
  let packages_dirs =
    Stdlib.Option.to_list local_packages_dir
    @ Stdlib.Option.to_list pin_dir
    @ extra_pkg_dirs @ base_pkg_dirs
  in
  let cache_root = Oi.Cache.root_s cache in
  let build_prefix = cache_root / "build" / "prefix" in
  let ctx =
    Oi.Solver.Ctx.create ~prefix:build_prefix ~packages_dirs ~conf
      ?toolchain:tc_ctx ()
  in
  let extra_constraints = Oi.Project.Script.constraints extra_cli in
  let extra_names =
    List.filter_map
      (fun (d : Oi.Project.Script.dep) ->
        if OpamPackage.Name.to_string d.name = "ocaml" then None
        else Some d.name)
      extra_cli
  in
  let url_names = List.map OpamPackage.Name.of_string url_project.roots in
  let names =
    List.map OpamPackage.Name.of_string project.deps @ extra_names @ url_names
    |> Oi.Pipeline.strip_compiler_roots_for_override
         ~override:toolchain_override ~toolchain
  in
  match
    Oi.Solver.solve ~sys ~fs ~cache_root ctx ~packages_dirs
      ~constraints:extra_constraints names
  with
  | Ok pkgs -> (packages_dirs, conf, pkgs)
  | Error msg -> Oi.Error.no_solution msg

(* Print depexts for the cwd's project deps, one per line. Solves the
   project's *.opam closure (no build), unions the [depexts:] declared
   by every solved package under [conf]'s opam filter variables, and
   emits each token. *)
let depexts ~fs ~sys ~platform ~cache ~data_dir ?(refresh = false)
    ?(with_repos = []) ?(with_deps = []) ?toolchain ~cwd () =
  let packages_dirs, conf, pkgs =
    project_solve ~fs ~sys ~cache ~data_dir ~refresh ~platform ~with_repos
      ~with_deps ~toolchain_override:toolchain ~cwd ()
  in
  let entries = Oi.Depexts.compute_for_conf ~conf ~packages_dirs pkgs in
  let all =
    List.fold_left
      (fun acc e -> OpamSysPkg.Set.union acc e.Oi.Depexts.sys_pkgs)
      OpamSysPkg.Set.empty entries
  in
  OpamSysPkg.Set.iter (fun p -> Fmt.pr "%s@." (OpamSysPkg.to_string p)) all;
  0

(* Drive the cwd's [*.opam] project after a successful [Sync.run].
   Dispatches on [action]:
   - [`Deps_only]: stop after sync (deps + tools + maybe .envrc).
   - [`Build]: run [dune build --profile=release].
   - [`Test]: run [dune runtest --profile=release].
   Non-dune projects only support [`Deps_only] for now. *)
type action = [ `Build | `Test | `Deps_only ]

let dune_target = function
  | `Build -> Some "build"
  | `Test -> Some "runtest"
  | `Deps_only -> None

let action_label = function
  | `Build -> "Build"
  | `Test -> "Test"
  | `Deps_only -> "Sync"

let run ~action ~fs ~proc_mgr ~clock ~sys ~platform ~os_key ~cache ~data_dir
    ~registry ~use_registry ~session ?(refresh = false) ?(with_repos = [])
    ?(with_deps = []) ?jobs ?toolchain ?envrc_mode ?(dry_run = false) ?dist ~cwd
    () =
  let label = action_label action in
  let label_lc = String.lowercase_ascii label in
  let opams = read_opams ~cwd in
  if opams = [] then
    Oi.Error.fail_config_error "oi %s: no *.opam files in %s" label_lc cwd;
  let dune_project = cwd / "dune-project" in
  let needs_dune = action <> `Deps_only in
  if needs_dune && not (Sys.file_exists dune_project) then
    Oi.Error.fail_config_error
      "oi %s: %s has no dune-project. Non-dune projects are not yet supported."
      label_lc cwd;
  let order = topo_sort_local opams in
  if dry_run then begin
    let cmd_line =
      match dune_target action with
      | None -> "(deps + tools only)"
      | Some t -> Fmt.str "dune %s --profile=release" t
    in
    Fmt.pr "@[<v>%a@,@,  cd %s@,  %s@,@,%a packages: %s@,@]@."
      Oi.Style.pp_header_string "Would run:" cwd cmd_line Oi.Style.pp_dim_string
      "→" (String.concat ", " order);
    0
  end
  else
    (* One spinner across the whole flow. The multi-bar [Progress]
       displays inside [Pipeline.fetch_layer_hashes] / [Execute.run]
       / [install_tools] attach to the same [shared_display] via
       [add_line] / [remove_line], so the overall bar stays visible
       across phase transitions. The dune subprocess at the end runs
       after [preflight_done] finalises the display so its stdout has
       a clean canvas.

       Phase budget for the overall bar: the visible phases that
       [Pipeline.build] / [Sync.run] / [install_tools] fire when
       run from [oi build]. Easy to over- or under-shoot — the bar
       caps at 100% — but lands close enough to give the user a
       useful sense of "how far am I". *)
    let prefix, tc =
      Sync.run ~quiet:false ~refresh ~with_repos ~with_deps ?jobs ?toolchain
        ?envrc_mode ~proc_mgr ~fs ~clock ~sys ~platform ~os_key ~cache ~data_dir
        ~registry ~use_registry ~session ~cwd ()
    in
    match dune_target action with
    | None -> 0
    | Some target ->
        let dune_cache_root = Oi.Cache.dune_root cache in
        let tc_ctx = Option.map Oi.Toolchain.opam_ctx_of_info tc in
        let env =
          Oi.Solver.Env.make_env ?toolchain:tc_ctx ~prefix ~dune_cache_root ()
        in
        Fmt.pr "@.%a %d package(s): %s@." Oi.Style.pp_header_string
          (label ^ "ing") (List.length opams) (String.concat ", " order);
        let ec =
          Subprocess.run proc_mgr ~env [ "dune"; target; "--profile=release" ]
        in
        if ec <> 0 then begin
          Fmt.epr "%a (dune %s exit %d)@." Oi.Style.pp_error_string
            (label ^ " failed") target ec;
          ec
        end
        else begin
          Fmt.pr "%a@." Oi.Style.pp_ok_string (label ^ " successful");
          (* Only the [`Build] action produces installable binaries —
             [`Test] runs [dune runtest] which leaves [_build/install/]
             untouched, and [`Deps_only] returned earlier. *)
          (match action with
          | `Build -> (
              (match collect_dist ~cwd ~os_key with
              | [] -> ()
              | mapping ->
                  Fmt.pr "@.%a@." Oi.Style.pp_header_string "Dist artifacts:";
                  List.iter
                    (fun (name, dst) ->
                      Fmt.pr "  %s %a %s@." name Oi.Style.pp_dim_string "→" dst)
                    mapping);
              (* [--dist=DIR] (passed through from the CLI): also mirror
                 the dune install tree into [DIR/{bin,sbin,share}/].
                 Different layout from the cwd/dist flat one above so
                 the docker multi-stage flow can [COPY /dist/ /usr/local/]
                 in one shot. *)
              match dist with
              | None -> ()
              | Some dir ->
                  let install_root = cwd / "_build" / "install" / "default" in
                  let extra =
                    Dist.collect_install ~root:install_root ~dst:dir
                  in
                  let bin_sbin =
                    List.filter
                      (fun (sub, _, _) -> sub = "bin" || sub = "sbin")
                      extra
                  in
                  let share_count =
                    List.length
                      (List.filter (fun (sub, _, _) -> sub = "share") extra)
                  in
                  if bin_sbin <> [] || share_count > 0 then begin
                    Fmt.pr "@.%a (--dist):@." Oi.Style.pp_header_string
                      "Dist tree";
                    List.iter
                      (fun (_, n, d) ->
                        Fmt.pr "  %s %a %s@." n Oi.Style.pp_dim_string "→" d)
                      bin_sbin;
                    if share_count > 0 then
                      Fmt.pr "  share/ %a %a@." Oi.Style.pp_dim_string
                        (Fmt.str "(%d files)" share_count)
                        Oi.Style.pp_dim_string "—"
                  end)
          | `Test | `Deps_only -> ());
          0
        end
