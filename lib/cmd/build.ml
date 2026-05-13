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
              List.iter
                (fun (pkg, log_path) ->
                  if
                    log_path <> ""
                    && (not (Hashtbl.mem seen log_path))
                    && Sys.file_exists log_path
                  then begin
                    Hashtbl.replace seen log_path ();
                    match
                      Oi.Audit.tail_of_file ~lines:100 ~path:log_path ()
                    with
                    | None -> ()
                    | Some tail ->
                        Oi.Say.newline ();
                        Fmt.kstr
                          (Fmt.pr "%a %s %a@." Oi.Style.pp_error_string
                             "── tail" pkg Oi.Style.pp_dim_string)
                          "(%s)" log_path;
                        Fmt.pr "%s@." tail
                  end)
                per_pkg_logs
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

(* Enumerate every [name/name.version/opam] under [pkgs_dir] as
   [OpamPackage.t]. Used by the [Walk_clone] arm of {!overlay_inputs}:
   no solving, just "anything the overlay ships could land in the
   container, so its depexts must be installed up front". *)
let all_packages_in_dir pkgs_dir =
  if not (Sys.file_exists pkgs_dir) then []
  else
    Sys.readdir pkgs_dir |> Array.to_list
    |> List.concat_map (fun name ->
        let name_dir = pkgs_dir / name in
        if (not (Sys.file_exists name_dir)) || not (Sys.is_directory name_dir)
        then []
        else
          Sys.readdir name_dir |> Array.to_list
          |> List.filter_map (fun pv ->
              let opam_path = name_dir / pv / "opam" in
              if not (Sys.file_exists opam_path) then None
              else try Some (OpamPackage.of_string pv) with Failure _ -> None))

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
  (* For toolchain-defining handles ([toolchain-oxcaml],
     [toolchain-ocaml-5-3], …) the entry carries its own
     [x-oi-toolchain-name], but [Pipeline.toolchain_names_of_handle]
     only consults [x-oi-toolchain] (use-site) and the
     [Toolchain.depends_of] reverse map (where the lookup key is the
     CLI name, not the reporepo handle). Without this fallback, those
     handles resolve to the default toolchain and the solve fails with
     compiler-version conflicts. *)
  let toolchain_for handle =
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
  in
  let pkg_dirs_for handle =
    packages_dirs_for_overlay ~base_packages_dirs ~reporepo_entries handle
  in
  List.concat_map
    (function
      | Solve_groups { handle; groups } -> (
          let toolchain =
            try toolchain_for handle
            with Oi.Error.E _ ->
              Log.warn (fun m ->
                  m
                    "overlay depexts: %s toolchain resolution failed — \
                     skipping handle"
                    handle);
              None
          in
          match toolchain with
          | None -> []
          | Some _ ->
              let pkg_dirs = pkg_dirs_for handle in
              let conf, tc_ctx =
                Oi.Pipeline.solver_inputs toolchain host_conf
              in
              List.filter_map
                (fun group ->
                  let ctx =
                    Oi.Solver.Ctx.create ~prefix:build_prefix
                      ~packages_dirs:pkg_dirs ~conf ?toolchain:tc_ctx ()
                  in
                  let items = List.map Target.parse_pkg_target group in
                  let names = List.map fst items in
                  let constraints =
                    List.fold_left
                      (fun acc (name, c) ->
                        match c with
                        | None -> acc
                        | Some c -> OpamPackage.Name.Map.add name c acc)
                      OpamPackage.Name.Map.empty items
                  in
                  match
                    Oi.Solver.solve ~sys ~fs ~cache_root ctx
                      ~packages_dirs:pkg_dirs ~constraints names
                  with
                  | Ok solved -> Some (pkg_dirs, solved)
                  | Error msg ->
                      Log.warn (fun m ->
                          m "overlay depexts: %s group failed to solve: %s"
                            handle msg);
                      None)
                groups)
      | Walk_clone { handle } ->
          let pkg_dirs = pkg_dirs_for handle in
          let overlay_pkgs_dir =
            try Some (Oi.Source.Reporepo.assert_overlay_dir ~path ~handle)
            with Oi.Error.E _ -> None
          in
          let pkgs =
            match overlay_pkgs_dir with
            | None -> []
            | Some d -> all_packages_in_dir d
          in
          if pkgs = [] then []
          else begin
            Log.info (fun m ->
                m
                  "overlay depexts: %s walking %d packages (no x-root-packages \
                   declared)"
                  handle (List.length pkgs));
            [ (pkg_dirs, pkgs) ]
          end)
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
  let extra_deps, url_project =
    Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh with_deps
  in
  let handle_pins = Stdlib.Option.to_list target_pin @ with_pins in
  let tc_handles =
    Target.pin_handles handle_pins
    @ Target.handles_of_tokens with_repos
    @ url_project.overlays
    |> List.sort_uniq String.compare
  in
  let toolchain =
    Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf ~install:true
      ~override:toolchain ~handles:tc_handles ()
  in
  let cli_extras = Target.cli_extra_repos ~fs ~sys ?toolchain with_repos in
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
  let pkg_name = OpamPackage.Name.of_string target in
  let names =
    [ pkg_name ]
    |> Oi.Pipeline.strip_compiler_roots_for_override ~override:None ~toolchain
  in
  if dry_run then begin
    Fmt.pr
      "@[<v>%a@,\
       @,\
      \  oi build %s@,\
      \  cd <build_dir>@,\
      \  dune runtest --profile=release@,\
       @]@."
      Oi.Style.pp_header_string "Would run:" target_display;
    0
  end
  else begin
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
    let req : Oi.Build_pipeline.request =
      {
        targets =
          [
            Group
              {
                tokens = List.map OpamPackage.Name.to_string names;
                handles = [];
              };
          ];
        (* Overlay handles must flow through so [solve_uncached] can
           resolve them into [packages/] dirs — [extra_repos] alone
           isn't read by the per-group solve. *)
        with_repos;
        pins = url_project.pins;
        extra_repos = all_extras;
        constraints = extra_constraints;
        toolchain_override = None;
        toolchain;
        conf;
        local_packages_dir = url_project.packages_dir;
        project_root = None;
        force_source = false;
        refresh;
      }
    in
    let layer_hashes =
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
          }
      in
      Oi.Build_pipeline.layer_hashes solved
    in
    match
      find_target_layer ~fs ~cache ~os_key ~pkg_name:target layer_hashes
    with
    | None ->
        Oi.Error.fail_not_found target
          "no built layer matched %s; the solve may have substituted a \
           different package."
          target
    | Some (layer_hash, pkg_full) ->
        let short =
          String.sub layer_hash 0 (min 12 (String.length layer_hash))
        in
        let build_dir =
          Oi.Cache.root_s cache / "build" / "_build" / (pkg_full ^ "-" ^ short)
        in
        if not (Sys.file_exists build_dir) then
          Oi.Error.fail_not_found target
            "build dir %s missing (layer was cached without preserved source). \
             Pass --refresh to rebuild from source."
            build_dir;
        if not (Sys.file_exists (build_dir / "dune-project")) then
          Oi.Error.fail_config_error
            "%s has no dune-project; native opam test commands not yet \
             supported."
            build_dir;
        let prefix =
          Oi.Pipeline.assemble_prefix ~sys ~fs ~clock ~cache ~os_key
            ~layer_hashes
        in
        let dune_cache_root = Oi.Cache.dune_root cache in
        let tc_ctx = Option.map Oi.Toolchain.opam_ctx_of_info toolchain in
        let env =
          Oi.Solver.Env.make_env ?toolchain:tc_ctx ~prefix ~dune_cache_root ()
        in
        Fmt.pr "@.%a %s@.%a %s@." Oi.Style.pp_header_string "Testing" pkg_full
          Oi.Style.pp_dim_string "→" build_dir;
        let cmd = Fmt.str "cd %s && dune runtest --profile=release" build_dir in
        let ec = Subprocess.run proc_mgr ~env [ "/bin/sh"; "-c"; cmd ] in
        if ec <> 0 then begin
          Fmt.epr "%a (dune runtest exit %d)@." Oi.Style.pp_error_string
            "Test failed" ec;
          ec
        end
        else begin
          Fmt.pr "%a@." Oi.Style.pp_ok_string "Test successful";
          0
        end
  end

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

(* -- oi build dispatcher ------------------------------------------------ *)

let cmd =
  let run (c : Terms.common) refresh locked skip_local all only skip registry
      use_registry with_repos with_deps jobs toolchain_override depext_only
      export envrc_mode archives_only every_version save_d10ir dist
      upload_archive targets =
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
    let data_dir = c.data_dir in
    let refresh = refresh && not locked in
    let use_registry = if locked then Oi.Use_registry.Never else use_registry in
    (* Project mode: no positional, no --all, *.opam present in cwd.
       [--skip-local] forces non-project mode regardless. *)
    let cwd_s, _ = Workspace.resolved_cwd fs in
    let project_mode =
      (not skip_local) && targets = [] && (not all)
      &&
        try
          Sys.readdir cwd_s
          |> Array.exists (fun f ->
              Filename.check_suffix f ".opam"
              && Filename.chop_suffix f ".opam" <> "")
        with Sys_error _ -> false
    in
    let needs_spec what =
      Oi.Error.fail_config_error
        "oi build %s: no spec and no project (cwd has no *.opam). Pass a PKG, \
         @HANDLE, or --all."
        what
    in
    (* Flag validation. Mode-specific dispatch happens after this
       block; here we only reject combinations that can't possibly
       proceed: [--export] + [--depext] together, and any flag whose
       result depends on solving when there's nothing to solve. *)
    if export <> None && depext_only then
      Oi.Error.fail_config_error
        "oi build: --export and --depext are mutually exclusive";
    if archives_only && (export <> None || depext_only) then
      Oi.Error.fail_config_error
        "oi build --archives-only: cannot combine with --export or --depext \
         (no build runs, so there's nothing to publish or depext)";
    let no_spec = targets = [] && (not all) && not project_mode in
    if no_spec && export <> None then needs_spec "--export";
    if no_spec && depext_only then needs_spec "--depext";
    if no_spec && archives_only then needs_spec "--archives-only";
    if every_version && not archives_only then
      Oi.Error.fail_config_error
        "oi build --every-version: only valid with --archives-only (it skips \
         the solver and walks every recorded reporepo opam file)";
    if every_version then begin
      let path = Terms.reporepo_path () in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh ~path
        ~url:(Terms.reporepo_url ()) ();
      let archives =
        let seen : (string, unit) Hashtbl.t = Hashtbl.create 4096 in
        let acc = ref [] in
        Oi.Source.Reporepo.iter_opam_files ~path ~include_handles:only
          ~skip_handles:skip (fun ~handle ~pkg ~version ~opam_path ->
            let pkg_label = Fmt.str "@%s/%s.%s" handle pkg version in
            Oi.Source.Mirror.archives_of_opam_file ~path:opam_path
              ~pkg:pkg_label
            |> List.iter (fun (a : Oi.Source.Mirror.archive) ->
                let key = OpamUrl.to_string a.url in
                if not (Hashtbl.mem seen key) then begin
                  Hashtbl.add seen key ();
                  acc := a :: !acc
                end));
        List.rev !acc
      in
      exit (mirror_archives ~fs ~cache ~label:"every-version" archives)
    end;
    if depext_only && all then begin
      let conf =
        Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
      in
      let names =
        compute_overlay_depexts_for_conf ~fs ~sys ~cache ~data_dir ~refresh
          ~conf ?override:toolchain_override ()
      in
      List.iter (fun n -> Fmt.pr "%s@." n) names;
      exit 0
    end;
    let do_export_if_set ?(ok = true) () =
      match export with
      | Some output when ok ->
          Registry_export.run ~fs
            ~clock:(clock :> D10.Config.clk)
            ~sys ~os_key ~cache ~registry ~output
      | _ -> ()
    in
    if project_mode then begin
      let ec =
        if depext_only then
          Project_build.depexts ~fs ~sys ~platform ~cache ~data_dir ~refresh
            ~with_repos ~with_deps ?toolchain:toolchain_override ~cwd:cwd_s ()
        else
          Project_build.run ~action:`Build ~fs ~proc_mgr ~clock ~sys ~platform
            ~os_key ~cache ~data_dir ~registry ~use_registry
            ~session:http_session ~refresh ~with_repos ~with_deps ?jobs
            ?toolchain:toolchain_override ~envrc_mode ?dist ~cwd:cwd_s ()
      in
      do_export_if_set ~ok:(ec = 0) ();
      exit ec
    end;
    (* Timestamp for filtering stale log files out of the end-of-run
       "transient fetch errors" listing. Any [fetch-*.log] with mtime
       older than this was left over by a previous invocation. *)
    let run_start_time = Unix.time () in
    let target_label =
      if all then "all"
      else match targets with [] -> "." | xs -> String.concat ", " xs
    in
    (* [cache_root] needed by both the in-bar phases and the
       post-bar [print_build_summary] / log-listing logic. *)
    let cache_root = Oi.Cache.root_s cache in
    (* Hoisted out of [with_ui] so [print_build_summary] (called
       AFTER [with_ui] returns) can read them on a clean terminal. *)
    let targets_ref : string list ref = ref [] in
    let target_handle : (string, string) Hashtbl.t = Hashtbl.create 16 in
    let solve_failures : (string, string * string) Hashtbl.t =
      Hashtbl.create 8
    in
    let target_group : (string, int) Hashtbl.t = Hashtbl.create 16 in
    let group_results : (int, group_outcome) Hashtbl.t = Hashtbl.create 16 in
    (* Accumulator for [--dist=DIR]: each successful per-bucket build
       appends [(subdir, basename, dst-path)] triples for the binaries
       it surfaced from its root layers. Printed in a "Dist artifacts:"
       block AFTER [with_ui] closes so the live progress bar isn't
       overwritten. Stays empty when [--dist] is unset. *)
    let dist_mapping : (string * string * string) list ref = ref [] in
    (* One [Progress_ui] covers slow setup ([ensure_base],
       classify_with_args, pick_toolchain) AND the build itself.
       The body returns [unit] — the build_summary print fires
       OUTSIDE [with_ui] so the display is finalised first and the
       summary lands on a clean terminal. *)
    Progress_ui.with_ui ~target:target_label
      ~clock:(clock :> _ Eio.Resource.t)
      ~enabled:(Tty.is_tty ())
      (fun ui_reporter ->
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
        (* When [--all] is set, walk every overlay in the reporepo and
       derive targets from each one:
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
        let reporepo_target_groups =
          if not all then []
          else begin
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
            List.concat_map
              (fun h ->
                let default_skipped =
                  h = "default"
                  &&
                  match only_set with
                  | None -> true
                  | Some s -> not (List.mem h s)
                in
                if default_skipped then begin
                  Log.info (fun m ->
                      m "--all: skipping %s (pass --only default to include)" h);
                  []
                end
                else
                  let included =
                    (match only_set with
                      | None -> true
                      | Some s -> List.mem h s)
                    && not (List.mem h skip_set)
                  in
                  if not included then []
                  else
                    match Oi.Source.Reporepo.latest entries ~handle:h with
                    | None -> []
                    | Some e when e.toolchain_name <> None ->
                        Log.info (fun m ->
                            m "--all: skipping toolchain definition %s" h);
                        []
                    | Some e ->
                        if e.root_packages = [] then begin
                          Log.info (fun m ->
                              m
                                "--all: overlay %s has no x-root-packages, \
                                 expanding to every package in the overlay"
                                h);
                          [ [ "@" ^ h ] ]
                        end
                        else
                          List.map
                            (fun group ->
                              List.map (fun p -> "@" ^ h ^ "/" ^ p) group)
                            e.root_packages)
              handles
          end
        in
        (* Each CLI-supplied target is its own (singleton) solve group, so
       [oi build a b] solves [a] and [b] independently. Reporepo groups
       may be multi-element (compiler variants etc.). The tokens are
       still raw — [@handle]-only entries haven't been fanned out to
       the overlay's packages yet (we need the clone first). *)
        let token_groups =
          List.map (fun t -> [ t ]) targets @ reporepo_target_groups
        in
        let tokens = List.concat token_groups in
        if tokens = [] then
          begin if all then
            Oi.Error.fail_config_error
              "--all expanded to nothing in %s (all overlays filtered by \
               --skip/--only, or the reporepo only contains 'default')"
              (Terms.reporepo_path ())
          else
            Oi.Error.fail_config_error
              "no targets to build (pass PKG arguments or --all)"
          end;
        (* Classify each input into a plain target or an overlay form.
       Overlay forms collect handles to thread through [with_repos]
       so the later [Target.cli_extra_repos] run clones them up front. The
       "build everything in this overlay" form is expanded once the
       clones exist. *)
        let parsed = List.map Target.parse_build_target tokens in
        let with_repos =
          let handles =
            List.filter_map
              (function
                | Target.Plain_target _ -> None
                | Target.Overlay_pkg (h, _) | Target.Overlay_all h -> Some h)
              parsed
            |> List.sort_uniq String.compare
          in
          with_repos @ handles
        in
        let extra_cli, url_project =
          Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh
            ~reporter:ui_reporter with_deps
        in
        (* Split handles into two scopes:
       - [global_handles] apply to every solve (explicit [--with-repo]
         + any URL-project [x-repos] @-handles).
       - [token_handles] come from [@h/pkg] tokens and only apply to
         their group's solve.
       [with_repos] at this point already contains both, so recover
       [global_handles] by subtracting the token-derived set. *)
        let token_handles =
          List.filter_map
            (function
              | Target.Overlay_pkg (h, _) | Target.Overlay_all h -> Some h
              | Target.Plain_target _ -> None)
            parsed
        in
        let global_handles =
          let tokens = List.sort_uniq String.compare token_handles in
          List.filter (fun h -> not (List.mem h tokens)) with_repos
          @ url_project.overlays
        in
        (* Clone every relevant overlay (global + token) upfront so
       per-group resolution below just reads already-materialised
       packages dirs. We don't keep the merged paths list — packages
       dirs are recomputed per-group from the handle subset. *)
        let all_handles =
          List.sort_uniq String.compare (global_handles @ token_handles)
        in
        let cli_extras_records =
          Target.merge_extras
            ~cli:(Target.cli_extra_repos ~fs ~sys all_handles)
            ~project:url_project.extra_repos
        in
        let _ : string list =
          Oi.Source.Repo.ensure_many ~fs ~data_dir ~refresh cli_extras_records
        in
        (* URL-project pins materialize into a synthetic packages/ tree
       the solver consumes ahead of everything else, so the URL's
       dev-version of each local package wins over any stable version
       from the opam-repository. *)
        let pin_dir =
          Oi.Source.Pin.materialize ~fs ~sys ~cache ~refresh url_project.pins
        in
        (* Expand [@handle] into every package the overlay's clone
       provides. List just the top-level names under the overlay's
       [packages/] dir — the solver will pick specific versions. If
       the overlay was force-bumped to a new version since the last
       [ensure_extra] call (e.g. the user just ran [oi repo add
       --force] pointing at a new URL), clone it on the fly rather
       than fail out.

       Packages that any reporepo toolchain definition claims as its
       own (via [x-oi-toolchain-compiler] or [x-oi-toolchain-roots])
       are excluded: building them as standalone targets is pointless
       under a non-relocatable toolchain (they get filtered out of the
       exec plan, leaving nothing to build) and merely duplicates a
       compiler bake under a relocatable one. *)
        let spec_name spec =
          match String.index_opt spec '.' with
          | None -> spec
          | Some i -> String.sub spec 0 i
        in
        let toolchain_pkg_names entries =
          let names = ref [] in
          List.iter
            (fun (e : Oi.Source.Reporepo.entry) ->
              if e.toolchain_name <> None then begin
                Stdlib.Option.iter
                  (fun s -> names := spec_name s :: !names)
                  e.toolchain_compiler;
                List.iter
                  (fun group ->
                    List.iter (fun s -> names := spec_name s :: !names) group)
                  e.toolchain_roots
              end)
            entries;
          List.sort_uniq String.compare !names
        in
        let overlay_packages handle =
          let path = Terms.reporepo_path () in
          let entries = Oi.Source.Reporepo.load ~path in
          match Oi.Source.Reporepo.latest entries ~handle with
          | None ->
              Oi.Error.fail_config_error "no overlay %s in reporepo" handle
          | Some e ->
              let pkgs_dir =
                Oi.Source.Reporepo.overlay_packages_dir ~path ~handle:e.handle
              in
              if not (Sys.file_exists pkgs_dir) then
                Oi.Error.fail_config_error
                  "overlay %s.%s is not materialised at %s; run 'oi repo bump \
                   %s' to populate it (upstream %s)"
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
                      "Overlay %s: excluding %d toolchain package(s) from \
                       --all expansion: %s"
                      handle (List.length dropped)
                      (String.concat ", " dropped));
              kept
        in
        (* Expand each raw group into package-name groups. A group
       containing just an [@handle] fallback (no [x-root-packages])
       fans out into one singleton group per package the overlay
       ships — "build everything in the overlay" isn't a single-solve
       concept. Groups composed of [@handle/pkg] or plain [pkg] tokens
       keep their shape so multi-package solve groups (compiler
       variants) survive intact.

       Each result carries its own [handles] — the set of overlay
       handles that should be visible to the solver for this group.
       [@avsm/karakeep] yields a group with handles [avsm], nothing
       else. That scope is what keeps [@avsm] solves from picking up
       conflicting packages out of [@samoht]'s overlay. *)
        let raw_target_groups :
            (string list (* targets *) * string list (* handles *)) list =
          List.concat_map
            (fun raw_group ->
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
                              "@%s cannot appear inside a multi-package solve \
                               group; use @%s/PKG or list packages explicitly"
                              h h)
                      classified
                  in
                  let handles =
                    List.filter_map
                      (function
                        | Target.Plain_target _ -> None
                        | Target.Overlay_pkg (h, _) | Target.Overlay_all h ->
                            Some h)
                      classified
                    |> List.sort_uniq String.compare
                  in
                  [ (names, handles) ])
            token_groups
        in
        let target_groups = raw_target_groups in
        let targets = List.concat_map fst target_groups in
        if targets = [] && url_project.roots = [] then
          Oi.Error.fail_config_error "no targets to build";
        (* [--with] adds extra packages to every target's root set plus any
       version constraints they carry. *)
        let base_constraints = Oi.Project.Script.constraints extra_cli in
        let extra_names =
          List.filter_map
            (fun (d : Oi.Project.Script.dep) ->
              if OpamPackage.Name.to_string d.name = "ocaml" then None
              else Some d.name)
            extra_cli
          @ List.map OpamPackage.Name.of_string url_project.roots
        in
        let target_groups =
          target_groups @ List.map (fun r -> ([ r ], [])) url_project.roots
        in
        let targets = List.concat_map fst target_groups in
        targets_ref := targets;
        (* Per-target result tracking; the final summary walks [targets] in
       order and looks each name up here. A target either fails to
       solve (status stored directly), or lands in some group. Groups
       are keyed by index; their build result (ok / failed, with the
       package counts) is written into [group_results] when the group
       finishes. *)
        (* [solve_failures], [target_group] and [group_results] hoisted
       to outer scope; populated below from [Build_pipeline.solve]'s
       per-group results. *)
        (* -- Build_pipeline.solve + .build ----------------------------------- *)
        (* The whole multi-group orchestration funnels into [Build_pipeline]:
       it runs the solver per group, elaborates each into a [Plan.t] +
       [D10ir.Plan.t] recipe, merges the recipes, and (when
       [Build_pipeline.build] runs) drives the unified fetch + archive
       prefetch + [D10ir.Direct.run] on the merged plan. The cmdliner
       layer is left with: target classification (above), short-circuit
       flag handling ([--depext], [--archives-only], [--save-d10ir]),
       failure mapping back into [group_results] for the summary. *)
        let pipeline_env : Oi.Build_pipeline.env =
          { proc_mgr; fs; clock; sys; os_key; cache; data_dir; http_session }
        in
        let extra_token_names =
          List.map OpamPackage.Name.to_string extra_names
        in
        let make_req ~override tgs : Oi.Build_pipeline.request =
          let bp_targets : Oi.Build_pipeline.target list =
            List.map
              (fun (toks, handles) : Oi.Build_pipeline.target ->
                Group { tokens = toks @ extra_token_names; handles })
              tgs
          in
          {
            targets = bp_targets;
            with_repos = global_handles;
            pins = [];
            extra_repos = [];
            constraints = base_constraints;
            toolchain_override = override;
            toolchain = None;
            conf;
            local_packages_dir = pin_dir;
            project_root = None;
            force_source = false;
            refresh;
          }
        in
        (* [--all] fans every reporepo overlay into a target group, and
       each overlay's [x-oi-toolchain] field can point at a different
       toolchain. [Build_pipeline.solve] requires one toolchain per
       request, so partition [target_groups] by toolchain and run
       solve/build/post-process once per partition. Indices are
       offset across partitions so [target_group] / [group_results]
       stay collision-free. Only kicks in for the plain build path;
       [--depext] / [--archives-only] take the single-solve path. *)
        let reporepo_entries =
          lazy
            (try Oi.Source.Reporepo.load ~path:(Terms.reporepo_path ())
             with Sys_error _ | Failure _ -> [])
        in
        let toolchain_of_handles handles =
          match handles with
          | [] -> None
          | h :: _ -> (
              match
                Oi.Pipeline.toolchain_names_of_handle
                  (Lazy.force reporepo_entries)
                  h
              with
              | [ n ] -> Some n
              | _ -> None)
        in
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
                  let cur =
                    Stdlib.Option.value (Hashtbl.find_opt by_k k) ~default:[]
                  in
                  Hashtbl.replace by_k k (x :: cur)
              | None -> none_bucket := x :: !none_bucket)
            xs;
          let keyed =
            Hashtbl.fold (fun k vs acc -> (Some k, List.rev vs) :: acc) by_k []
            |> List.sort (fun (a, _) (b, _) -> compare a b)
          in
          match !none_bucket with
          | [] -> keyed
          | xs -> (None, List.rev xs) :: keyed
        in
        let buckets : (string option * (string list * string list) list) list =
          let split_for_all =
            all && toolchain_override = None && (not depext_only)
            && not archives_only
          in
          if not split_for_all then [ (toolchain_override, target_groups) ]
          else
            let buckets =
              partition_by
                ~key:(fun (_, handles) -> toolchain_of_handles handles)
                target_groups
            in
            if List.length buckets <= 1 then
              [ (toolchain_override, target_groups) ]
            else begin
              let names =
                List.filter_map (fun (tc, _) -> tc) buckets
                |> List.sort String.compare
              in
              Log.info (fun m ->
                  m
                    "--all: splitting solve into %d toolchain bucket(s) (%s); \
                     pass --toolchain=NAME to force a single bucket"
                    (List.length buckets) (String.concat ", " names));
              buckets
            end
        in
        let gi_offset_ref = ref 0 in
        List.iter
          (fun (override, bucket_groups) ->
            let gi_offset = !gi_offset_ref in
            let req = make_req ~override bucket_groups in
            let solved =
              Oi.Build_pipeline.solve pipeline_env ~reporter:ui_reporter req
            in
            (* Populate per-target tracking from [solved.groups]: each
         token's group index, each solve-failure's message + log path.
         [gi_offset] keeps indices unique across per-toolchain buckets. *)
            List.iteri
              (fun gi (gr : Oi.Build_pipeline.group_result) ->
                let gi = gi + gi_offset in
                List.iter
                  (fun t -> Hashtbl.replace target_group t gi)
                  gr.group.tokens;
                match gr.error with
                | Error (Solve_failed { msg; log_path }) ->
                    List.iter
                      (fun t ->
                        Hashtbl.replace solve_failures t (msg, log_path))
                      gr.group.tokens;
                    (* Mirror the original Audit emission for solve failures. *)
                    let layer_hash =
                      let key =
                        String.concat " "
                          (gr.group.tokens
                          @ List.map (fun h -> "@" ^ h) gr.group.handles)
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
                          | [ h ] ->
                              Some { D10.Overlay.handle = h; version = "" }
                          | _ -> None);
                        toolchain =
                          Option.map
                            (fun (i : Oi.Toolchain.info) -> i.handle)
                            gr.toolchain;
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
                        Oi.Audit.append ~fs ~cache_root:(Oi.Cache.root_s cache)
                          event)
                      gr.group.tokens
                | _ -> ())
              solved.groups;
            let any_solved =
              List.exists
                (fun (gr : Oi.Build_pipeline.group_result) ->
                  Result.is_ok gr.error)
                solved.groups
            in
            if not any_solved then
              Oi.Error.fail_msg "no packages solved successfully";
            (* [--depext]: every group is solved, sum their depexts. *)
            if depext_only then begin
              let group_conf, _ =
                Oi.Pipeline.solver_inputs solved.toolchain conf
              in
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
                        (fun acc e ->
                          OpamSysPkg.Set.union acc e.Oi.Depexts.sys_pkgs)
                        acc entries)
                  OpamSysPkg.Set.empty solved.groups
              in
              OpamSysPkg.Set.iter
                (fun p -> Fmt.pr "%s@." (OpamSysPkg.to_string p))
                all;
              exit 0
            end;
            if archives_only then begin
              let archives =
                List.concat_map
                  (fun (gr : Oi.Build_pipeline.group_result) ->
                    if not (Result.is_ok gr.error) then []
                    else
                      Oi.Source.Mirror.collect_archives
                        ~packages_dirs:gr.pkgs_dir gr.pkgs)
                  solved.groups
              in
              exit (mirror_archives ~fs ~cache ~label:"solved" archives)
            end;
            (* Cycle-failed groups: log + emit Dep_failed events for the
         counters + populate [group_results] so the summary reports
         them. They never reach [Direct.run] (no recipe to run). *)
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
                           This is an upstream metadata bug — the cyclic \
                           packages all declare each other as [{= version}] \
                           dependencies. Re-bump the offending overlay once \
                           the upstream opam files are fixed."
                          gr.group.label cycle_label);
                    let n_pkgs = List.length gr.pkgs in
                    Hashtbl.replace group_results gi
                      (Group_failed
                         {
                           counts = { n_pkgs; n_built = 0; n_cached = 0 };
                           msg = Fmt.str "dependency cycle: %s" cycle_label;
                           per_pkg_logs = [];
                         })
                | _ -> ())
              solved.groups;
            (* [--save-d10ir]: persist each group's recipe to disk. *)
            (match save_d10ir with
            | None -> ()
            | Some dir ->
                Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dir);
                List.iter
                  (fun (gr : Oi.Build_pipeline.group_result) ->
                    match gr.recipe with
                    | None -> ()
                    | Some recipe ->
                        let stem =
                          match gr.group.tokens with
                          | t :: _ ->
                              String.map
                                (fun c ->
                                  if
                                    (c >= 'a' && c <= 'z')
                                    || (c >= 'A' && c <= 'Z')
                                    || (c >= '0' && c <= '9')
                                    || c = '.' || c = '-' || c = '_'
                                  then c
                                  else '_')
                                t
                          | [] -> "recipe"
                        in
                        let dst = Filename.concat dir (stem ^ ".d10ir.json") in
                        D10ir.Plan.save Eio.Path.(fs / dst) recipe;
                        Logs.info (fun m -> m "Saved d10ir recipe to %s" dst))
                  solved.groups);
            (* Audit emission for cached layers. [Direct.run] doesn't append
         audit events for [Cached] outcomes; record them ourselves so
         the manifest's [callers[]] list still attributes this run. *)
            let audit_now = Unix.gettimeofday () in
            let toolchain_handle =
              Option.map
                (fun (i : Oi.Toolchain.info) -> i.handle)
                solved.toolchain
            in
            List.iter
              (fun (gr : Oi.Build_pipeline.group_result) ->
                if not (Result.is_ok gr.error) then ()
                else
                  match gr.exec_plan with
                  | None -> ()
                  | Some xp ->
                      List.iter
                        (fun (p : Oi.Plan.package_plan) ->
                          if p.method_ = Oi.Identity.Binary then
                            let opam_pkg =
                              match OpamPackage.of_string_opt p.pkg with
                              | Some op -> op
                              | None -> OpamPackage.of_string (p.pkg ^ ".0.0")
                            in
                            let context : Oi.Audit.context =
                              {
                                (Oi.Audit.default_context ()) with
                                overlay =
                                  Oi.Plan.overlay_of_pkg
                                    ~packages_dirs:gr.pkgs_dir opam_pkg;
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
                            Oi.Audit.append ~fs
                              ~cache_root:(Oi.Cache.root_s cache) event)
                        xp.packages)
              solved.groups;
            (* Run the unified post-merge fetch + Direct.run on the merged
         plan. Returns [None] when no recipe was produced (every group
         failed solve / cycle / emit). *)
            let result_opt =
              Oi.Build_pipeline.build pipeline_env ~reporter:ui_reporter
                {
                  solved;
                  layer_remote;
                  source_remote;
                  jobs;
                  upload_archive_url = upload_archive;
                }
            in
            (* Map per-node failures back to per-group [group_results]. *)
            let failed_layers : (string, string) Hashtbl.t =
              Hashtbl.create 64
            in
            (match result_opt with
            | None -> ()
            | Some (result : D10ir.Direct.result) ->
                List.iteri
                  (fun gi (gr : Oi.Build_pipeline.group_result) ->
                    let gi = gi + gi_offset in
                    match gr.error with
                    | Error (Cycle _) -> ()
                    | Error _ -> ()
                    | Ok () -> (
                        match gr.exec_plan with
                        | None -> ()
                        | Some exec_plan ->
                            let count_by f =
                              List.length (List.filter f exec_plan.packages)
                            in
                            let n_build =
                              count_by (fun (p : Oi.Plan.package_plan) ->
                                  p.method_ = Source)
                            in
                            let n_cached =
                              count_by (fun (p : Oi.Plan.package_plan) ->
                                  p.method_ = Binary)
                            in
                            let n_pkgs = n_build + n_cached in
                            let pkg_set : (string, unit) Hashtbl.t =
                              Hashtbl.create 32
                            in
                            List.iter
                              (fun (p : Oi.Plan.package_plan) ->
                                Hashtbl.replace pkg_set p.pkg ())
                              exec_plan.packages;
                            let group_failures =
                              List.filter
                                (fun (f : D10ir.Direct.failure) ->
                                  let pkg_str =
                                    Fmt.str "%s.%s" f.package.name
                                      f.package.version
                                  in
                                  Hashtbl.mem pkg_set pkg_str)
                                result.failures
                            in
                            List.iter
                              (fun (f : D10ir.Direct.failure) ->
                                let pkg_str =
                                  Fmt.str "%s.%s" f.package.name
                                    f.package.version
                                in
                                List.iter
                                  (fun (p : Oi.Plan.package_plan) ->
                                    if p.pkg = pkg_str then
                                      Hashtbl.replace failed_layers p.layer_hash
                                        f.log_path)
                                  exec_plan.packages)
                              group_failures;
                            let collect_failures (exec_plan : Oi.Plan.t) =
                              let seen = Hashtbl.create 16 in
                              List.filter_map
                                (fun (p : Oi.Plan.package_plan) ->
                                  match
                                    Hashtbl.find_opt failed_layers p.layer_hash
                                  with
                                  | Some path
                                    when path <> ""
                                         && not (Hashtbl.mem seen path) ->
                                      Hashtbl.replace seen path ();
                                      Some (p.pkg, path)
                                  | _ -> None)
                                exec_plan.packages
                            in
                            let counts =
                              { n_pkgs; n_built = n_build; n_cached }
                            in
                            let outcome =
                              if group_failures = [] then Group_ok counts
                              else
                                let pkg_summary =
                                  match group_failures with
                                  | [ f ] ->
                                      Fmt.str "package %s.%s in phase %s"
                                        f.package.name f.package.version
                                        (D10ir.Direct.string_of_phase f.phase)
                                  | fs -> Fmt.str "%d packages" (List.length fs)
                                in
                                let output =
                                  Fmt.str "%a" D10ir.Direct.pp_failures
                                    group_failures
                                in
                                Group_failed
                                  {
                                    counts;
                                    msg =
                                      Fmt.str "build failed: %s\n%s" pkg_summary
                                        output;
                                    per_pkg_logs = collect_failures exec_plan;
                                  }
                            in
                            Hashtbl.replace group_results gi outcome))
                  solved.groups);
            (* [--dist=DIR]: copy [bin/] + [sbin/] + [share/] from each
               root layer's fs prefix into
               [DIR/{bin,sbin,share}/]. Skips roots whose layer never
               materialised (solve / build failure); silently no-ops
               when [dist] is unset. *)
            (* Roots = user-requested packages. Sourced from
               [exec_plan.packages] filtered by [group.names], not from
               [merged.roots] — the d10ir plan goes empty whenever every
               requested package is already binary-cached, so a warm-cache
               [oi build --dist=DIR] would otherwise produce nothing. *)
            (match (dist, result_opt) with
            | Some dir, Some _ ->
                (try Unix.mkdir dir 0o755
                 with Unix.Unix_error (EEXIST, _, _) -> ());
                List.iter
                  (fun h ->
                    let layer_fs = cache_root / "layers" / os_key / h / "fs" in
                    if Sys.file_exists layer_fs then
                      let added =
                        Dist.collect_install ~root:layer_fs ~dst:dir
                      in
                      dist_mapping := !dist_mapping @ added)
                  (Oi.Build_pipeline.root_layer_hashes solved)
            | _ -> ());
            gi_offset_ref := !gi_offset_ref + List.length solved.groups)
          buckets);
    (* Progress_ui closed; the summary block prints on a clean
       terminal. *)
    print_build_summary ~targets:!targets_ref ~target_handle ~solve_failures
      ~target_group ~group_results;
    (* Point at any fetch-retry logs collected during the run so the
       user can investigate transient errors (git fetch failures,
       opam archive 500s) without them polluting the live output. *)
    let logs_dir = Oi.Cache.Logs.dir ~cache_root in
    if Sys.file_exists logs_dir then begin
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
    end;
    (* [--export DIR]: publish the local cache once the build phase has
       settled. *)
    do_export_if_set ();
    (* [--dist=DIR] mapping print — fired after [with_ui] has closed so
       the bar isn't overwritten. Counts by sub-dir to keep the share
       tree (which can be 100s of entries) summarised rather than
       enumerated line by line. *)
    if !dist_mapping <> [] then begin
      let by_sub = Hashtbl.create 4 in
      List.iter
        (fun (sub, _, _) ->
          Hashtbl.replace by_sub sub
            (1 + Stdlib.Option.value (Hashtbl.find_opt by_sub sub) ~default:0))
        !dist_mapping;
      Fmt.pr "@.%a@." Oi.Style.pp_header_string "Dist artifacts:";
      List.iter
        (fun (_, name, dst) ->
          Fmt.pr "  %s %a %s@." name Oi.Style.pp_dim_string "→" dst)
        (List.filter
           (fun (sub, _, _) -> sub = "bin" || sub = "sbin")
           !dist_mapping);
      match Hashtbl.find_opt by_sub "share" with
      | Some n when n > 0 ->
          Fmt.pr "  share/ %a %a@." Oi.Style.pp_dim_string
            (Fmt.str "(%d files)" n) Oi.Style.pp_dim_string "—"
      | _ -> ()
    end
  in
  let targets =
    Arg.(
      value & pos_all string []
      & info ~docv:"PKG"
          ~doc:"Build target(s). Omit in project mode or with $(b,--all)." [])
  in
  let all =
    Arg.(
      value & flag
      & info
          ~doc:
            "Build every overlay's $(b,x-root-packages); fall back to the \
             whole overlay otherwise. Skips $(b,default) unless named in \
             $(b,--only)."
          [ "all" ])
  in
  let only =
    Arg.(
      value & opt_all string []
      & info ~docv:"HANDLE"
          ~doc:"Restrict $(b,--all) to $(i,HANDLE). Repeatable." [ "only" ])
  in
  let skip =
    Arg.(
      value & opt_all string []
      & info ~docv:"HANDLE"
          ~doc:"Exclude $(i,HANDLE) from $(b,--all). Repeatable." [ "skip" ])
  in
  let depext_only =
    Arg.(
      value & flag
      & info ~doc:"Solve only; print required system packages, one per line."
          [ "depext" ])
  in
  let export =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"DIR"
          ~doc:
            "Publish a registry into $(i,DIR) after the build: layers, source \
             archives, and $(b,index.db). Requires a build spec or project. \
             Mutually exclusive with $(b,--depext)."
          [ "export" ])
  in
  let archives_only =
    Arg.(
      value & flag
      & info
          ~doc:
            "Fetch source archives into the local mirror; skip build and \
             install. Mutually exclusive with $(b,--export) and $(b,--depext)."
          [ "archives-only" ])
  in
  let every_version =
    Arg.(
      value & flag
      & info
          ~doc:
            "With $(b,--archives-only), mirror every recorded version in the \
             reporepo (including $(b,default)). Honours $(b,--only) / \
             $(b,--skip)."
          [ "every-version" ])
  in
  let save_d10ir =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"DIR"
          ~doc:
            "Write each solve group's recipe to $(i,DIR) as \
             $(b,<root-pkg>.d10ir.json). Build still runs."
          [ "save-d10ir" ])
  in
  let dist =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"DIR"
          ~doc:
            "After a successful build, copy the $(b,bin/) and $(b,sbin/) \
             contents of every root layer (resolving symlinks) into \
             $(i,DIR/bin/) and $(i,DIR/sbin/). Used by the multi-stage $(b,oi \
             docker) image to surface installed binaries for the runtime \
             stage's $(b,COPY --from=build)."
          [ "dist" ])
  in
  let upload_archive =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"URL"
          ~doc:
            "After the build, mirror every freshly built layer to \
             $(i,URL)/$(b,<os_key>/<hash>.tar.zst) (plus the matching \
             $(b,.txt.zst) listing) via $(b,s3cmd put). Only layers actually \
             built locally are uploaded — anything restored from the local \
             cache or pulled from $(b,--use-registry) is skipped. Assumes a \
             working $(b,~/.s3cfg). Typical use: \
             $(b,--upload-archive=s3://oiu/)."
          [ "upload-archive" ])
  in
  let info =
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
            \  oi build --all --export ./registry\n\
            \  oi build --all --depext | sudo apt install -y -";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.common $ Terms.refresh $ Terms.locked $ Terms.skip_local
      $ all $ only $ skip $ Terms.registry $ Terms.use_registry
      $ Terms.with_repos $ Terms.with_deps $ Terms.jobs $ Terms.toolchain
      $ depext_only $ export $ Sync.envrc_mode_arg $ archives_only
      $ every_version $ save_d10ir $ dist $ upload_archive $ targets)

(* -- oi test ------------------------------------------------------------ *)

let test_cmd =
  let run (c : Terms.common) refresh skip_local registry use_registry with_repos
      with_deps jobs toolchain_override envrc_mode dry_run targets =
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
    let data_dir = c.data_dir in
    let cwd_s, _ = Workspace.resolved_cwd fs in
    let project_mode =
      (not skip_local) && targets = []
      &&
        try
          Sys.readdir cwd_s
          |> Array.exists (fun f ->
              Filename.check_suffix f ".opam"
              && Filename.chop_suffix f ".opam" <> "")
        with Sys_error _ -> false
    in
    match targets with
    | [] ->
        if not project_mode then
          Oi.Error.fail_config_error
            "oi test: no *.opam in %s. Run from a project, or pass a PKG / \
             @HANDLE/PKG."
            cwd_s;
        let ec =
          Project_build.run ~action:`Test ~fs ~proc_mgr ~clock ~sys ~platform
            ~os_key ~cache ~data_dir ~registry ~use_registry
            ~session:http_session ~refresh ~with_repos ~with_deps ?jobs
            ?toolchain:toolchain_override ~envrc_mode ~dry_run ~cwd:cwd_s ()
        in
        exit ec
    | [ target ] ->
        let ec =
          run_target_test ~target ~fs ~proc_mgr ~clock ~sys ~platform ~os_key
            ~cache ~data_dir ~registry ~use_registry ~session:http_session
            ~refresh ~with_repos ~with_deps ?jobs ?toolchain:toolchain_override
            ~dry_run ()
        in
        exit ec
    | _ ->
        Oi.Error.fail_config_error
          "oi test takes at most one PKG / @HANDLE/PKG target."
  in
  let dry_run =
    Arg.(
      value & flag
      & info ~doc:"Print the test command without running it."
          [ "n"; "dry-run" ])
  in
  let targets =
    Arg.(
      value & pos_all string []
      & info ~docv:"PKG" ~doc:"Test target: $(b,PKG) or $(b,@HANDLE/PKG)." [])
  in
  let info =
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
  in
  Cmd.v info
    Term.(
      const run $ Terms.common $ Terms.refresh $ Terms.skip_local
      $ Terms.registry $ Terms.use_registry $ Terms.with_repos $ Terms.with_deps
      $ Terms.jobs $ Terms.toolchain $ Sync.envrc_mode_arg $ dry_run $ targets)
