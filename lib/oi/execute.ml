[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

(** Stage-based parallel build executor. *)

let log_src = Logs.Src.create "oi.execute"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

(* Cap concurrent package builds. Each in-flight build spawns subprocess
   pipes (2 fds per capture) plus transient file descriptors for fetch
   and patch; large stages would otherwise exhaust macOS's default 256
   soft [rlim]. Honours [OI_BUILD_PARALLELISM] when set; defaults to
   [min (cpu_count) 8]. *)
let build_parallelism =
  match Sys.getenv_opt "OI_BUILD_PARALLELISM" with
  | Some s -> ( match int_of_string_opt s with Some n when n > 0 -> n | _ -> 8)
  | None -> min (Domain.recommended_domain_count ()) 8

(* -- Command execution --------------------------------------------------- *)

(* Resolve an unqualified executable name against the PATH in [env],
   not the parent process's PATH. Eio.Process.spawn uses execvp-style
   lookups against the caller's PATH, so [ocaml] would otherwise resolve
   to the host opam switch's ocaml rather than the one installed into the
   build prefix. Resolving here forces exec to use our prefix's binary. *)
let path_of_env env =
  Array.find_map
    (fun s ->
      if String.starts_with ~prefix:"PATH=" s then
        Some (String.sub s 5 (String.length s - 5))
      else None)
    env

let is_executable path =
  try
    Unix.access path [ Unix.X_OK ];
    true
  with Unix.Unix_error _ -> false

let resolve_in_path ~env exe =
  if String.contains exe '/' then exe
  else
    match path_of_env env with
    | None -> exe
    | Some path ->
        let found =
          String.split_on_char ':' path
          |> List.find_map (function
            | "" -> None
            | d ->
                let candidate = d / exe in
                if is_executable candidate then Some candidate else None)
        in
        Stdlib.Option.value ~default:exe found

let find_in_path ~env exe =
  let s = resolve_in_path ~env exe in
  if s = exe && not (String.contains exe '/') then None else Some s

(* Many opam patches use GNU-patch features (e.g. unified context,
   `diff -ruN` of empty files) that BSD /usr/bin/patch on macOS rejects.
   If `gpatch` is on PATH (from Homebrew's gpatch / coreutils) we prefer
   it; otherwise fall back to `patch` and hope for the best. *)
let patch_cmd =
  lazy
    (let env = Unix.environment () in
     match find_in_path ~env "gpatch" with Some p -> p | None -> "patch")

let run_cmd ~proc_mgr ~fs ~env ~cwd ~pkg cmd =
  let cmd_s = String.concat " " cmd in
  Log.debug (fun m -> m "  + %s" cmd_s);
  (* Resolve relative executables (starting with ./) against cwd, and
     resolve bare names against the build env's PATH. *)
  let cmd =
    match cmd with
    | exe :: rest when String.length exe > 0 && exe.[0] = '.' ->
        (cwd / exe) :: rest
    | exe :: rest -> resolve_in_path ~env exe :: rest
    | [] -> cmd
  in
  Eio.Switch.run @@ fun sw ->
  (* Capture stdout+stderr to a single pipe so we can suppress output
     on success and show it in the Build_error on failure. Merging into
     one pipe preserves the interleaved order of the two streams. *)
  let r, w = Eio.Process.pipe ~sw proc_mgr in
  let child =
    Eio.Process.spawn ~sw proc_mgr ~env
      ~cwd:Eio.Path.(fs / cwd)
      ~stdout:w ~stderr:w cmd
  in
  (* Close the parent's copy of the write end so the reader sees EOF
     when the child exits. *)
  Eio.Flow.close w;
  let output =
    try Eio.Buf_read.(parse_exn take_all) r ~max_size:max_int
    with End_of_file -> ""
  in
  Eio.Flow.close r;
  match Eio.Process.await child with
  | `Exited 0 -> ()
  | `Exited n ->
      Error.build_failed ~pkg ~cmd:cmd_s
        ~output:(Fmt.str "exited with code %d\n\n%s" n output)
  | `Signaled n ->
      Error.build_failed ~pkg ~cmd:cmd_s
        ~output:(Fmt.str "killed by signal %d\n\n%s" n output)

(* -- Fetching ------------------------------------------------------------- *)

let fetch_source ?(cache_urls = []) (p : Plan.package_plan) =
  match p.source with
  | None -> ()
  | Some src ->
      if not (Sys.file_exists p.build_dir) then begin
        let url = OpamUrl.parse ~handle_suffix:true src.url in
        let checksums = List.map OpamHash.of_string src.checksums in
        let dst_dir = OpamFilename.Dir.of_string p.build_dir in
        let cache_dir =
          OpamRepositoryPath.download_cache OpamStateConfig.(!r.root_dir)
        in
        Log.info (fun m -> m "Fetching %s from %s" p.pkg src.url);
        let result =
          OpamRepository.pull_tree p.pkg ~cache_dir ~cache_urls dst_dir
            checksums [ url ]
          |> OpamProcess.Job.run
        in
        match result with
        | OpamTypes.Result _ | OpamTypes.Up_to_date _ -> ()
        | OpamTypes.Not_available (_, msg) ->
            Fmt.failwith "Failed to fetch %s: %s" p.pkg msg
      end

let fetch_extra_sources ?(cache_urls = []) (p : Plan.package_plan) =
  List.iter
    (fun (name, (src : Plan.source_info)) ->
      let dst = p.build_dir / name in
      if not (Sys.file_exists dst) then begin
        let url = OpamUrl.parse ~handle_suffix:true src.url in
        let checksums = List.map OpamHash.of_string src.checksums in
        let dst_file = OpamFilename.of_string dst in
        let cache_dir =
          OpamRepositoryPath.download_cache OpamStateConfig.(!r.root_dir)
        in
        let result =
          OpamRepository.pull_file name ~cache_dir ~cache_urls
            ~silent_hits:true dst_file checksums [ url ]
          |> OpamProcess.Job.run
        in
        match result with
        | OpamTypes.Result () | OpamTypes.Up_to_date () -> ()
        | OpamTypes.Not_available (_, msg) ->
            Log.warn (fun m -> m "Failed to fetch extra source %s: %s" name msg)
      end)
    p.extra_sources

(* -- Patches and substitutions -------------------------------------------- *)

let apply_patches ~proc_mgr ~fs (p : Plan.package_plan) =
  List.iter
    (fun (patch : Plan.patch) ->
      let patch_file = p.build_dir / patch.file in
      if Sys.file_exists patch_file then
        run_cmd ~proc_mgr ~fs ~env:(Unix.environment ()) ~cwd:p.build_dir
          ~pkg:p.pkg
          [ Lazy.force patch_cmd; "-p1"; "-i"; patch_file ])
    p.patches

let apply_substs (p : Plan.package_plan) =
  (* Build an OpamFilter.env from the pre-computed subst_vars *)
  let env v =
    let key = OpamVariable.Full.to_string v in
    match List.assoc_opt key p.subst_vars with
    | Some s -> Some (OpamTypes.S s)
    | None -> None
  in
  List.iter
    (fun base ->
      let src = p.build_dir / (base ^ ".in") in
      let dst = p.build_dir / base in
      if Sys.file_exists src then
        OpamFilter.expand_interpolations_in_file_full env
          ~src:(OpamFilename.of_string src)
          ~dst:(OpamFilename.of_string dst))
    p.substs

(* -- Build and install ---------------------------------------------------- *)

let build_package ?(cache_urls = []) ~proc_mgr ~fs (p : Plan.package_plan) =
  fetch_source ~cache_urls p;
  (* Ensure build_dir exists before fetching extra-sources: pull_tree
     (in fetch_source) creates the directory, but packages with no main
     source (e.g. seq.base) still need the directory to exist so that
     extra-source files can be written into it. *)
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / p.build_dir);
  fetch_extra_sources ~cache_urls p;
  apply_patches ~proc_mgr ~fs p;
  apply_substs p;
  List.iter
    (fun cmd ->
      run_cmd ~proc_mgr ~fs ~env:p.env ~cwd:p.build_dir ~pkg:p.pkg cmd)
    p.build_commands

let install_package ~proc_mgr ~fs (p : Plan.package_plan) =
  List.iter
    (fun cmd ->
      run_cmd ~proc_mgr ~fs ~env:p.env ~cwd:p.build_dir ~pkg:p.pkg cmd)
    p.install_commands;
  if Sys.file_exists p.install_file then
    Installer.install ~fs ~prefix:p.prefix ~build_dir:p.build_dir
      ~install_file:p.install_file

(* -- Main loop ------------------------------------------------------------ *)

let run ?(cache_urls = []) ~proc_mgr ~fs ~clock ~sys ~os_key plan =
  let d10 : D10.Config.t =
    { sys; fs; clock; root = Eio.Path.(fs / plan.Plan.cache_root); os_key }
  in
  let prefix = plan.cache_root / "build" / "prefix" in
  (* Clean and create prefix directories *)
  Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / prefix);
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / prefix);
  List.iter
    (fun sub ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / prefix / sub))
    [ "bin"; "lib"; "sbin"; "share"; "etc"; "doc"; "man" ];
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
    Eio.Path.(fs / plan.cache_root / "build" / "_build");
  let n_stages = List.length plan.groups in
  let failed_pkgs : (string, bool) Hashtbl.t = Hashtbl.create 16 in
  let completed = ref 0 in
  let config = Progress.Config.v ~persistent:false () in
  let bar =
    let open Progress.Line in
    pair ~sep:(const " ")
      (list [ spinner (); brackets (count_to plan.total_packages) ])
      (rpad 60 string)
  in
  Progress.with_reporter ~config bar (fun report ->
      List.iter
        (fun (group : Plan.group) ->
          let stage_s = Fmt.str "stage %d/%d" group.stage n_stages in
          (* Phase 1: restore cached layers (Binary packages) *)
          List.iter
            (fun (p : Plan.package_plan) ->
              if p.method_ = `Binary then begin
                D10.Layer.restore d10 ~hash:p.layer_hash ~prefix;
                incr completed;
                report (1, Fmt.str "[%s] %s (cached)" stage_s p.pkg)
              end)
            group.packages;
          (* Phase 2: parallel fetch+build for Source packages *)
          let to_build =
            List.filter
              (fun (p : Plan.package_plan) ->
                if
                  p.method_ = `Source
                  && not
                       (List.exists
                          (fun (d : Plan.dep_layer) ->
                            Hashtbl.mem failed_pkgs d.pkg)
                          p.dep_layers)
                then true
                else begin
                  if p.method_ = `Source then
                    Hashtbl.replace failed_pkgs p.pkg true;
                  false
                end)
              group.packages
          in
          let build_failures : (string * exn) list ref = ref [] in
          if to_build <> [] then begin
            let active = ref 0 in
            Eio.Fiber.List.iter ~max_fibers:build_parallelism
              (fun (p : Plan.package_plan) ->
                active := !active + 1;
                report
                  ( 0,
                    Fmt.str "[%s] [%d active] %s (build)" stage_s !active p.pkg
                  );
                (try
                   Eio.Path.rmtree ~missing_ok:true
                     Eio.Path.(fs / p.build_dir);
                   build_package ~cache_urls ~proc_mgr ~fs p
                 with exn ->
                   build_failures := (p.pkg, exn) :: !build_failures;
                   Hashtbl.replace failed_pkgs p.pkg true);
                active := !active - 1)
              to_build
          end;
          (* Report build failures from this stage *)
          List.iter
            (fun (pkg, exn) ->
              Progress.interject_with (fun () ->
                  match exn with
                  | Error.E (Build_failed { pkg = p; cmd; output }) ->
                      Fmt.epr
                        "@.@[<v>\x1b[1;31mFAIL\x1b[0m %s@,command: %s@,@,%s@]@."
                        p cmd output
                  | _ -> Fmt.epr "  FAIL %s: %s@." pkg (Printexc.to_string exn)))
            !build_failures;
          (* Phase 3: serial install for successfully built packages *)
          List.iter
            (fun (p : Plan.package_plan) ->
              if p.method_ = `Source && not (Hashtbl.mem failed_pkgs p.pkg) then begin
                report (0, Fmt.str "[%s] %s (install)" stage_s p.pkg);
                let before = D10.Prefix.snapshot ~fs prefix in
                (try
                   install_package ~proc_mgr ~fs p;
                   let files =
                     D10.Prefix.diff ~fs ~prefix ~before |> List.map fst
                   in
                   let dep_hashes =
                     List.filter_map
                       (fun (d : Plan.dep_layer) ->
                         if D10.Layer.succeeded d10 ~hash:d.hash then
                           Some d.hash
                         else None)
                       p.dep_layers
                   in
                   D10.Layer.store d10 ~hash:p.layer_hash ~prefix ~files
                     ~package:p.pkg
                     ~deps:
                       (List.map
                          (fun (d : Plan.dep_layer) -> d.pkg)
                          p.dep_layers)
                     ~parent_hashes:dep_hashes ~exit_status:0
                 with exn ->
                   Hashtbl.replace failed_pkgs p.pkg true;
                   Progress.interject_with (fun () ->
                       Fmt.epr "  FAIL %s: %s@." p.pkg (Printexc.to_string exn)));
                incr completed;
                report (1, Fmt.str "[%s] %s" stage_s p.pkg)
              end)
            group.packages)
        plan.groups);
  let n_failed = Hashtbl.length failed_pkgs in
  if n_failed > 0 then Error.msg "%d package(s) failed to build" n_failed
