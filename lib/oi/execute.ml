[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

(** Stage-based parallel build executor. *)

let log_src = Logs.Src.create "oi.execute"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

(* Cap concurrent package builds. Each in-flight build spawns subprocess
   pipes (2 fds per capture) plus transient file descriptors for fetch
   and patch, and each build then recursively spawns compiler processes
   of its own, so the fd tree fans out fast; large stages would exhaust
   macOS's default 256 soft [rlim]. Resolution order: explicit [?jobs]
   argument to {!run} wins, then [OI_BUILD_PARALLELISM] env var, then
   [min (cpu_count) 4]. *)
let default_build_parallelism () =
  match Sys.getenv_opt "OI_BUILD_PARALLELISM" with
  | Some s -> (
      match int_of_string_opt s with Some n when n > 0 -> n | _ -> 4)
  | None -> min (Domain.recommended_domain_count ()) 4

(* -- Failure logging ----------------------------------------------------- *)

(* Failed package builds get their output written to a file under the
   shared [<cache_root>/build/logs] dir instead of dumped to stderr.
   Keeps the live progress bar / summary readable and gives the user
   a file path to grep without re-running the build. The file name
   mirrors the [build_dir] convention: [build-<pkg>-<short_hash>.log]
   so the same [name.version] built in two different solve contexts
   doesn't collide. *)
let write_failure_log ~fs ~cache_root ~(p : Plan.package_plan) ~exn =
  let path =
    Build_logs.path ~cache_root ~kind:"build" ~name:p.pkg ~hash:p.layer_hash
  in
  let body =
    match exn with
    | Error.E (Build_failed { pkg; cmd; output }) ->
        Fmt.str "package: %s\ncommand: %s\n\n%s" pkg cmd output
    | _ -> Printexc.to_string exn
  in
  Build_logs.write ~fs ~cache_root path body;
  path

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
        String.split_on_char ':' path
        |> List.find_map (function
          | "" -> None
          | d ->
              let candidate = d / exe in
              if is_executable candidate then Some candidate else None)
        |> Stdlib.Option.value ~default:exe

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

(* Per-fetch retry log so retry warnings go to a file instead of stderr.
   Retry.with_attempts opens the file in append mode on each retry, so
   the caller just has to supply the path and make sure the parent
   [logs/] directory exists. *)
let fetch_log_path_of ~cache_root ~(p : Plan.package_plan) =
  Build_logs.path ~cache_root ~kind:"fetch" ~name:p.pkg ~hash:p.layer_hash

let fetch_source ?(cache_urls = []) ~fs ~cache_root (p : Plan.package_plan) =
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
        let error_log_path = fetch_log_path_of ~cache_root ~p in
        Build_logs.ensure ~fs ~cache_root;
        Retry.with_attempts ~label:(Fmt.str "fetch %s (%s)" p.pkg src.url)
          ~error_log_path (fun () ->
            let result =
              OpamRepository.pull_tree p.pkg ~cache_dir ~cache_urls dst_dir
                checksums [ url ]
              |> OpamProcess.Job.run
            in
            match result with
            | OpamTypes.Result _ | OpamTypes.Up_to_date _ -> ()
            | OpamTypes.Not_available (_, msg) ->
                Fmt.failwith "Failed to fetch %s: %s" p.pkg msg)
      end

let fetch_extra_sources ?(cache_urls = []) ~fs ~cache_root
    (p : Plan.package_plan) =
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
        let error_log_path = fetch_log_path_of ~cache_root ~p in
        Build_logs.ensure ~fs ~cache_root;
        try
          Retry.with_attempts
            ~label:(Fmt.str "fetch extra source %s (%s)" name src.url)
            ~error_log_path (fun () ->
              let result =
                OpamRepository.pull_file name ~cache_dir ~cache_urls
                  ~silent_hits:true dst_file checksums [ url ]
                |> OpamProcess.Job.run
              in
              match result with
              | OpamTypes.Result () | OpamTypes.Up_to_date () -> ()
              | OpamTypes.Not_available (_, msg) -> Fmt.failwith "%s" msg)
        with Failure msg ->
          (* Match the previous semantics: extra sources are
             best-effort, so a hard failure (after retries) downgrades
             to a warning rather than aborting the whole build. *)
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

let build_package ?(cache_urls = []) ~cache_root ~proc_mgr ~fs
    (p : Plan.package_plan) =
  fetch_source ~cache_urls ~fs ~cache_root p;
  (* Ensure build_dir exists before fetching extra-sources: pull_tree
     (in fetch_source) creates the directory, but packages with no main
     source (e.g. seq.base) still need the directory to exist so that
     extra-source files can be written into it. *)
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / p.build_dir);
  fetch_extra_sources ~cache_urls ~fs ~cache_root p;
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

(* -- Reporter ------------------------------------------------------------- *)

type pkg_event =
  | Started of { pkg : string; stage : int; total_stages : int }
  | Cached of { pkg : string }
  | Built of { pkg : string }
  | Build_failed of { pkg : string; log : string }
  | Dep_failed of { pkg : string; upstream_log : string }
  | Install_failed of { pkg : string; log : string }

type reporter = { pkg_event : pkg_event -> unit }

(* -- Main loop ------------------------------------------------------------ *)

(* Default reporter: internal Progress bar + inline FAIL prints. Used by
   [oi run] / [oi sync], which only ever run one [Execute.run] at a time
   and have no outer UI to coordinate with. [oi registry build] supplies
   its own reporter to drive a cross-invocation bar with live counters. *)
let with_default_reporter ~total_packages ~n_stages f =
  let config = Progress.Config.v ~persistent:false () in
  let bar =
    let open Progress.Line in
    pair ~sep:(const " ")
      (list [ spinner (); brackets (count_to total_packages) ])
      (rpad 60 string)
  in
  Progress.with_reporter ~config bar (fun report ->
      let stage_s stage = Fmt.str "stage %d/%d" stage n_stages in
      let reporter =
        {
          pkg_event =
            (fun e ->
              match e with
              (* [Started] is the "now working on X" signal — show the
                 package on the bar so the user sees what's in flight,
                 without ticking the completion counter. *)
              | Started { pkg; stage; total_stages = _ } ->
                  report (0, Fmt.str "[%s] %s" (stage_s stage) pkg)
              (* Terminal events only tick the counter; the label is
                 left at whatever [Started] last set, so the bar always
                 reads as the currently-in-flight job rather than the
                 last one that finished. *)
              | Cached _ | Built _ -> report (1, "")
              | Dep_failed _ -> ()
              | Build_failed { pkg; log } ->
                  Progress.interject_with (fun () ->
                      Fmt.epr "  %a %s → %s@."
                        Fmt.(styled (`Fg `Red) string)
                        "FAIL" pkg log)
              | Install_failed { pkg; log } ->
                  Progress.interject_with (fun () ->
                      Fmt.epr "  %a %s (install) → %s@."
                        Fmt.(styled (`Fg `Red) string)
                        "FAIL" pkg log));
        }
      in
      f reporter)

let run ?(cache_urls = []) ?jobs ?failed_layers ?reporter ~proc_mgr ~fs ~clock
    ~sys ~os_key plan =
  let build_parallelism =
    match jobs with Some n when n > 0 -> n | _ -> default_build_parallelism ()
  in
  let d10 : D10.Config.t =
    { sys; fs; clock; root = Eio.Path.(fs / plan.Plan.cache_root); os_key }
  in
  let prefix = plan.cache_root / "build" / "prefix" in
  let n_stages = List.length plan.groups in
  (* Track failed layer-hashes rather than package names so
     cross-overlay builds of the same [name.version] (which resolve to
     different layer hashes) stay independent. The optional arg lets
     [oi registry build --all] thread one tracker through every build
     group, so a failure in group 1 skips dependents in group 2
     rather than retrying the same build. Keyed by layer hash; value
     is the failure-log path (empty string for cascaded failures that
     inherit the log from an upstream dep). *)
  let failed_layers : (string, string) Hashtbl.t =
    match failed_layers with Some t -> t | None -> Hashtbl.create 16
  in
  (* Snapshot the pre-run count so the end-of-run raise only fires for
     failures introduced BY THIS CALL. Without this, every group after
     the first failure in an [--all] run re-reports that failure as
     its own (Execute.run would raise on each subsequent call because
     [failed_layers] still carries the earlier entry). *)
  let failed_count_before = Hashtbl.length failed_layers in
  let mark_failed ~(p : Plan.package_plan) ~log_path =
    if not (Hashtbl.mem failed_layers p.layer_hash) then
      Hashtbl.replace failed_layers p.layer_hash log_path
  in
  let is_dep_failed (d : Plan.dep_layer) = Hashtbl.mem failed_layers d.hash in
  (* First dep's log path, for cascading skip messages. *)
  let cascade_log (p : Plan.package_plan) =
    List.find_map
      (fun (d : Plan.dep_layer) -> Hashtbl.find_opt failed_layers d.hash)
      p.dep_layers
    |> Stdlib.Option.value ~default:""
  in
  (* A plan is "doomed" when every source package in it is already in
     [failed_layers] or has a failed dep, so no Source build will run.
     In that case every Binary in the plan would be restored into the
     prefix purely to emit [Cached] events — pure IO waste. Detect it
     up front and short-circuit: emit Cached / Dep_failed events for
     the reporter's accounting, mark sources as failed, and skip every
     prefix wipe / layer restore. This is the common case when a
     common dep fails early in [--all] and drags hundreds of groups
     with it. *)
  let is_pkg_doomed (p : Plan.package_plan) =
    p.method_ <> `Source
    || Hashtbl.mem failed_layers p.layer_hash
    || List.exists is_dep_failed p.dep_layers
  in
  let plan_doomed =
    List.for_all
      (fun (g : Plan.group) -> List.for_all is_pkg_doomed g.packages)
      plan.groups
  in
  if not plan_doomed then begin
    Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / prefix);
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / prefix);
    List.iter
      (fun sub ->
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
          Eio.Path.(fs / prefix / sub))
      [ "bin"; "lib"; "sbin"; "share"; "etc"; "doc"; "man" ];
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
      Eio.Path.(fs / plan.cache_root / "build" / "_build")
  end;
  let do_work (reporter : reporter) =
    if plan_doomed then
      List.iter
        (fun (group : Plan.group) ->
          List.iter
            (fun (p : Plan.package_plan) ->
              match p.method_ with
              | `Binary -> reporter.pkg_event (Cached { pkg = p.pkg })
              | `Source ->
                  let upstream_log = cascade_log p in
                  mark_failed ~p ~log_path:upstream_log;
                  reporter.pkg_event
                    (Dep_failed { pkg = p.pkg; upstream_log }))
            group.packages)
        plan.groups
    else
    List.iter
      (fun (group : Plan.group) ->
        (* Phase 1: restore cached layers (Binary packages). Emit a
           [Started] before each restore so the progress bar label
           shows the package currently being restored, not the one
           that just finished. *)
        List.iter
          (fun (p : Plan.package_plan) ->
            if p.method_ = `Binary then begin
              reporter.pkg_event
                (Started
                   { pkg = p.pkg; stage = group.stage; total_stages = n_stages });
              D10.Layer.restore d10 ~hash:p.layer_hash ~prefix;
              reporter.pkg_event (Cached { pkg = p.pkg })
            end)
          group.packages;
        (* Phase 2: parallel fetch+build for Source packages. Skip
           a package when its own layer has already failed in a
           previous build group (persistent tracker), or when any
           of its dep layers is known-failed. In either case we
           mark the package's own layer as failed so its dependents
           propagate the skip downstream and emit a Dep_failed
           event for the reporter. *)
        let to_build =
          List.filter
            (fun (p : Plan.package_plan) ->
              if
                p.method_ = `Source
                && not (Hashtbl.mem failed_layers p.layer_hash)
                && not (List.exists is_dep_failed p.dep_layers)
              then true
              else begin
                (if p.method_ = `Source then
                   let upstream_log = cascade_log p in
                   mark_failed ~p ~log_path:upstream_log;
                   reporter.pkg_event
                     (Dep_failed { pkg = p.pkg; upstream_log }));
                false
              end)
            group.packages
        in
        if to_build <> [] then begin
          let active = ref 0 in
          Eio.Fiber.List.iter ~max_fibers:build_parallelism
            (fun (p : Plan.package_plan) ->
              active := !active + 1;
              reporter.pkg_event
                (Started
                   { pkg = p.pkg; stage = group.stage; total_stages = n_stages });
              (try
                 Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / p.build_dir);
                 build_package ~cache_urls ~cache_root:plan.cache_root
                   ~proc_mgr ~fs p
               with exn ->
                 let log_path =
                   write_failure_log ~fs ~cache_root:plan.cache_root ~p ~exn
                 in
                 mark_failed ~p ~log_path;
                 reporter.pkg_event
                   (Build_failed { pkg = p.pkg; log = log_path }));
              active := !active - 1)
            to_build
        end;
        (* Phase 3: serial install for successfully built packages.
           Emit [Started] before each install so the bar label tracks
           the in-flight package. *)
        List.iter
          (fun (p : Plan.package_plan) ->
            if
              p.method_ = `Source
              && not (Hashtbl.mem failed_layers p.layer_hash)
            then begin
              reporter.pkg_event
                (Started
                   { pkg = p.pkg; stage = group.stage; total_stages = n_stages });
              let before = D10.Prefix.snapshot ~fs prefix in
              try
                install_package ~proc_mgr ~fs p;
                let files =
                  D10.Prefix.diff ~fs ~prefix ~before |> List.map fst
                in
                let dep_hashes =
                  List.filter_map
                    (fun (d : Plan.dep_layer) ->
                      if D10.Layer.succeeded d10 ~hash:d.hash then Some d.hash
                      else None)
                    p.dep_layers
                in
                D10.Layer.store d10 ~hash:p.layer_hash ~prefix ~files
                  ~package:p.pkg
                  ~deps:
                    (List.map (fun (d : Plan.dep_layer) -> d.pkg) p.dep_layers)
                  ~parent_hashes:dep_hashes ~exit_status:0
                  ?overlay_handle:p.overlay_handle
                  ?overlay_version:p.overlay_version ();
                reporter.pkg_event (Built { pkg = p.pkg })
              with exn ->
                let log_path =
                  write_failure_log ~fs ~cache_root:plan.cache_root ~p ~exn
                in
                mark_failed ~p ~log_path;
                reporter.pkg_event
                  (Install_failed { pkg = p.pkg; log = log_path })
            end)
          group.packages)
      plan.groups
  in
  (match reporter with
  | Some r -> do_work r
  | None ->
      with_default_reporter ~total_packages:plan.total_packages ~n_stages do_work);
  let n_failed = Hashtbl.length failed_layers - failed_count_before in
  if n_failed > 0 then Error.msg "%d package(s) failed to build" n_failed
