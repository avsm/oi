let ( / ) = Filename.concat
let log_src = Logs.Src.create "d10ir.direct"

module Log = (val Logs.src_log log_src : Logs.LOG)

type phase =
  | Stage_deps
  | Unpack_archive
  | Apply_substs
  | Snapshot_pre
  | Run_script
  | Apply_install_file
  | Run_docs
  | Diff_layer
  | Store_layer

let string_of_phase = function
  | Stage_deps -> "stage-deps"
  | Unpack_archive -> "unpack"
  | Apply_substs -> "subst"
  | Snapshot_pre -> "snapshot"
  | Run_script -> "build"
  | Apply_install_file -> "install"
  | Run_docs -> "docs"
  | Diff_layer -> "diff"
  | Store_layer -> "store"

type event =
  | Plan_started of { total : int }
  | Plan_done of { built : int; cached : int; failed : int; skipped : int }
  | Node_queued of { node : Plan.node }
  | Node_started of { node : Plan.node }
  | Node_phase of { node : Plan.node; phase : phase }
  | Node_cached of { node : Plan.node }
  | Node_built of { node : Plan.node; duration_s : float; log_path : string }
  | Node_failed of {
      node : Plan.node;
      phase : phase;
      log_path : string;
      error : string;
      duration_s : float;
    }
  | Node_skipped of { node : Plan.node; reason : string }

type reporter = { event : event -> unit }

let null_reporter = { event = (fun _ -> ()) }

type failure = {
  package : Plan.package;
  phase : phase;
  log_path : string;
  error : string;
}

type result = {
  built : int;
  cached : int;
  failed : int;
  skipped : int;
  failures : failure list;
}

(* Strip the verbose [Eio.Io Process Child_error Exited (code N), running
   command: "…"] decoration around what's basically just an exit code.
   Keeps the exit number; replaces the rest with the prefix [exit N].
   Falls back to the raw string if the pattern doesn't match. *)
let tidy_error_string s =
  let trim_after_newline s =
    match String.index_opt s '\n' with
    | None -> String.trim s
    | Some i -> String.trim (String.sub s 0 i)
  in
  let prefix = "Eio.Io Process Child_error Exited (code " in
  let plen = String.length prefix in
  if String.length s >= plen && String.sub s 0 plen = prefix then
    let rest = String.sub s plen (String.length s - plen) in
    match String.index_opt rest ')' with
    | Some i ->
        let code = String.sub rest 0 i in
        Fmt.str "exit %s" code
    | None -> trim_after_newline s
  else trim_after_newline s

let pp_failure ppf (f : failure) =
  Fmt.pf ppf "%s.%s  phase=%s  log=%s@,    %s" f.package.name f.package.version
    (string_of_phase f.phase) f.log_path f.error

let pp_failures ppf = function
  | [] -> ()
  | fs ->
      Fmt.pf ppf "@[<v>Build failures (%d):@,%a@]" (List.length fs)
        Fmt.(list ~sep:cut (fun ppf f -> Fmt.pf ppf "  %a" pp_failure f))
        fs

(* ---- Path helpers ----------------------------------------------------- *)

let cache_root_native (d10 : D10.Config.t) = Eio.Path.native_exn d10.root

let staging_dir_for d10 (n : Plan.node) =
  cache_root_native d10 / "build" / "staging"
  / Layer_hash.to_string n.layer_hash

let build_dir_for d10 (n : Plan.node) =
  cache_root_native d10 / "build" / "_build"
  / Fmt.str "%s.%s-%s" n.package.name n.package.version
      (let h = Layer_hash.to_string n.layer_hash in
       String.sub h 0 (min 8 (String.length h)))

let log_dir_for ~(config : Config.t) ~d10 =
  match config.log_dir with
  | Some d -> d
  | None -> cache_root_native d10 / "build" / "logs"

let log_path_for ~config ~d10 (n : Plan.node) =
  let h = Layer_hash.to_string n.layer_hash in
  let short = String.sub h 0 (min 8 (String.length h)) in
  log_dir_for ~config ~d10
  / Fmt.str "%s.%s-%s.log" n.package.name n.package.version short

(* ---- String rebasing -------------------------------------------------- *)

let str_replace ~from_str ~to_str s =
  if from_str = "" || from_str = to_str then s
  else
    let buf = Buffer.create (String.length s) in
    let n = String.length s in
    let m = String.length from_str in
    let i = ref 0 in
    while !i < n do
      if !i + m <= n && String.sub s !i m = from_str then begin
        Buffer.add_string buf to_str;
        i := !i + m
      end
      else begin
        Buffer.add_char buf s.[!i];
        incr i
      end
    done;
    Buffer.contents buf

(* ---- Per-node procedure ----------------------------------------------- *)

let succeeded d10 hash =
  D10.Layer.exists d10 ~hash:(Layer_hash.to_string hash)
  && D10.Layer.succeeded d10 ~hash:(Layer_hash.to_string hash)

let skeleton ~fs staging =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / staging);
  List.iter
    (fun sub ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / staging / sub))
    [ "bin"; "lib"; "sbin"; "share"; "etc"; "doc"; "man" ]

let transitive_dep_layers ~producers d10 (n : Plan.node) =
  let seen = Hashtbl.create 32 in
  let queue = Queue.create () in
  let push h =
    let key = Layer_hash.to_string h in
    if not (Hashtbl.mem seen key) then begin
      Hashtbl.add seen key ();
      Queue.add h queue
    end
  in
  let result = ref [] in
  List.iter push n.dep_layer_hashes;
  while not (Queue.is_empty queue) do
    let h = Queue.pop queue in
    result := h :: !result;
    let next_deps =
      match Hashtbl.find_opt producers (Layer_hash.to_string h) with
      | Some (m : Plan.node) -> m.dep_layer_hashes
      | None -> (
          let json_path =
            D10.Layer.json_path d10 ~hash:(Layer_hash.to_string h)
          in
          match D10.Layer.load_meta json_path with
          | Some meta -> List.map Layer_hash.of_string meta.hashes
          | None -> [])
    in
    List.iter push next_deps
  done;
  List.rev !result

let stage_dependencies ~producers ~fs d10 (n : Plan.node) staging =
  Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / staging);
  skeleton ~fs staging;
  let all = transitive_dep_layers ~producers d10 n in
  Log.debug (fun m ->
      m "%s.%s: staging %d transitive dep layers into %s" n.package.name
        n.package.version (List.length all) staging);
  List.iter
    (fun h ->
      if succeeded d10 h then
        D10.Layer.restore d10 ~hash:(Layer_hash.to_string h) ~prefix:staging)
    all

let resolve_archive_path ~plan_dir ~archive_root (a : Archive.t) =
  let with_root =
    if Filename.is_relative a.path then archive_root / a.path else a.path
  in
  if Filename.is_relative with_root then plan_dir / with_root else with_root

let unpack_archive ~proc_mgr ~fs ~plan_dir ~archive_root (n : Plan.node)
    ~build_dir =
  Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / build_dir);
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / build_dir);
  let archive_abs = resolve_archive_path ~plan_dir ~archive_root n.archive in
  if not (Sys.file_exists archive_abs) then
    Fmt.failwith "archive missing: %s" archive_abs;
  let strip = string_of_int n.archive.strip_components in
  Eio.Process.run proc_mgr
    [
      "tar";
      "-x";
      "-f";
      archive_abs;
      "-C";
      build_dir;
      "--strip-components";
      strip;
    ]

let parse_kv_entry s =
  match String.index_opt s '=' with
  | None -> None
  | Some i ->
      Some (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

let apply_substs ~rebase (n : Plan.node) ~build_dir =
  let assoc = List.filter_map parse_kv_entry n.subst_vars in
  let env (var : OpamVariable.Full.t) =
    let key = OpamVariable.Full.to_string var in
    match List.assoc_opt key assoc with
    | Some s -> Some (OpamTypes.S (rebase s))
    | None -> None
  in
  List.iter
    (fun base ->
      let src = build_dir / (base ^ ".in") in
      let dst = build_dir / base in
      if Sys.file_exists src then
        OpamFilter.expand_interpolations_in_file_full env
          ~src:(OpamFilename.of_string src)
          ~dst:(OpamFilename.of_string dst))
    n.substs

let starts_with_key ~k entry =
  let kl = String.length k in
  let el = String.length entry in
  el > kl && String.sub entry 0 kl = k && entry.[kl] = '='

(* Strip from [acc] any entry whose key matches one carried by
   [overrides]. [overrides] is a flat list of "K=V" entries; we extract
   the K of each. Used by both [config.inject_env] (executor policy)
   and the per-recipe mount env (which overrides anything the recipe
   declared for the same key). *)
let strip_overridden ~overrides acc =
  let keys =
    List.filter_map
      (fun e ->
        match String.index_opt e '=' with
        | Some i -> Some (String.sub e 0 i)
        | None -> None)
      overrides
  in
  List.fold_left
    (fun acc k -> List.filter (fun e -> not (starts_with_key ~k e)) acc)
    acc keys

(* Append the host's PATH to the recipe's PATH (which is restricted to
   oi-owned prefixes plus a hard-coded FHS list by [Recipe_emitter.scrub_path]
   — see that module for why the recipe doesn't bake the host PATH in).
   Without this, the script can't reach platform-specific tool prefixes that
   live outside FHS: most notably [/opt/homebrew/bin] on macOS, where
   [pkg-config], [m4], [autoconf], and other depext-installed tools live.
   Appending (rather than prepending) keeps oi-owned bins ahead of any
   user-installed shadow like [~/.opam/<switch>/bin]. Dedupes so a host
   PATH that happens to contain the same FHS entries doesn't blow up
   resolution time. Other env vars are NOT inherited — they come from
   [n.env] as the recipe declared them. *)
let dedup_path_entries entries =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun e ->
      if e = "" || Hashtbl.mem seen e then false
      else begin
        Hashtbl.replace seen e ();
        true
      end)
    entries

let augment_path_with_host entry =
  let prefix = "PATH=" in
  if not (String.length entry >= 5 && String.sub entry 0 5 = prefix) then entry
  else
    let recipe = String.sub entry 5 (String.length entry - 5) in
    let host = try Sys.getenv "PATH" with Not_found -> "" in
    if host = "" then entry
    else
      let parts =
        dedup_path_entries
          (String.split_on_char ':' recipe @ String.split_on_char ':' host)
      in
      "PATH=" ^ String.concat ":" parts

let merge_env ~(config : Config.t) ~rebase ~(mount_env : string list)
    (n : Plan.node) =
  let rebased = List.map rebase n.env in
  let with_host_path = List.map augment_path_with_host rebased in
  let after_mounts =
    strip_overridden ~overrides:mount_env with_host_path @ mount_env
  in
  let stripped =
    List.fold_left
      (fun acc (k, _) -> List.filter (fun e -> not (starts_with_key ~k e)) acc)
      after_mounts config.inject_env
  in
  let extras = List.map (fun (k, v) -> Fmt.str "%s=%s" k v) config.inject_env in
  Array.of_list (stripped @ extras)

let run_script ~proc_mgr ~fs ~config ~d10 ~mount_env (n : Plan.node) ~staging
    ~build_dir =
  let log_path = log_path_for ~config ~d10 n in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
    Eio.Path.(fs / Filename.dirname log_path);
  let rebase = str_replace ~from_str:n.prefix ~to_str:staging in
  let script = rebase n.script in
  let env = merge_env ~config ~rebase ~mount_env n in
  let cwd = Eio.Path.(fs / build_dir) in
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) Eio.Path.(fs / log_path)
  @@ fun log_sink ->
  let log_pre =
    let env_lines =
      Array.to_list env
      |> List.map (fun e -> Fmt.str "#   %s" e)
      |> String.concat "\n"
    in
    Fmt.str
      "# d10ir.direct %s.%s\n\
       # layer_hash %s\n\
       # env:\n\
       %s\n\
       # script:\n\
       %s\n\
       # end\n"
      n.package.name n.package.version
      (Layer_hash.to_string n.layer_hash)
      env_lines script
  in
  Eio.Flow.copy_string log_pre log_sink;
  try
    Eio.Process.run proc_mgr ~env ~cwd
      ~stdout:(log_sink :> Eio.Flow.sink_ty Eio.Resource.t)
      ~stderr:(log_sink :> Eio.Flow.sink_ty Eio.Resource.t)
      [ "/bin/sh"; "-e"; "-c"; script ];
    log_path
  with exn ->
    Eio.Flow.copy_string
      (Fmt.str "\n# script failed: %s\n" (Printexc.to_string exn))
      log_sink;
    raise exn

let maybe_apply_install_file ~fs (n : Plan.node) ~staging ~build_dir =
  let install_basename = n.package.name ^ ".install" in
  let install_path = build_dir / install_basename in
  if Sys.file_exists install_path then
    Install_file.apply ~fs ~prefix:staging ~build_dir ~install_file:install_path

let store_layer ~d10 (n : Plan.node) ~staging ~files =
  let dep_hashes_str =
    List.map Layer_hash.to_string n.dep_layer_hashes
    |> List.filter (fun h -> D10.Layer.succeeded d10 ~hash:h)
  in
  let pkg_str = Fmt.str "%s.%s" n.package.name n.package.version in
  (* Recipe info (script + env + substs) now ships inside the unified
     layer manifest sidecar written by [Oi.Build_pipeline] — at
     [<cache>/registry/<os_key>/layers/<hash>.json]. No need to write
     a separate per-layer-dir [recipe.json] any more. *)
  D10.Layer.store d10
    ~hash:(Layer_hash.to_string n.layer_hash)
    ~prefix:staging ~files ~package:pkg_str ~deps:dep_hashes_str
    ~parent_hashes:dep_hashes_str ~exit_status:0 ()

(* ---- Doc generation step (voodoo) ------------------------------------- *)

let doc_log_path_for ~config ~d10 (n : Plan.node) =
  let h = Layer_hash.to_string n.layer_hash in
  let short = String.sub h 0 (min 8 (String.length h)) in
  log_dir_for ~config ~d10
  / Fmt.str "%s.%s-%s.doc.log" n.package.name n.package.version short

(* Fixed universe name for the prep tree's [universes/<U>/] directory.
   voodoo infers the universe from the tree shape at runtime, so the string
   only matters as the directory name — [--blessed] output paths ignore it
   entirely. *)
let voodoo_universe = "oi"

(* Hardlink each install-delta file from [staging] into
   [<prep_root>/prep/universes/oi/<pkg>/<ver>/<rel>], the layout voodoo's
   hard-coded [prep_path = "prep"] expects relative to its CWD. No copy
   fallback: [staging] and [prep_root] live under the same d10 cache root,
   so a same-FS [Unix.link] always succeeds. An exotic cache layout that
   crosses filesystems would raise [Unix_error EXDEV], which bubbles up to
   {!maybe_run_docs}' warn-continue handler. *)
let materialise_prep_tree ~fs ~staging ~prep_root ~pkg_name ~pkg_ver
    ~install_delta =
  let pkg_root =
    prep_root / "prep" / "universes" / voodoo_universe / pkg_name / pkg_ver
  in
  List.iter
    (fun (rel_path, _abs) ->
      let src = staging / rel_path in
      let dst = pkg_root / rel_path in
      if Sys.file_exists src && not (Sys.is_directory src) then begin
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
          Eio.Path.(fs / Filename.dirname dst);
        Unix.link src dst
      end)
    install_delta

(* Run [odoc_driver_voodoo] with [cwd = prep_root] so [./prep/...] resolves,
   and absolute paths for outputs and helper binaries so they land in /
   come from the staging dir. PATH includes [doc_tools_dir/bin] so
   voodoo's runtime [sherlodoc] lookup succeeds. *)
let run_voodoo ~proc_mgr ~fs ~doc_tools_dir ~prep_root ~staging ~pkg_name
    ~log_path =
  let bin name = doc_tools_dir / "bin" / name in
  let argv =
    [
      bin "odoc_driver_voodoo";
      pkg_name;
      "--blessed";
      "--odoc-dir=" ^ (staging / "odoc" / "odoc");
      "--html-dir=" ^ (staging / "odoc" / "html");
      "--odoc=" ^ bin "odoc";
      "--odoc-md=" ^ bin "odoc-md";
    ]
  in
  let home =
    match Sys.getenv_opt "HOME" with Some h when h <> "" -> h | _ -> "/"
  in
  let env =
    [|
      "PATH=" ^ (doc_tools_dir / "bin") ^ ":/usr/bin:/bin";
      "HOME=" ^ home;
    |]
  in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
    Eio.Path.(fs / Filename.dirname log_path);
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / prep_root);
  let cwd = Eio.Path.(fs / prep_root) in
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) Eio.Path.(fs / log_path)
  @@ fun log_sink ->
  let env_lines =
    Array.to_list env
    |> List.map (fun e -> "#   " ^ e)
    |> String.concat "\n"
  in
  Eio.Flow.copy_string
    (Fmt.str
       "# d10ir.direct doc step %s\n# argv: %s\n# cwd: %s\n# env:\n%s\n"
       pkg_name (String.concat " " argv) prep_root env_lines)
    log_sink;
  try
    Eio.Process.run proc_mgr ~env ~cwd
      ~stdout:(log_sink :> Eio.Flow.sink_ty Eio.Resource.t)
      ~stderr:(log_sink :> Eio.Flow.sink_ty Eio.Resource.t)
      argv
  with exn ->
    Eio.Flow.copy_string
      (Fmt.str "# voodoo failed: %s\n" (Printexc.to_string exn))
      log_sink;
    raise exn

(* Best-effort doc step. No-op when [config.doc_tools_dir = None] (docs
   off). Failures — binary missing, voodoo crash, prep materialisation
   error — warn and let the build proceed without docs for this package;
   downstream cross-refs degrade gracefully. *)
let maybe_run_docs ~(config : Config.t) ~d10 ~fs ~proc_mgr (n : Plan.node)
    ~staging ~before =
  match config.doc_tools_dir with
  | None -> ()
  | Some doc_tools_dir -> (
      let voodoo_bin = doc_tools_dir / "bin" / "odoc_driver_voodoo" in
      if not (Sys.file_exists voodoo_bin) then
        Log.warn (fun m ->
            m "docs: %s.%s: voodoo binary missing at %s; skipping"
              n.package.name n.package.version voodoo_bin)
      else
        try
          let install_delta = D10.Prefix.diff ~fs ~prefix:staging ~before in
          if install_delta = [] then
            Log.info (fun m ->
                m "docs: %s.%s: empty install delta; skipping"
                  n.package.name n.package.version)
          else begin
            let prep_root = build_dir_for d10 n / "_doc_prep" in
            Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / prep_root);
            materialise_prep_tree ~fs ~staging ~prep_root
              ~pkg_name:n.package.name ~pkg_ver:n.package.version
              ~install_delta;
            let log_path = doc_log_path_for ~config ~d10 n in
            run_voodoo ~proc_mgr ~fs ~doc_tools_dir ~prep_root ~staging
              ~pkg_name:n.package.name ~log_path
          end
        with exn ->
          Log.warn (fun m ->
              m "docs: %s.%s failed: %s (continuing)" n.package.name
                n.package.version
                (tidy_error_string (Printexc.to_string exn))))

let cleanup_staging ~fs ~(config : Config.t) staging build_dir =
  if not config.keep_staging then begin
    (try Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / staging)
     with Eio.Exn.Io _ -> ());
    try Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / build_dir)
    with Eio.Exn.Io _ -> ()
  end

let now_s ~clock = Eio.Time.now clock

(* Wrap a phase. Emits [Node_phase] before [body ()] runs; on failure
   re-raises with the [phase] tagged in [Phase_failed] so the outer
   handler can tell the caller which step failed. *)
exception Phase_failed of phase * exn

let with_phase ~reporter (n : Plan.node) phase body =
  reporter.event (Node_phase { node = n; phase });
  try body () with exn -> raise (Phase_failed (phase, exn))

(* Cross-process serialisation is provided by {!Oi.Lock.acquire_global} in
   {!Cmd.Harness.bootstrap}, so [build_one] does not take its own lock; the
   per-hash [staging] / [build_dir] paths only need to be safe against
   in-process scheduling, which the d10ir scheduler handles by not
   dispatching the same node twice. *)
let build_one ~config ~d10 ~fs ~proc_mgr ~clock ~plan_dir ~archive_root
    ~producers ~reporter ~mount_env (n : Plan.node) =
  if succeeded d10 n.layer_hash then `Cached
  else begin
    let staging = staging_dir_for d10 n in
    let build_dir = build_dir_for d10 n in
    let t0 = now_s ~clock in
    try
      reporter.event (Node_started { node = n });
      with_phase ~reporter n Stage_deps (fun () ->
          stage_dependencies ~producers ~fs d10 n staging);
      with_phase ~reporter n Unpack_archive (fun () ->
          unpack_archive ~proc_mgr ~fs ~plan_dir ~archive_root n ~build_dir);
      let rebase = str_replace ~from_str:n.prefix ~to_str:staging in
      with_phase ~reporter n Apply_substs (fun () ->
          apply_substs ~rebase n ~build_dir);
      let before =
        with_phase ~reporter n Snapshot_pre (fun () ->
            D10.Prefix.snapshot ~fs staging)
      in
      let log_path =
        with_phase ~reporter n Run_script (fun () ->
            run_script ~proc_mgr ~fs ~config ~d10 ~mount_env n ~staging
              ~build_dir)
      in
      with_phase ~reporter n Apply_install_file (fun () ->
          maybe_apply_install_file ~fs n ~staging ~build_dir);
      with_phase ~reporter n Run_docs (fun () ->
          maybe_run_docs ~config ~d10 ~fs ~proc_mgr n ~staging ~before);
      let files =
        with_phase ~reporter n Diff_layer (fun () ->
            D10.Prefix.diff ~fs ~prefix:staging ~before |> List.map fst)
      in
      with_phase ~reporter n Store_layer (fun () ->
          store_layer ~d10 n ~staging ~files);
      cleanup_staging ~fs ~config staging build_dir;
      let dt = now_s ~clock -. t0 in
      `Built (log_path, dt)
    with Phase_failed (phase, exn) ->
      (* Build failures are NOT logged as warnings here — they're
         a normal part of any non-trivial build. The Failed event
         carries enough info, the per-node log file has the full
         output, and the run-level summary printed by the caller
         is where the user should see the failure. Logging here
         too would duplicate (during the run) and mid-bar the
         progress UI. *)
      let log_path = log_path_for ~config ~d10 n in
      cleanup_staging ~fs ~config staging build_dir;
      let dt = now_s ~clock -. t0 in
      `Failed (phase, log_path, tidy_error_string (Printexc.to_string exn), dt)
  end

(* ---- Scheduler -------------------------------------------------------- *)

(* Pre-create mount sources on the host and flatten their env entries
   into a single list to be merged into every node. The native
   executor doesn't actually mount anything — making the directory
   exist and exporting [DUNE_CACHE_ROOT] etc. is the entire contract.
   A future sandbox backend (obuilder) replays the same record as a
   real bind mount. *)
let prepare_mounts ~fs (mounts : Plan.mount list) =
  List.iter
    (fun (m : Plan.mount) ->
      try Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / m.source)
      with exn ->
        Log.warn (fun k ->
            k "mount %s: failed to create source %s: %s" m.name m.source
              (Printexc.to_string exn)))
    mounts;
  List.concat_map (fun (m : Plan.mount) -> m.env) mounts

type counts_t =
  < built : unit -> unit
  ; cached : unit -> unit
  ; failed : failure -> unit
  ; skipped : unit -> unit
  ; snapshot : int * int * int * int * failure list >

let new_counts () : counts_t =
  object
    val mutable built = 0
    val mutable cached = 0
    val mutable failed = 0
    val mutable skipped = 0
    val mutable failures : failure list = []
    method built () = built <- built + 1
    method cached () = cached <- cached + 1

    method failed (f : failure) =
      failed <- failed + 1;
      failures <- f :: failures

    method skipped () = skipped <- skipped + 1
    method snapshot = (built, cached, failed, skipped, List.rev failures)
  end

type node_tables = {
  promises : (string, [ `Ok | `Failed | `Skipped ] Eio.Promise.t) Hashtbl.t;
  resolvers : (string, [ `Ok | `Failed | `Skipped ] Eio.Promise.u) Hashtbl.t;
  producer_keys : (string, unit) Hashtbl.t;
  producers : (string, Plan.node) Hashtbl.t;
  external_keys : (string, unit) Hashtbl.t;
}

let build_node_tables (plan : Plan.t) =
  let n_nodes = List.length plan.nodes in
  let promises = Hashtbl.create n_nodes in
  let resolvers = Hashtbl.create n_nodes in
  let producer_keys = Hashtbl.create n_nodes in
  let producers = Hashtbl.create n_nodes in
  List.iter
    (fun (n : Plan.node) ->
      let key = Layer_hash.to_string n.layer_hash in
      let p, r = Eio.Promise.create () in
      Hashtbl.replace promises key p;
      Hashtbl.replace resolvers key r;
      Hashtbl.replace producer_keys key ();
      Hashtbl.replace producers key n)
    plan.nodes;
  (* Layer hashes the host provides (non-relocatable toolchain). Treated
     as already-satisfied: no producer in [plan.nodes], no d10 lookup,
     no [stage_dependencies] copy (the build env's PATH points at the
     host prefix that ships the binaries). *)
  let external_keys = Hashtbl.create (List.length plan.external_layers) in
  List.iter
    (fun h -> Hashtbl.replace external_keys (Layer_hash.to_string h) ())
    plan.external_layers;
  { promises; resolvers; producer_keys; producers; external_keys }

let producer_dep_status ~tables key =
  match Eio.Promise.await (Hashtbl.find tables.promises key) with
  | `Ok -> `Ok
  | `Failed | `Skipped -> `Failed

let one_dep_status ~tables ~d10 h =
  let key = Layer_hash.to_string h in
  if Hashtbl.mem tables.producer_keys key then producer_dep_status ~tables key
  else if Hashtbl.mem tables.external_keys key then `Ok
  else if succeeded d10 h then `Ok
  else `Failed

let dep_status ~tables ~d10 (n : Plan.node) =
  let step acc h =
    match acc with `Failed -> `Failed | `Ok -> one_dep_status ~tables ~d10 h
  in
  List.fold_left step `Ok n.dep_layer_hashes

let resolve_node ~tables (n : Plan.node) v =
  let key = Layer_hash.to_string n.layer_hash in
  Eio.Promise.resolve (Hashtbl.find tables.resolvers key) v

let handle_build_outcome ~counts ~reporter ~bump ~resolve outcome n =
  match outcome with
  | `Cached ->
      bump (fun () -> counts#cached ());
      reporter.event (Node_cached { node = n });
      resolve n `Ok
  | `Built (log_path, dt) ->
      bump (fun () -> counts#built ());
      reporter.event (Node_built { node = n; duration_s = dt; log_path });
      resolve n `Ok
  | `Failed (phase, log_path, error, dt) ->
      let f = { package = n.package; phase; log_path; error } in
      bump (fun () -> counts#failed f);
      reporter.event
        (Node_failed { node = n; phase; log_path; error; duration_s = dt });
      resolve n `Failed

let try_build_node ~config ~d10 ~fs ~proc_mgr ~clock ~plan_dir ~plan ~tables
    ~reporter ~mount_env ~counts ~bump ~resolve ~with_slot n =
  if succeeded d10 n.Plan.layer_hash then begin
    bump (fun () -> counts#cached ());
    reporter.event (Node_cached { node = n });
    resolve n `Ok
  end
  else begin
    let outcome =
      with_slot (fun () ->
          build_one ~config ~d10 ~fs ~proc_mgr ~clock ~plan_dir
            ~archive_root:plan.Plan.archive_root ~producers:tables.producers
            ~reporter ~mount_env n)
    in
    handle_build_outcome ~counts ~reporter ~bump ~resolve outcome n
  end

let pkg_fiber ~config ~d10 ~fs ~proc_mgr ~clock ~plan_dir ~plan ~tables
    ~reporter ~mount_env ~counts ~bump ~with_slot n =
  let resolve = resolve_node ~tables in
  reporter.event (Node_queued { node = n });
  match dep_status ~tables ~d10 n with
  | `Failed ->
      bump (fun () -> counts#skipped ());
      reporter.event (Node_skipped { node = n; reason = "dep failed" });
      resolve n `Skipped
  | `Ok ->
      try_build_node ~config ~d10 ~fs ~proc_mgr ~clock ~plan_dir ~plan ~tables
        ~reporter ~mount_env ~counts ~bump ~resolve ~with_slot n

let run ~(config : Config.t) ~d10 ~fs ~proc_mgr ~clock
    ?(reporter = null_reporter) ?(plan_dir = Sys.getcwd ()) (plan : Plan.t) =
  let mount_env = prepare_mounts ~fs plan.mounts in
  reporter.event (Plan_started { total = List.length plan.nodes });
  let tables = build_node_tables plan in
  let counts = new_counts () in
  let mutex = Eio.Mutex.create () in
  let bump f = Eio.Mutex.use_rw ~protect:false mutex (fun () -> f ()) in
  let build_sem = Eio.Semaphore.make config.build_parallelism in
  let with_slot f =
    Eio.Semaphore.acquire build_sem;
    Fun.protect ~finally:(fun () -> Eio.Semaphore.release build_sem) f
  in
  let run_pkg n =
    pkg_fiber ~config ~d10 ~fs ~proc_mgr ~clock ~plan_dir ~plan ~tables
      ~reporter ~mount_env ~counts ~bump ~with_slot n
  in
  Eio.Switch.run (fun sw ->
      List.iter (fun n -> Eio.Fiber.fork ~sw (fun () -> run_pkg n)) plan.nodes);
  let built, cached, failed, skipped, failures = counts#snapshot in
  reporter.event (Plan_done { built; cached; failed; skipped });
  { built; cached; failed; skipped; failures }
