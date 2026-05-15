let ( / ) = Filename.concat

type env = {
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  sys : D10.Sysops.t;
  platform : Osrel.t;
  os_key : string;
  cache : Oi.Cache.t;
  data_dir : string;
  format : Terms.format;
  http_session : D10.Sysops.Http.session;
}

(* The resolved target a command wants to hand control to once it has
   nothing left to do but run it. Carries only plain data — no Eio
   resources — so it stays valid after the Eio scheduler has shut down. *)
type target = { env : string array; argv : string list }

(* Raised by {!exec} from deep in a command body as a non-local return:
   "the cache is fully staged; the only thing left is to run [argv]".
   The command body catches it itself and returns [Some target] out of
   {!run}; the spawn then happens in the cmd layer, {b after} {!run} has
   returned and the Harness switch has torn down.

   That switch owns the process-wide cache lock fd
   ([<data_dir>/.oi.lock], see {!Oi.Lock.acquire_global}); ordinary
   teardown closes it and releases the lock. Spawning the target from
   the cmd layer therefore runs it with no oi cache lock held, so a
   long-lived / interactive target ([oi run utop]) no longer blocks
   every other [oi] invocation for its whole lifetime. The locking
   semantics are unchanged — only where we spawn moves. *)
exception Exec of target

let exec ~env argv = raise (Exec { env; argv })

(* Set by {!bootstrap} from [c.format] before the command body runs;
   read by {!with_error_handling} when an exception bubbles out. The
   ref defaults to [Text] so cmdliner-time errors (before bootstrap)
   still print human text. *)
let error_format : Terms.format ref = ref Terms.Text

let env_var k =
  match Sys.getenv_opt k with Some v when v <> "" -> Some v | _ -> None

let data_dir_of_env () =
  match env_var "OI_DATA_DIR" with
  | Some v -> v
  | None -> (
      match env_var "XDG_DATA_HOME" with
      | Some v -> v / "oi"
      | None -> (
          match env_var "HOME" with
          | Some h -> h / ".local" / "share" / "oi"
          | None -> "/tmp/oi"))

(* Mirror of [Terms.data_dir]'s env resolution for callers that don't
   thread a [--data-dir] flag (e.g. [oi cache *]). *)
let resolve_data_dir ?override () =
  match override with Some v when v <> "" -> v | _ -> data_dir_of_env ()

let pp_one_exn fmt = function
  | Oi.Error.E e -> Oi.Error.pp fmt e
  | Failure msg -> Fmt.pf fmt "%a %s" Oi.Style.pp_error_string "error:" msg
  | e ->
      Fmt.pf fmt "%a %s" Oi.Style.pp_error_string "error:"
        (Printexc.to_string e)

let rec is_interrupt = function
  | Oi.Signals.Interrupted | Sys.Break -> true
  | Eio.Cancel.Cancelled e -> is_interrupt e
  | Eio.Exn.Io _ -> false
  | _ -> false

(* Lift any non-interrupt exception into an [Oi.Error.t] so the same
   {!Oi.Error.code} / {!Oi.Error.to_json} pipeline handles every exit
   path. [Failure] becomes a generic Msg; everything else (an
   unexpected internal exception) becomes a generic Msg too — the
   shape we want is "an error occurred", with the printed text
   already covered by [pp_one_exn]. *)
let error_of_exn = function
  | Oi.Error.E e -> e
  | Failure msg -> Oi.Error.Msg msg
  | exn -> Oi.Error.Msg (Printexc.to_string exn)

(* All top-level prints route through [Logs_progress.interject] in
   case any [Progress.Display] is still alive on the active-display
   slot when an exception escapes the inner [Fun.protect]s. The slot
   should have been cleared before reaching here, but defensive
   pause/resume here means a stray uncaught exception during
   tear-down doesn't leave the error message buried inside a half-
   rendered bar. *)
let eprintf_raw fmt = Fmt.kstr (fun s -> Fmt.epr "%s@." s) fmt

let eprintf fmt =
  Fmt.kstr (fun s -> Logs_progress.interject (fun () -> eprintf_raw "%s" s)) fmt

let exit_for_exn exn =
  match !error_format with
  | Json ->
      Logs_progress.interject (fun () ->
          print_string (Oi.Error.to_json (error_of_exn exn)));
      exit (Oi.Error.code (error_of_exn exn))
  | Text ->
      eprintf "%a" pp_one_exn exn;
      exit (Oi.Error.code (error_of_exn exn))

let with_error_handling f =
  try f () with
  | exn when is_interrupt exn ->
      eprintf "Interrupted.";
      exit 130
  | Eio.Exn.Multiple exns when List.exists (fun (e, _) -> is_interrupt e) exns
    ->
      eprintf "Interrupted.";
      exit 130
  | (Oi.Error.E _ | Failure _ | Eio.Io (D10.Sysops.Cmd_failed _, _)) as exn ->
      exit_for_exn exn
  | Eio.Exn.Multiple exns -> (
      match !error_format with
      | Json ->
          (* Pick the first non-interrupt as the representative error
             for the JSON envelope; the others would each need their
             own envelope, which an agent doesn't expect. *)
          let exn =
            try fst (List.find (fun (e, _) -> not (is_interrupt e)) exns)
            with Not_found -> Failure "multiple errors"
          in
          exit_for_exn exn
      | Text ->
          List.iter (fun (e, _bt) -> eprintf "%a" pp_one_exn e) exns;
          exit 1)

(* Forced to the POSIX backend rather than [Eio_main.run] so builds under
   Linux don't pick up [eio_linux] / io_uring — we want the same syscall
   surface everywhere, and io_uring interacts poorly with some of the
   subprocess / signal paths we rely on.

   The outer switch [sw] also owns the process-wide HTTP session created
   in {!bootstrap}, so its connection pool spans the entire CLI run. *)
let with_eio_root f =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Oi.Signals.install ~sw;
  f ~sw env

let run f = with_error_handling @@ fun () -> with_eio_root f

let bootstrap ~sw ?data_dir ?(format = Terms.Text) (env : Eio_unix.Stdenv.base)
    cache_dir =
  (* Capture format first so any later step in this function (stamp
     ensure, opam init, …) raising an exception is reported in the
     requested form. *)
  error_format := format;
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let stdout = Eio.Stdenv.stdout env in
  let stderr = Eio.Stdenv.stderr env in
  let sys = D10.Sysops.v ~stdout ~stderr ~proc_mgr ~fs ~net ~clock () in
  let platform = Osrel.detect ~proc_mgr ~fs in
  let os_key = D10.Os_key.(to_string (of_platform platform)) in
  let cache = Oi.Cache.v ~root:cache_dir fs in
  let data_dir = resolve_data_dir ?override:data_dir () in
  (* Serialise every [oi] invocation against every other one sharing the
     same [data_dir]. The lockfile fd is owned by [sw] (the
     command-lifetime switch installed in {!with_eio_root}) so the lock
     releases on normal exit, exception unwind, or signal-driven cancel;
     a second [oi] blocks here and logs every 5 s until the first
     releases. The lockfile lives at [<data_dir>/.oi.lock] (mirroring
     [.reporepo.lock]), {b outside} any git-tree-rooted subdir so
     [git clean -xfd] never sweeps it. *)
  (try Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / data_dir)
   with Eio.Exn.Io _ -> ());
  Oi.Lock.acquire_global ~sw ~clock ~fs ~data_dir ();
  (* Validate on-disk schema stamps before any command body runs. A
     mismatch means the user upgraded [oi] across an incompatible
     format change; stale caches get cleared automatically so the
     command itself can assume current-shape state. *)
  Oi.Stamp.ensure ~fs ~cache ~data_dir;
  (* Create the session under the outer Harness switch so its connection
     pool lives for the whole CLI invocation — every command body that
     fetches from a registry shares it, no per-command setup cost. *)
  let http_session = D10.Sysops.Http.with_session ~sw sys (fun s -> s) in
  {
    proc_mgr;
    fs;
    clock;
    sys;
    platform;
    os_key;
    cache;
    data_dir;
    format;
    http_session;
  }
