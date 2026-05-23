[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

let log_src = Logs.Src.create "sysops"

module Log = (val Logs.src_log log_src : Logs.LOG)

type tools = { tar : string }
type pm = [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t
type net = [ `Generic ] Eio.Net.ty Eio.Resource.t
type clk = float Eio.Time.clock_ty Eio.Resource.t

(* Structured error for non-zero subprocess exits.

   Extends [Eio.Exn.err] (rather than defining a fresh [exception])
   because every subprocess this module spawns goes through [Eio.Process]
   anyway, so the failure mode is morally an Eio I/O error — and the
   Eio convention gives us [Eio.Exn.add_context] for layering "while
   running cp -Rfl from X to Y" annotations, and [Eio.Exn.register_pp]
   wires into the same [Printexc] printer that already handles
   [Eio.Io _]. Callers pattern-match on
   [Eio.Io (Cmd_failed _, _)].

   The previous [Fmt.failwith "command exited N: ..."] tunnel through
   bare [Failure] was indistinguishable from any other [Failure] in
   the system, and silently disarmed catch sites that meant to retry
   only on subprocess failure (e.g. the [cp -Rfl] hardlink-pass
   fallback in [link_tree], which narrowed to [Eio.Exn.Io] and so
   never fired). *)
type Eio.Exn.err +=
  | Cmd_failed of {
      argv : string list;
      status : [ `Exited of int | `Signaled of int ];
      output : string;
    }

let () =
  Eio.Exn.register_pp (fun ppf -> function
    | Cmd_failed { argv; status; output } ->
        let how =
          match status with
          | `Exited n -> Fmt.str "command exited %d" n
          | `Signaled n -> Fmt.str "command killed by signal %d" n
        in
        Fmt.pf ppf "%s: %s" how (String.concat " " argv);
        if output <> "" then
          Fmt.pf ppf "@,--- output ---@,%s" (String.trim output);
        true
    | _ -> false)

let cmd_failed ~argv ~status ~output =
  Eio.Exn.create (Cmd_failed { argv; status; output })

type t = {
  proc_mgr : pm;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  net : net;
  clock : clk;
  stdout : Eio.Flow.sink_ty Eio.Resource.t option;
  stderr : Eio.Flow.sink_ty Eio.Resource.t option;
  tools : tools;
}

(* -- Low-level helpers --------------------------------------------------- *)

let native p = Eio.Path.native_exn p

(* -- Subprocess timeout --------------------------------------------------

   Default 10-minute hard cap on every syscmd subprocess this module
   spawns: git fetch/clone, tar extract, cp, which, etc. Long enough
   for a slow git clone of a multi-GB repository on flaky connectivity,
   short enough to break a wedged process before it tanks an
   [oi build --all]. Override per-process via [OI_CMD_TIMEOUT] (seconds);
   set to [0] to disable.

   Why this exists: bare [Eio.Process.await] has no timeout. A [git
   clone] from a stalled remote will block the calling fiber forever,
   leaving the rest of the build queued and the spawned [git] process
   uncollectable. With this cap a wedged subprocess fails the calling
   fiber with [Failure], the switch unwinds, the child is killed, and
   the rest of the [oi build --all] continues.

   Build-step subprocesses (compile commands invoked by [D10ir.Direct])
   do NOT go through this path — those legitimately take longer for
   large native compiles and have their own bookkeeping. *)
let default_cmd_timeout_s = 600.0

let cmd_timeout_s () =
  match Sys.getenv_opt "OI_CMD_TIMEOUT" with
  | Some v -> (
      match float_of_string_opt v with
      | Some n when n >= 0.0 -> n
      | _ -> default_cmd_timeout_s)
  | None -> default_cmd_timeout_s

let with_timeout t ~cmd f =
  let timeout = cmd_timeout_s () in
  if timeout <= 0.0 then f ()
  else
    try Eio.Time.with_timeout_exn t.clock timeout f
    with Eio.Time.Timeout ->
      let argv = String.concat " " cmd in
      let mins = timeout /. 60.0 in
      Log.warn (fun m ->
          m "subprocess timed out after %.0fs (%.1f min): %s" timeout mins argv);
      Fmt.failwith
        "subprocess timed out after %.0fs (%.1f min) and was cancelled: %s\n\
         A child process exceeded the per-command time limit. The most common \
         cause is a wedged network operation (git fetch/clone from a slow or \
         unreachable remote, an opam source download, a hung HTTPS handshake). \
         Override the cap by exporting OI_CMD_TIMEOUT=N (seconds; [0] \
         disables)."
        timeout mins argv

let handle_run_quiet_result ~cmd ~buf result =
  let output = String.trim (Buffer.contents buf) in
  match result with
  | `Exited 0 -> if output <> "" then Log.debug (fun m -> m "%s" output)
  | `Exited n ->
      if output <> "" then Log.debug (fun m -> m "%s" output);
      raise (cmd_failed ~argv:cmd ~status:(`Exited n) ~output)
  | `Signaled n -> raise (cmd_failed ~argv:cmd ~status:(`Signaled n) ~output)

let run_quiet t cmd =
  Log.debug (fun m -> m "$ %s" (String.concat " " cmd));
  with_timeout t ~cmd @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let buf = Buffer.create 256 in
  let sink = Eio.Flow.buffer_sink buf in
  let child = Eio.Process.spawn ~sw t.proc_mgr ~stdout:sink ~stderr:sink cmd in
  handle_run_quiet_result ~cmd ~buf (Eio.Process.await child)

let run_capture t cmd =
  Log.debug (fun m -> m "$ %s" (String.concat " " cmd));
  let out =
    with_timeout t ~cmd @@ fun () ->
    String.trim (Eio.Process.parse_out t.proc_mgr Eio.Buf_read.take_all cmd)
  in
  Log.debug (fun m -> m "%s" out);
  out

(* Like [run_capture] but with stderr redirected into a throwaway
   buffer so chatty subprocesses (git's "warning: redirecting to ..."
   notice) don't leak to the parent's stderr. *)
let run_capture_quiet t cmd =
  Log.debug (fun m -> m "$ %s" (String.concat " " cmd));
  let stderr_buf = Buffer.create 64 in
  let stderr_sink = Eio.Flow.buffer_sink stderr_buf in
  let out =
    with_timeout t ~cmd @@ fun () ->
    String.trim
      (Eio.Process.parse_out ~stderr:stderr_sink t.proc_mgr
         Eio.Buf_read.take_all cmd)
  in
  Log.debug (fun m -> m "%s" out);
  out

(* Spawn [which NAME] with both stdout and stderr routed into a
   throwaway buffer. Nix's [which] writes "no NAME in PATH" to stderr
   on miss; [Eio.Process.parse_out] only captures stdout, so without
   the buffered stderr sink that line leaks to the CLI's own stderr. *)
let has_cmd t name =
  let cmd = [ "which"; name ] in
  with_timeout t ~cmd @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let buf = Buffer.create 64 in
  let sink = Eio.Flow.buffer_sink buf in
  let child = Eio.Process.spawn ~sw t.proc_mgr ~stdout:sink ~stderr:sink cmd in
  match Eio.Process.await child with
  | `Exited 0 -> true
  | `Exited _ | `Signaled _ -> false

(* -- Initialisation ------------------------------------------------------ *)

let resolve_tools t =
  let tar = if has_cmd t "gtar" then "gtar" else "tar" in
  { tar }

(* Kill every pathway that would make git block on stdin for
   credentials. Hitting an HTTP 401 or a private git repo from inside
   [oi] should fail fast (and let [Retry] back off) rather than hang
   the whole build waiting for a [Username for 'https://github.com':]
   prompt that'll never come in a non-interactive session. Set via
   [Unix.putenv] so every child process [Eio.Process.spawn] launches
   — opam's git backend, direct [git] invocations, and ssh under
   [git@] URLs — inherits the same muzzle. *)
let disable_interactive_git () =
  Unix.putenv "GIT_TERMINAL_PROMPT" "0";
  (* [GIT_ASKPASS] overrides [core.askPass]; [/bin/true] returns empty
     credentials so git fails fast instead of blocking. *)
  Unix.putenv "GIT_ASKPASS" "/bin/true";
  (* Same story for ssh: don't spawn a GUI askpass helper. *)
  Unix.putenv "SSH_ASKPASS" "/bin/true";
  Unix.putenv "SSH_ASKPASS_REQUIRE" "never"

(* The TLS stack inside [Requests.One] needs a process-wide RNG default,
   otherwise the first HTTPS request errors out with "Crypto_rng: no
   default generator". Idempotent: subsequent calls are no-ops. *)
let install_crypto_rng () = Crypto_rng_unix.use_default ()

let v ?stdout ?stderr ~proc_mgr ~fs ~net ~clock () =
  disable_interactive_git ();
  install_crypto_rng ();
  let stdout =
    Option.map (fun s -> (s :> Eio.Flow.sink_ty Eio.Resource.t)) stdout
  in
  let stderr =
    Option.map (fun s -> (s :> Eio.Flow.sink_ty Eio.Resource.t)) stderr
  in
  let t_partial =
    {
      proc_mgr :> pm;
      fs;
      net :> net;
      clock :> clk;
      stdout;
      stderr;
      tools = { tar = "tar" };
    }
  in
  let tools = resolve_tools t_partial in
  { t_partial with tools }

let pp ppf _t = Fmt.string ppf "<sysops>"

(* -- File queries -------------------------------------------------------- *)

let file_exists path =
  try
    ignore (Eio.Path.stat ~follow:true path);
    true
  with Eio.Exn.Io _ -> false

(* -- File copying -------------------------------------------------------- *)

let copy_tree t ~src ~dst =
  let src_s = native src and dst_s = native dst in
  try run_quiet t [ "cp"; "-ac"; src_s; dst_s ]
  with Eio.Io (Cmd_failed _, _) ->
    Eio.Path.rmtree ~missing_ok:true dst;
    run_quiet t [ "cp"; "-a"; src_s; dst_s ]

let link_tree t ~src ~dst =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dst;
  let src_s = native src and dst_s = native dst in
  (* No [-v]: [run_quiet] throws the output away and one layer-restore
     can produce tens of thousands of lines that then have to be
     drained through the Eio buffer_sink, which is slow at best and
     has hung in the wild when stdout and stderr are merged.

     Try the hardlink pass first ([cp -Rfl]); if [src] and [dst] live on
     different filesystems (typical in containers: BuildKit cache mounts
     are usually overlay/tmpfs, the image's writable layer is overlay too
     but a different one), [cp] fails with [EXDEV] and a non-zero exit.
     Fall back to a real archive-mode copy ([cp -Rfa]) so the build
     prefix still gets a complete tree. Slower for a one-time fill but
     unblocks every cross-mount setup we've seen. *)
  (* [Cmd_failed] is what [run_quiet] raises on a non-zero exit (now
     wrapped in [Eio.Io]); the earlier wildcard [Eio.Exn.Io] catch in
     fact ran against [Failure], not an Eio error, so it never fired
     and the build died on the first cross-filesystem layer restore
     (BuildKit cache mount -> image overlay -> [EXDEV] from
     [cp -Rfl]). *)
  try run_quiet t [ "cp"; "-Rfl"; src_s ^ "/."; dst_s ^ "/" ]
  with Eio.Io (Cmd_failed _, _) ->
    Log.debug (fun m ->
        m "link_tree %s -> %s: hardlink pass failed; falling back to copy" src_s
          dst_s);
    run_quiet t [ "cp"; "-Rfa"; src_s ^ "/."; dst_s ^ "/" ]

(* -- Low-level command execution ----------------------------------------- *)

(* Run [cmd] with stdout/stderr inherited from the parent terminal so
   any progress or error output the subprocess writes is shown to the
   user as it happens. Used for interactive-feeling commands like git
   pull/push where hiding the subprocess output would leave the user
   guessing. Falls back to [run_quiet] when no stdout/stderr resources
   were registered on [t]. *)
let handle_run_inherit_result ~cmd = function
  | `Exited 0 -> ()
  | `Exited n -> raise (cmd_failed ~argv:cmd ~status:(`Exited n) ~output:"")
  | `Signaled n -> raise (cmd_failed ~argv:cmd ~status:(`Signaled n) ~output:"")

let run_inherit_with t ~stdout ~stderr cmd =
  with_timeout t ~cmd @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let child = Eio.Process.spawn ~sw t.proc_mgr ~stdout ~stderr cmd in
  handle_run_inherit_result ~cmd (Eio.Process.await child)

let run_inherit t cmd =
  Log.debug (fun m -> m "$ %s" (String.concat " " cmd));
  match (t.stdout, t.stderr) with
  | None, _ | _, None -> run_quiet t cmd
  | Some stdout, Some stderr -> run_inherit_with t ~stdout ~stderr cmd

module Cmd = struct
  let run t cmd = run_quiet t cmd
  let run_out t cmd = run_capture t cmd
  let run_out_quiet t cmd = run_capture_quiet t cmd
  let run_inherit t cmd = run_inherit t cmd
end

(* -- Archive operations -------------------------------------------------- *)

module Tar = struct
  let extract t ~archive ~dst ?(strip = 0) () =
    let cmd =
      [ t.tools.tar; "xf"; native archive; "-C"; native dst ]
      @ if strip > 0 then [ Fmt.str "--strip-components=%d" strip ] else []
    in
    run_quiet t cmd

  let create_zstd t ~src ~dst =
    run_quiet t
      [ t.tools.tar; "--zstd"; "-cf"; native dst; "-C"; native src; "." ]
end

module Http = struct
  (* -- Per-request HTTP timeout --------------------------------------------

     Default 10-minute hard cap on every in-process [Requests] fetch this
     module performs. Same rationale as {!cmd_timeout_s} for subprocess
     spawns, but for the HTTP path: a wedged TCP socket (e.g. a remote
     that sent FIN but where our [Eio.Flow.single_read] never observes
     EOF, leaving us in [CLOSE_WAIT] until the kernel's keepalive expires
     ~2 hours later) blocks the whole [oi build --all] pipeline.

     Override per-fetch via [OI_HTTP_TIMEOUT] (seconds); set to [0] to
     disable. Distinct from [OI_CMD_TIMEOUT] so callers can tune the two
     paths independently. *)
  let default_http_timeout_s = 600.0

  let http_timeout_s () =
    match Sys.getenv_opt "OI_HTTP_TIMEOUT" with
    | Some v -> (
        match float_of_string_opt v with
        | Some n when n >= 0.0 -> n
        | _ -> default_http_timeout_s)
    | None -> default_http_timeout_s

  (* [Http.fetch] / [Http.fetch_session] return [bool] (true on success,
     false on any error) — never raise. On timeout we log at warn and
     return false so callers fall back to whatever cache hierarchy or
     retry policy they have, the same way they would on a 5xx or a
     network error. *)
  let with_http_timeout ~clock ~url f =
    let timeout = http_timeout_s () in
    if timeout <= 0.0 then f ()
    else
      try Eio.Time.with_timeout_exn clock timeout f
      with Eio.Time.Timeout ->
        Log.warn (fun m ->
            m
              "HTTP request timed out after %.0fs (%.1f min); cancelling: %s\n\
               Override the cap by exporting OI_HTTP_TIMEOUT=N (seconds; [0] \
               disables)."
              timeout (timeout /. 60.0) url);
        false

  (* Stream [src] into [sink] while invoking [on_progress] with the
     running byte count. Throttles callbacks to ~20Hz so a fast
     download doesn't spam the renderer (each [Tty.Progress.set] is
     a synchronous ANSI write that's expensive on slow terminals).
     The final tick fires unconditionally so the bar always settles
     at 100% / total. *)
  let copy_with_progress ~clock ?on_progress ?total src sink =
    match on_progress with
    | None -> Eio.Flow.copy src sink
    | Some on_progress ->
        let buf = Cstruct.create 65536 in
        let received = ref 0L in
        let last_tick = ref 0.0 in
        let throttle_s = 0.05 in
        let maybe_tick () =
          let now = Eio.Time.now clock in
          if now -. !last_tick >= throttle_s then begin
            last_tick := now;
            on_progress ~received:!received ~total
          end
        in
        (try
           while true do
             let n = Eio.Flow.single_read src buf in
             Eio.Flow.write sink [ Cstruct.sub buf 0 n ];
             received := Int64.add !received (Int64.of_int n);
             maybe_tick ()
           done
         with End_of_file -> ());
        on_progress ~received:!received ~total

  (* In-process HTTP fetch via the [requests] library. Behaviour:

     - silent: no progress output to stdout/stderr
     - follows redirects (the [Requests.One.get] default)
     - fails on HTTP error status: we check {!Requests.Response.ok}
       ourselves and treat 4xx/5xx as failure, since [One.get] returns
       a [Response.t] regardless of status.

     Returns [true] on a 2xx response with the body fully written to
     [dst]; [false] on any error (4xx/5xx, network failure, TLS error,
     timeout). Never raises — callers use the [if not (fetch …) then …]
     pattern.

     [on_progress], when supplied, is called periodically with
     [(received, total)] where [total = Some n] when the response carries
     a [Content-Length] header and [None] otherwise (chunked transfer).
     Throttled to ~20Hz inside [copy_with_progress] so terminal redraws
     don't dominate the wall-clock cost on a fast LAN. *)
  let save_body ?on_progress t ~dst resp =
    let total = Requests.Response.content_length resp in
    let body = Requests.Response.body resp in
    Eio.Path.with_open_out ~create:(`Or_truncate 0o644) dst (fun out ->
        copy_with_progress ~clock:t.clock ?on_progress ?total body out);
    true

  let fetch_response ?on_progress t ~url ~dst resp =
    if not (Requests.Response.ok resp) then begin
      Log.debug (fun m ->
          m "http %s: status %d" url (Requests.Response.status_code resp));
      false
    end
    else save_body ?on_progress t ~dst resp

  let fetch_in_switch ?on_progress t ~url ~dst sw =
    try
      let resp = Requests.One.get ~sw ~clock:t.clock ~net:t.net url in
      fetch_response ?on_progress t ~url ~dst resp
    with exn ->
      Log.debug (fun m -> m "http %s: %s" url (Printexc.to_string exn));
      false

  let fetch ?on_progress t ~url ~dst =
    with_http_timeout ~clock:t.clock ~url @@ fun () ->
    Eio.Switch.run (fetch_in_switch ?on_progress t ~url ~dst)

  (* HEAD probe. Same one-shot connection model as [fetch] (no session),
     same timeout cap, same "never raises, return bool" contract. Used by
     [oi repo bump --upload-archives] to skip re-uploads of objects
     already at the public read URL. *)
  let head_in_switch t ~url sw =
    try
      let resp = Requests.One.head ~sw ~clock:t.clock ~net:t.net url in
      let ok = Requests.Response.ok resp in
      if not ok then
        Log.debug (fun m ->
            m "http HEAD %s: status %d" url (Requests.Response.status_code resp));
      ok
    with exn ->
      Log.debug (fun m -> m "http HEAD %s: %s" url (Printexc.to_string exn));
      false

  let head t ~url =
    with_http_timeout ~clock:t.clock ~url @@ fun () ->
    Eio.Switch.run (head_in_switch t ~url)

  (* -- Session-based pooled HTTP -------------------------------------------

     [Requests.One.get]/[head] open a fresh TCP+TLS connection per call, so a
     batch of N parallel layer fetches against [oi.ci.dev] does N TLS
     handshakes (and N HTTP/2 setups when ALPN picks h2) — wasteful even
     before we count the concurrency loss from not multiplexing on a single
     h2 connection. A [Requests.t] session shares a connection pool across
     requests and lets concurrent fibers multiplex over the same h2 stream,
     so N parallel fetches collapse to one handshake plus N concurrent
     STREAM frames. *)
  (* Wraps the [Requests.t] connection pool with a [clock] so
     {!fetch_session} can cap each request via {!with_http_timeout}.
     Held by the parent switch via {!with_session}. *)
  type session = { req : Requests.t; clock : clk }

  let env_of (t : t) : < clock : _ Eio.Time.clock ; net : _ Eio.Net.t ; fs : _ >
      =
    object
      method clock = t.clock
      method net = t.net
      method fs = t.fs
    end

  (* [max_connections_per_host] is sized to match [OI_HTTP_PARALLELISM]
     (default 8 in [Oi.Pipeline.fetch_parallelism]) — one connection
     per concurrent fiber, no headroom. Earlier we ran a 2x cushion
     (16 connections vs 8 fibers) which sounded harmless but made the
     pool harder to drain when a remote got flaky: half-closed sockets
     piled up and new fibers picked already-broken connections out of
     the pool. Sizing the cap to the fiber count keeps the pool small
     and the failure mode obvious — if every connection is stuck, no
     fiber starts.

     The idle/lifetime numbers are deliberately generous so a CLI run
     that issues fetches in two waves (layers, then sources) reuses the
     same connections across the gap.

     [OI_FORCE_HTTP1=1] (test-only) routes through [Requests.v]'s
     [~protocol_mode:Http1_only] so the session advertises only
     [http/1.1] in ALPN, forcing the registry to fall back to HTTP/1.1
     even when it supports HTTP/2. Useful for A/B-comparing the H1 vs
     H2 paths. *)
  let default_pool_size = 8

  let force_http1 () =
    match Sys.getenv_opt "OI_FORCE_HTTP1" with
    | Some v when v <> "" && v <> "0" -> true
    | _ -> false

  let with_session ~sw t f =
    let protocol_mode =
      if force_http1 () then Some Requests.Http1_only else None
    in
    let req =
      Requests.v ~sw ~max_connections_per_host:default_pool_size
        ~connection_idle_timeout:300.0 ~connection_lifetime:1800.0
        ?protocol_mode (env_of t)
    in
    f { req; clock = t.clock }

  let session_save_body ~dst resp =
    let body = Requests.Response.body resp in
    Eio.Path.with_open_out ~create:(`Or_truncate 0o644) dst (fun out ->
        Eio.Flow.copy body out);
    true

  let session_handle_response ~url ~dst resp =
    if not (Requests.Response.ok resp) then begin
      Log.debug (fun m ->
          m "http %s: status %d" url (Requests.Response.status_code resp));
      false
    end
    else session_save_body ~dst resp

  let fetch_session ?on_progress session ~url ~dst =
    with_http_timeout ~clock:session.clock ~url @@ fun () ->
    try
      (* [Requests.get ?on_progress] fires the callback as response
         body bytes arrive on the wire (per HTTP/2 DATA frame on the
         h2 path). That's what makes the per-layer progress bar tick
         in real time — without it the body is fully buffered before
         [Response.body] returns and the subsequent [Flow.copy] sees
         memory-copy speed, not network speed. *)
      let resp = Requests.get ?on_progress session.req url in
      session_handle_response ~url ~dst resp
    with exn ->
      Log.debug (fun m -> m "http %s: %s" url (Printexc.to_string exn));
      false
end
