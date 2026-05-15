(** Per-command harness: Eio root, signal handling, error catching, and the
    Eio-capability bundle every command body operates on.

    Idiom every command uses:
    {[
      let run () data_dir cache_dir … =
        Harness.run @@ fun ~sw env ->
        let { Harness.proc_mgr; fs; clock; sys; http_session; … } =
          Harness.bootstrap ~sw env cache_dir
        in
        … command-specific work …
    ]} *)

type env = {
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  sys : D10.Sysops.t;
  platform : Osrel.t;
  os_key : string;
  cache : Oi.Cache.t;
  data_dir : string;
      (** Resolved data directory — the [--data-dir] override if any, otherwise
          [$OI_DATA_DIR], [$XDG_DATA_HOME/oi], or [$HOME/.local/share/oi]. *)
  format : Terms.format;
      (** Output format requested by [--format]. Commands with structured output
          dispatch on this; the harness itself uses it to switch {!Oi.Error}'s
          envelope between human text and JSON. *)
  http_session : D10.Sysops.Http.session;
      (** Process-wide HTTP session, scoped to the {!run} switch. Every registry
          fetch the command pipeline performs — index probe, layer pulls, source
          archive downloads — shares this connection pool, so we do one TLS
          handshake per host across the whole CLI invocation regardless of how
          many solve groups or sub-commands run. *)
}
(** What every command body needs after Eio + cache setup. Returned by
    {!bootstrap}. *)

type target = { env : string array; argv : string list }
(** A resolved target to hand control to: the [env] to run it under and the full
    [argv] (with [argv.(0)] the program). Plain data only — no Eio resources —
    so it remains valid after {!run} has returned and the Eio scheduler has shut
    down. *)

exception Exec of target
(** Raised by {!exec} as a non-local return out of a command body. A command
    body that wants to run a target catches this itself and returns
    [Some target] from its {!run} callback; see {!exec}. *)

val exec : env:string array -> string list -> 'a
(** [exec ~env argv] is called from deep in a command body once the cache is
    fully staged and the only thing left is to run [argv]. It does {b not} spawn
    anything — it raises {!Exec} so the body can return the {!target} out of
    {!run}, after which the cmd layer ([oi run] / [oi exec]) runs it via
    {!Subprocess.exec}.

    Spawning from the cmd layer happens {e after} {!run} returns and the Harness
    switch has torn down. Teardown closes the [<data_dir>/.oi.lock] fd
    ({!Oi.Lock.acquire_global}), so the target runs with the process-wide cache
    lock already released and concurrent [oi] invocations proceed — important
    for long-lived / interactive targets ([oi run utop], [oi exec dune build]).
    The global locking semantics are unchanged; only the spawn point moves.

    Never returns (raises). *)

val run : (sw:Eio.Switch.t -> Eio_unix.Stdenv.base -> 'a) -> 'a
(** [run f] sets up the Eio root, installs the SIGINT/SIGTERM handler under a
    fresh switch, and calls [f ~sw env]. The switch owns long-lived resources
    like the {!env.http_session} created in {!bootstrap}. Any exception that
    escapes [f] is caught: signal-style exits print "Interrupted." and exit 130;
    structured {!Oi.Error.E} or [Failure] exceptions print a single coloured
    line and exit 1; everything else prints a generic error and exits 1. *)

val bootstrap :
  sw:Eio.Switch.t ->
  ?data_dir:string ->
  ?format:Terms.format ->
  Eio_unix.Stdenv.base ->
  string ->
  env
(** [bootstrap ~sw ?data_dir ?format env cache_dir] reads the proc-mgr / fs /
    clock / stdio capabilities off [env], creates the [Sysops], detects the
    platform, builds the cache rooted at [cache_dir], and opens the process-wide
    HTTP session under [sw]. Validates {!Oi.Stamp} stamps on the cache, data,
    and toolchain dirs and clears any whose schema is below the current build's
    expectation.

    [data_dir] defaults to [$OI_DATA_DIR] / [$XDG_DATA_HOME/oi] /
    [$HOME/.local/share/oi]. [format] (default [Text]) selects how {!run}'s
    exception handler renders failures: [Text] prints a coloured line on stderr,
    [Json] emits an {!Oi.Error.codec} envelope on stdout. *)
