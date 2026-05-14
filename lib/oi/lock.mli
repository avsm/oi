(** Process-wide global lock for [oi] commands.

    Every [oi] subcommand acquires {!path}'s lockfile {!D10.Lock.Exclusive} on
    entry (in {!Cmd.Harness.bootstrap}) and holds it for the whole command. A
    second concurrent [oi] invocation blocks on the same lockfile until the
    first releases (process exit, ctrl-C, crash), then runs to completion in
    turn.

    This replaces the previous per-resource granular locks (per-layer-hash,
    per-archive-sha, per-prefix, per-pin-set, reporepo). With the single global
    lock, every cache-write path is implicitly serialised across processes;
    in-process concurrency (build-parallelism fibres) is governed by the
    existing per-module in-flight tables. *)

val path : data_dir:string -> string
(** [path ~data_dir] is the canonical global lockfile path
    [<data_dir>/.oi.lock]. Sited next to {b not} inside [<data_dir>/reporepo/]
    so a [git clean -xfd] in any working tree never sweeps it. *)

val acquire_global :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  data_dir:string ->
  unit ->
  unit
(** [acquire_global ~sw ~clock ~fs ~data_dir ()] takes {!D10.Lock.Exclusive} on
    {!path} for the lifetime of [sw]: the lockfile fd is owned by the switch via
    {!Eio_unix.Fd.of_unix} [~close_unix:true], so the lock releases on normal
    switch teardown, exception propagation, signal-driven cancel, or process
    death. The function returns [unit] — there is no handle to manage, and
    calling [release] early would be incorrect (the rest of the command body
    needs the lock).

    Blocks forever if another [oi] holds it, logging progress every 5 s via
    {!D10.Lock}'s default [on_wait]. *)
