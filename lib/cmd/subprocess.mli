(** Subprocess execution helpers.

    [Eio.Process.spawn] uses execvp-style lookup, which resolves bare executable
    names against the {e caller's} PATH — not the PATH inside the [~env]
    argument. {!run} resolves the first token against [env]'s PATH first so the
    child finds binaries from the assembled prefix. *)

val run : _ Eio.Process.mgr -> env:string array -> string list -> int
(** [run proc_mgr ~env cmd] spawns [cmd] under [env], blocks until it exits, and
    returns its exit code (128+signal for signal-terminated children). Never
    raises on non-zero exit. *)

val exec : env:string array -> string list -> 'a
(** [exec ~env cmd] replaces the current process image with [cmd] via
    [Unix.execve], resolving [cmd]'s program against [env]'s [PATH] the same way
    {!run} does. No Eio: this is the cmd-layer hand-off used {e after} the
    Harness switch has torn down (and the global cache lock released), so the
    target — including a long-lived / interactive one — runs lock-free with its
    exit status naturally becoming [oi]'s. Never returns; exits 126 if the
    [execve] itself fails. *)
