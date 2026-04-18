[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Signal handling for oi commands.

    Installs a single SIGINT / SIGTERM handler that turns a clean
    Ctrl-C into Eio cancellation of the supplied root switch. A second
    Ctrl-C within the same process force-exits with code 130 — the
    standard Unix convention for SIGINT-terminated processes.

    Must be installed by every top-level command; idempotent if called
    more than once in a single process (the handler is global). *)

exception Interrupted
(** Raised via [Eio.Switch.fail] when the first signal arrives. Matches
    the exception in [Eio.Cancel.Cancelled], so callers see
    [Eio.Cancel.Cancelled Interrupted] when a cancel propagates up. *)

val install : sw:Eio.Switch.t -> unit
(** [install ~sw] takes over SIGINT and SIGTERM for this process. On
    the first signal, it broadcasts a cancel condition, prints a
    one-line advisory to stderr, and calls [Eio.Switch.fail sw
    Interrupted]. On the second signal it [Stdlib.exit 130].

    Also sets [Sys.catch_break false] so opam's default SIGINT →
    [Sys.Break] translation doesn't race us, and ignores SIGPIPE so
    writes to closed pipes don't crash on older kernels.

    Safe to call multiple times — subsequent calls are no-ops. *)
