(** [oi run] and the standalone [oix] tool runner.

    Resolves [TARGET] (a package name, a [name.version] / opam-atom, an
    [@handle/pkg] shortcut, or a path to a [.ml] script), solves+builds its
    dependency closure, assembles a prefix, and execs the binary it installs.
    Also handles binary-name fallback via the cache's layer index, so
    [oi run utop] works without spelling out the producing package. *)

val cmd : unit Cmdliner.Cmd.t
(** The [oi run] subcommand. Honours [--skip-local] from the user. *)

val cmd_x : unit Cmdliner.Cmd.t
(** Top-level command for the [oix] binary: same logic as [oi run] but with
    [--skip-local] forced on (and the flag itself omitted from the help). *)
