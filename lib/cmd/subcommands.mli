(** Canonical list of [oi]'s top-level subcommands.

    Owned by the [Oi_cmd] library (not [bin/main.ml]) so other binaries —
    notably [oix] — can introspect subcommand names without duplicating the
    list. *)

val all : unit Cmdliner.Cmd.t list
(** Every subcommand in the order [oi --help] prints them. *)

val names : unit -> string list
(** [names ()] is [List.map Cmdliner.Cmd.name all]. Used by the [oix] wrapper to
    detect when the user mistyped [oix <oi-subcommand> …]. *)
