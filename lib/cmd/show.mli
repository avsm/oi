(** [oi show]: render a solver-resolved view of the current project — root set,
    the dependency tree below each root, layer hashes, install paths, and other
    diagnostic detail. *)

val cmd : unit Cmdliner.Cmd.t
