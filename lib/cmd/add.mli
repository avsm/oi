(** [oi add]: add a package (and any extra dependencies) to the current
    project's [root.opam], invoking the solver to confirm the result is
    coherent. *)

val cmd : unit Cmdliner.Cmd.t
