(** The [oi dist] command group: [docker] / [obuilder] / [makefile] (all
    projecting the same solved plan) plus [duniverse] (vendor a target's sources
    into a self-contained bundle). *)

val cmd : unit Cmdliner.Cmd.t

