(** [oi clean]: remove cached layers, scratch trees and build outputs for the
    current project (or globally with the appropriate flag). *)

val cmd : unit Cmdliner.Cmd.t
