(** [oi exec]: run an arbitrary command inside the current project's activated
    environment (the same one [oi env] prints), provisioning the toolchain first
    if needed. *)

val cmd : unit Cmdliner.Cmd.t
