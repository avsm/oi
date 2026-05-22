(** [oi dist repo]: assemble a signed apt/dnf/bin repo from the artefacts
    produced by [oi dist pkg --build]. Idempotent — re-runs merge new packages
    into the existing tree, preserving prior versions. *)

val cmd : unit Cmdliner.Cmd.t
