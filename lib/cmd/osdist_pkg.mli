(** [oi dist pkg]: consume a bundle from [oi dist bundle] and materialise the
    per-distro packaging tree (Dockerfiles, [debian/], [.spec]). Optionally
    drives [docker buildx build] for each target. *)

val cmd : unit Cmdliner.Cmd.t
