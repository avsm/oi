(** The [docker] and [obuilder] subcommands of [oi dist]: generate Dockerfiles
    or obuilder specs (project build, [--test], target replay, or [--all]
    multi-distro). Both share the solve / depext / per-distro pipeline; the
    obuilder spec omits the docker-compose orchestration. *)

val subcommands : unit Cmdliner.Cmd.t list
(** [subcommands] are the [docker] and [obuilder] subcommands for the [oi dist]
    group. *)
