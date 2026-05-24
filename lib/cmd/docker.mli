(** The [docker] and [obuilder] subcommands of [oi dist]: generate Dockerfiles
    or obuilder specs (project build, [--test], target replay, or [--all]
    multi-distro). Both share the solve / depext / per-distro pipeline; the
    obuilder spec omits the docker-compose orchestration. *)

val subcommands : unit Cmdliner.Cmd.t list
(** The [docker] and [obuilder] subcommands for the [oi dist] group. *)

val default_distros : Registry_docker.Distro.t list
(** Fixed per-distro matrix driving [--all] and the [oi dist makefile]
    Dockerfile set: Alpine, Debian-stable, Ubuntu 24.04, Ubuntu 26.04, Fedora.
*)

val compute_per_distro_depexts :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  cache:Oi.Cache.t ->
  data_dir:string ->
  refresh:bool ->
  platform:Osrel.t ->
  (Registry_docker.Distro.t * string list) list
(** [compute_per_distro_depexts ~fs ~sys ~cache ~data_dir ~refresh ~platform]
    probes {!default_distros} for the union of overlay depexts (via
    {!Build.compute_overlay_depexts_per_distro}), degrading to empty depext
    lists with a warning if the probe fails (e.g. offline). Shared by
    [oi dist docker --all] and [oi dist makefile]. *)
