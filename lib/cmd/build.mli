(** [oi build] and [oi test]: build (or run tests for) a project, package,
    overlay, or the whole reporepo. *)

val compute_overlay_depexts_per_distro :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  cache:Oi.Cache.t ->
  data_dir:string ->
  refresh:bool ->
  platform:Osrel.t ->
  distros:Registry_docker.Distro.t list ->
  (Registry_docker.Distro.t * string list) list
(** [compute_overlay_depexts_per_distro ~fs ~sys ~cache ~data_dir ~refresh
     ~platform ~distros] runs the same expansion as
    {!compute_overlay_depexts_for_conf} but evaluated on each [distros] entry's
    filter context (os, os-distribution, os-family, os-version). Solves and
    overlay-tree walks happen once under the host conf and are reused across
    distros — only the per-distro depext filter is re-evaluated. Shared with
    [oi docker --all] which uses the result to parametrise the generated
    Dockerfiles. *)

val cmd : unit Cmdliner.Cmd.t
(** $(b,oi build). *)

val test_cmd : unit Cmdliner.Cmd.t
(** $(b,oi test). Defined here so the test path shares [find_target_layer],
    [run_target_test], and the rest of the build machinery without a
    one-function module. *)
