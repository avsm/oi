(** Dockerfile generation for [oi registry docker].

    Emits a standalone static musl build of [oi] on alpine plus one runnable
    per-distro image that ships [oi] and the target's depexts. The actual [oi
    registry build] and [oi registry export] invocations live in
    [docker-compose.yml] as a [command:] override, so the same image can run
    against different reporepo pins without rebuilding. *)

module Distro = Dockerfile_opam.Distro

val dockerfile_oi : src_context:string -> Dockerfile.t
(** [dockerfile_oi ~src_context] emits a Dockerfile that builds a static
    musl-linked [oi] binary from the source tree at [src_context] (relative to
    the build context) and exports it as a scratch image at [/oi]. *)

val dockerfile_one_distro : src_context:string -> Distro.t -> Dockerfile.t
(** Per-distro build image. Stage 0 is the [oi-builder]; the final stage
    installs depexts and copies the static [oi] binary into place. No [CMD] is
    set — the compose file drives the build+export steps. *)

val one_distro_filename : Distro.t -> string
(** Filename (without directory) for a per-distro Dockerfile, e.g.
    [Dockerfile.alpine-3.23]. *)

val service_name : Distro.t -> string
(** docker-compose service name for a distro, e.g. [alpine-3.23]. *)

val docker_compose_yaml :
  distros:Distro.t list ->
  registry_host_path:string ->
  reporepo_host_path:string ->
  unit ->
  string
(** [docker_compose_yaml ~distros ~registry_host_path ~reporepo_host_path ()]
    emits a compose file whose services each bind-mount [registry_host_path]
    at [/out] and [reporepo_host_path] at [/opt/reporepo] (read-only), set
    [OI_REPOREPO=/opt/reporepo], and run
    [oi registry build --all && oi registry export /out]. Layers are tagged
    with their overlay handle and version in the per-distro sqlite index so
    clients can scope queries to a specific overlay.

    An additional one-shot [oi-binary] service copies the static [oi] binary
    to [/out/oi-linux-$(uname -m)] so consumers can fetch it alongside the
    registry layers. *)

val write_dockerfile : string -> Dockerfile.t -> unit
(** Serialise a {!Dockerfile.t} to [path] in Dockerfile syntax. *)

val write_file : string -> string -> unit
(** Write arbitrary string contents to [path]. *)
