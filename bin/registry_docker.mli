(** Dockerfile generation for [oi registry docker].

    Emits a standalone static musl build of [oi] on alpine, plus one runnable
    per-distro image whose [CMD] runs [oi registry build] followed by
    [oi registry export /out]. A [docker-compose.yml] orchestrates them with a
    shared host bind mount at [/out] so a single [docker compose up] runs every
    distro in parallel and leaves the exported registry on the host. *)

module Distro = Dockerfile_opam.Distro

val dockerfile_oi : src_context:string -> Dockerfile.t
(** [dockerfile_oi ~src_context] emits a Dockerfile that builds a static
    musl-linked [oi] binary from the source tree at [src_context] (relative to
    the build context) and exports it as a scratch image at [/oi]. *)

val dockerfile_one_distro :
  src_context:string -> packages_ctx_path:string -> Distro.t -> Dockerfile.t
(** Per-distro build image. Stage 0 is the [oi-builder]; the final stage
    installs depexts, copies the static [oi] and [packages.txt] into place, and
    sets a [CMD] that runs [oi registry build] followed by
    [oi registry export /out]. The image is launched via [docker-compose.yml]
    with a bind mount at [/out]. *)

val one_distro_filename : Distro.t -> string
(** Filename (without directory) for a per-distro Dockerfile, e.g.
    [Dockerfile.alpine-3.23]. *)

val service_name : Distro.t -> string
(** docker-compose service name for a distro, e.g. [alpine-3.23]. *)

val docker_compose_yaml :
  distros:Distro.t list -> registry_host_path:string -> string
(** [docker_compose_yaml ~distros ~registry_host_path] emits a compose file
    whose services each build the corresponding per-distro Dockerfile and
    bind-mount [registry_host_path] onto [/out]. *)

val parse_packages_file : string -> string list
(** Read a packages file at [path]: strip [#] comments and blank lines, return
    one opam target spec per entry. *)

val write_dockerfile : string -> Dockerfile.t -> unit
(** Serialise a {!Dockerfile.t} to [path] in Dockerfile syntax. *)

val write_packages_file : string -> string list -> unit
(** Write a cleaned packages list (one target per line) to [path]. *)

val write_file : string -> string -> unit
(** Write arbitrary string contents to [path]. *)
