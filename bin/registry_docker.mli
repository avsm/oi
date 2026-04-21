(** Dockerfile generation for [oi registry docker].

    Emits a standalone static musl build of [oi] on alpine plus one runnable
    per-distro image that ships [oi], the target's depexts, and a
    [packages.txt]. The actual [oi registry build] and [oi registry export]
    invocations live in [docker-compose.yml] as a [command:] override, so the
    same image can run against different overlay sets without rebuilding. *)

module Distro = Dockerfile_opam.Distro

val dockerfile_oi : src_context:string -> Dockerfile.t
(** [dockerfile_oi ~src_context] emits a Dockerfile that builds a static
    musl-linked [oi] binary from the source tree at [src_context] (relative to
    the build context) and exports it as a scratch image at [/oi]. *)

val dockerfile_one_distro :
  src_context:string -> packages_ctx_path:string -> Distro.t -> Dockerfile.t
(** Per-distro build image. Stage 0 is the [oi-builder]; the final stage
    installs depexts and copies the static [oi] and [packages.txt] into place.
    No [CMD] is set — the compose file drives the build+export steps. *)

val one_distro_filename : Distro.t -> string
(** Filename (without directory) for a per-distro Dockerfile, e.g.
    [Dockerfile.alpine-3.23]. *)

val service_name : Distro.t -> string
(** docker-compose service name for a distro, e.g. [alpine-3.23]. *)

val docker_compose_yaml :
  ?overlays:string list ->
  distros:Distro.t list ->
  registry_host_path:string ->
  unit ->
  string
(** [docker_compose_yaml ?overlays ~distros ~registry_host_path ()] emits a
    compose file whose services each run [oi registry build] over the packages
    list followed by [oi registry export]. One extra build pass is scheduled per
    [overlay] handle — for each [H], the service also runs
    [oi registry build @H], which builds every package that overlay [H]
    contributes. Layers are tagged with the overlay's handle and version in the
    per-distro sqlite index so clients can scope queries to a specific overlay.
*)

val parse_packages_file : string -> string list
(** Read a packages file at [path]: strip [#] comments and blank lines, return
    one opam target spec per entry. *)

val write_dockerfile : string -> Dockerfile.t -> unit
(** Serialise a {!Dockerfile.t} to [path] in Dockerfile syntax. *)

val write_packages_file : string -> string list -> unit
(** Write a cleaned packages list (one target per line) to [path]. *)

val write_file : string -> string -> unit
(** Write arbitrary string contents to [path]. *)
