(** Dockerfile generation for [oi registry docker].

    Emits a standalone static musl build of [oi] on alpine (for CI / manual
    release builds) plus one runnable per-distro image that curls the latest
    [oi] binary from the GitHub releases page and ships the target's depexts.
    The actual [oi registry build] and [oi registry export] invocations live
    in [docker-compose.yml] as a [command:] override, so the same image can
    run against different reporepo pins without rebuilding. *)

module Distro = Dockerfile_opam.Distro

val dockerfile_oi : src_context:string -> Dockerfile.t
(** [dockerfile_oi ~src_context] emits a Dockerfile that builds a static
    musl-linked [oi] binary from the source tree at [src_context] (relative to
    the build context) and exports it as a scratch image at [/oi]. *)

val dockerfile_one_distro :
  ?overlay_depexts:string list -> Distro.t -> Dockerfile.t
(** Per-distro build image. Installs the generic build toolchain
    (compilers, headers, [curl]) together with [overlay_depexts] (the
    union of [depexts:] declared by every overlay's [x-root-packages]
    evaluated for this distro). It then fetches the latest statically
    linked [oi-linux-<arch>] from the [oi] GitHub releases page and
    sets [/work] as the working directory. No [CMD] is set: the
    compose file drives the build+export steps. *)

type opam_vars = {
  os_distribution : string;
  os_family : string;
  os_version : string;
}

val opam_vars_of_distro : Distro.t -> opam_vars
(** Map a supported docker distribution to the opam platform variables
    its opam filters expect. [os-family] follows opam's conventions
    ([debian], [rhel], [alpine], [suse], [arch]), [os-distribution] is
    the per-distro name ([ubuntu], [fedora], etc.), and [os-version] is
    the release string taken from the [Distro.t] tag. [os] is always
    [linux] for the distros this function supports. *)

val one_distro_filename : Distro.t -> string
(** Filename (without directory) for a per-distro Dockerfile, e.g.
    [Dockerfile.alpine-3.23]. *)

val service_name : Distro.t -> string
(** docker-compose service name for a distro, e.g. [alpine-3.23]. *)

val docker_compose_yaml :
  distros:Distro.t list ->
  registry_host_path:string ->
  unit ->
  string
(** [docker_compose_yaml ~distros ~registry_host_path ()] emits a compose
    file whose services each bind-mount [registry_host_path] at [/out] and
    run [oi registry build --refresh --all && oi registry export /out].
    Every container owns its own oi state — [oi] auto-clones the reporepo
    from its configured default URL (overridable via [OI_REPOREPO_URL] in
    the service environment) on first use — so containers are independent
    and safe to run in parallel. Layers are tagged with their overlay
    handle and version in the per-distro sqlite index so clients can scope
    queries to a specific overlay. *)

val write_dockerfile : string -> Dockerfile.t -> unit
(** Serialise a {!Dockerfile.t} to [path] in Dockerfile syntax. *)

val write_file : string -> string -> unit
(** Write arbitrary string contents to [path]. *)
