[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** System-package requirements for a solved package set.

    Reads [depexts:] from each package's opam file, evaluates the associated
    filter against the current platform variables, and returns the set of system
    package names required. Does not install anything: callers print, the user
    installs. *)

type entry = { pkg : OpamPackage.t; sys_pkgs : OpamSysPkg.Set.t }

val compute :
  Opam_ctx.t -> packages_dirs:string list -> OpamPackage.t list -> entry list
(** [compute ctx ~packages_dirs solved] returns one [entry] per opam package in
    [solved] that contributes at least one depext on the current platform. Order
    matches [solved]. Packages with no active depext are dropped. *)

val compute_for_conf :
  conf:Opam_ctx.conf ->
  packages_dirs:string list ->
  OpamPackage.t list ->
  entry list
(** [compute_for_conf ~conf ~packages_dirs solved] is like {!compute} but
    evaluates depext filters against a synthetic [conf] rather than the
    platform the host opam state picked up. Intended for cross-platform
    queries such as "what would the depexts be on alpine?" without
    rebuilding an {!Opam_ctx.t}. *)

(** {1 Host installation status} *)

type status = {
  installed : OpamSysPkg.Set.t;
      (** Depexts the host's package manager reports as present. *)
  missing : OpamSysPkg.Set.t;
      (** Depexts the package manager knows about but has not installed. *)
  not_found : OpamSysPkg.Set.t;
      (** Depexts the package manager does not recognise (third-party repo,
          renamed, missing on Windows, etc.). *)
}

val status : OpamSysPkg.Set.t -> status
(** [status pkgs] asks the host's package manager which of [pkgs] are already
    installed, which are available but not installed, and which are unknown.
    Implemented by shelling out through [OpamSysInteract.packages_status], so
    the answer only reflects the local machine. A failure to reach the package
    manager (Windows without Cygwin, unknown distro, etc.) classifies every
    input as [missing]. *)
