[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** System-package requirements for a solved package set.

    Reads [depexts:] from each package's opam file, evaluates the
    associated filter against the current platform variables, and
    returns the set of system package names required. Does not
    install anything — callers print, the user installs. *)

type entry = { pkg : OpamPackage.t; sys_pkgs : OpamSysPkg.Set.t }

val compute :
  Opam_ctx.t ->
  packages_dirs:string list ->
  OpamPackage.t list ->
  entry list
(** [compute ctx ~packages_dirs solved] returns one [entry] per opam
    package in [solved] that contributes at least one depext on the
    current platform. Order matches [solved]. Packages with no
    active depext are dropped. *)
