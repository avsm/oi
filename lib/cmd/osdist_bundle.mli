(** Internal: source-bundle production for {!Osdist_pkg}.

    Drives the {!Dist_runner} makefile flow into a staging dir, adds a
    [build.sh] wrapper + README, computes per-distro overlay depexts, tars the
    staging dir to [<output>/bundle/<pkg>-<ver>.tar.gz], and writes the
    {!Osdist.Spec.t} sidecar.

    Returns [(bundle_path, spec)] so the caller can hardlink the tarball into
    per-target dirs and feed [spec] to the per-family generators. *)

val produce :
  harness:Harness.env ->
  refresh:bool ->
  registry:string ->
  use_registry:Oi.Use_registry.t ->
  with_repos:string list ->
  with_deps:string list ->
  toolchain_override:string option ->
  targets:string list ->
  pkg_name:string option ->
  version:string option ->
  epoch:int option ->
  maintainer:string option ->
  homepage:string option ->
  license:string option ->
  prefix:string option ->
  output:string ->
  string * Osdist.Spec.t
