(** The "build a plan, run it, emit a Makefile + Dockerfiles" engine behind
    [oi dist makefile]. Extracted from {!Dist_cmd} so it can be re-used by
    [oi dist bundle] ({!Osdist_bundle}) without creating a Dist_cmd ⇄
    Osdist_bundle module cycle. *)

val run_makefile :
  harness:Harness.env ->
  refresh:bool ->
  registry:string ->
  use_registry:Oi.Use_registry.t ->
  with_repos:string list ->
  with_deps:string list ->
  toolchain_override:string option ->
  targets:string list ->
  output:string ->
  unit
(** [run_makefile ~harness ~refresh ~registry ~use_registry ~with_repos
    ~with_deps ~toolchain_override ~targets ~output] solves the requested
    target (or the cwd project, when [targets = []]), runs a validating
    [oi build], and emits a portable Makefile + per-distro Dockerfiles
    into [output]. *)
