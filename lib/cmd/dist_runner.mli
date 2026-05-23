(** The "build a plan, run it, emit a Makefile + Dockerfiles" engine behind
    [oi dist makefile]. Extracted from {!Dist_cmd} so it can be re-used by
    [oi dist bundle] ({!Osdist_bundle}) without creating a Dist_cmd ⇄
    Osdist_bundle module cycle. *)

val coalesce_targets : string list -> Oi.Build_pipeline.target list
(** [coalesce_targets toks] groups plain package names and overlay-package specs
    into a single [Group] target (so they share one solve), preserving
    [Overlay_all] / [Group] entries as separate targets. The result is what
    {!Pipeline_setup.prepare} expects as [~targets]. *)

val ensure_toolchain_built :
  harness:Harness.env ->
  pipeline_env:Oi.Build_pipeline.env ->
  req:Oi.Build_pipeline.request ->
  layer_remote:D10.Layer.remote option ->
  source_remote:D10.Layer.remote option ->
  targets:string list ->
  clock:D10.Config.clk ->
  string list
(** Run a full [oi build] over the request so the relocatable toolchain
    aux-installs itself and every source archive is cached locally. Returns the
    built-binaries list (for the Makefile header / [make dest] summary — callers
    that don't care can ignore the result). This pre-build step is what causes a
    subsequent [Build_pipeline.solve ~force_source:true] to include the
    relocatable compiler in [plan.nodes] rather than leaving it as an empty
    stub. *)

val unpack_sources :
  harness:Harness.env ->
  registry:string ->
  plan:D10ir.Plan.t ->
  output:string ->
  unit
(** [unpack_sources ~harness ~registry ~plan ~output] explodes every buildable
    node's [<sha>.tar.zst] from the local d10 cache into
    [<output>/sources/<sha>/]. Missing archives are first prefetched in parallel
    from [<registry>/d10ir-archives/<sha>.tar.zst]; if any sha is still missing
    after the prefetch attempt this raises [Oi.Error.fail_config_error] with the
    list. Skips dirs that already look populated, so re-running is cheap. *)

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
    [oi build], and emits a portable Makefile + per-distro Dockerfiles into
    [output]. *)
