(** Shared CLI-to-pipeline glue.

    Every command that needs to "solve and build" a target set ([oi run],
    [oi build], [oi install], …) duplicates the same ~60 lines of setup:
    opam-root init, reporepo [ensure_base], conf construction,
    [Terms.remotes_of], [Pipeline.classify_with_args], project [Project.load],
    toolchain pickup, project-overlay filtering, and
    {!Oi.Build_pipeline.request} construction. {!prepare} hoists that
    boilerplate behind one entry point so a new command only writes its own CLI
    surface and a few lines around the solve/build call. *)

type prepared = {
  env : Oi.Build_pipeline.env;
      (** Capabilities + cache bundle to feed [Build_pipeline.solve / .build].
      *)
  request : Oi.Build_pipeline.request;
      (** Fully populated request: parsed targets + scoped overlays + merged
          extras + resolved toolchain + project pins. *)
  layer_remote : D10.Layer.remote option;
      (** Layer registry (from [--registry] / [--use-registry]); pass into
          [Build_pipeline.build_inputs]. *)
  source_remote : D10.Layer.remote option;
      (** Source-archive registry, same source. *)
  toolchain : Oi.Toolchain.info option;
      (** The toolchain {!Pipeline.pick_toolchain} picked. Some callers thread
          this further (e.g. [oi run] uses it for [Solver.Env.make_env] when
          assembling the consumer prefix). *)
  cwd : string;
      (** Resolved cwd, returned so callers that need it (project mode
          detection, "no targets in this directory" error messages) don't have
          to re-derive it via {!Workspace.resolved_cwd}. *)
}

val prepare :
  harness:Harness.env ->
  refresh:bool ->
  locked:bool ->
  skip_local:bool ->
  registry:string ->
  use_registry:Oi.Use_registry.t ->
  with_repos:string list ->
  with_deps:string list ->
  toolchain_override:string option ->
  targets:Oi.Build_pipeline.target list ->
  ?extra_handles:string list ->
  ?extra_pins:Oi.Project.pin list ->
  ?extra_constraints:OpamFormula.version_constraint OpamPackage.Name.Map.t ->
  unit ->
  prepared
(** [prepare ~harness ~refresh ~locked ~skip_local ~registry ~use_registry
     ~with_repos ~with_deps ~toolchain_override ~targets ?extra_handles
     ?extra_pins ?extra_constraints ()] runs the shared opam-init / reporepo /
    project / toolchain pipeline and builds the {!Oi.Build_pipeline.request} a
    downstream solve/build call consumes.

    [~locked] propagates into [refresh] (locked forces it off) and
    [use_registry] (locked forces {!Oi.Use_registry.Never}). [~skip_local]
    suppresses the cwd [Project.load].

    [?extra_handles] adds at-handles to the toolchain-detection scan beyond
    what's already implied by [targets] / [with_repos] / project x-repos.
    [?extra_pins] and [?extra_constraints] are merged into the resulting request
    — used by [oi run] for [@h/pkg]-driven version pins. *)
