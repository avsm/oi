(** End-to-end pipeline: solve → plan → build → assemble.

    The CLI commands compose these operations: most do {!pick_toolchain} →
    {!Build_pipeline.solve} + {!Build_pipeline.build} → {!assemble_prefix}, then
    exec into the result. The smaller helpers ({!cache_urls},
    {!fetch_layer_hashes}) are exported so callers can intercept the pipeline at
    intermediate points. *)

[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** {1 Platform configuration and d10 wiring} *)

val conf : platform:Osrel.t -> ocaml_version:string -> Solver.Ctx.conf
(** Build a platform conf from a detected {!Osrel.t} plus an explicit OCaml
    version. *)

val d10 :
  sys:D10.Sysops.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:D10.Config.clk ->
  cache:Cache.t ->
  os_key:string ->
  D10.Config.t
(** [d10 ~fs ~clock ~sys ~cache ~os_key] constructs the {!D10.Config.t} every
    layer-store call expects. Combines [cache]'s root with [os_key] so layers
    from different platforms land in their own subdirectories. *)

val init_opam_root : fs:Eio.Fs.dir_ty Eio.Path.t -> data_dir:string -> unit
(** [init_opam_root ~fs ~data_dir] creates [<data_dir>/opam-root/] (if absent)
    and calls {!Solver.Ctx.init_opam} so the rest of the pipeline can construct
    {!Solver.Ctx.t}s. Idempotent. *)

(** {1 Toolchain resolution} *)

val solver_inputs :
  Toolchain.info option ->
  Solver.Ctx.conf ->
  Solver.Ctx.conf * Solver.Ctx.toolchain option
(** [solver_inputs info conf] derives the two views the rest of the pipeline
    consumes: a [conf] with [ocaml_version] aligned to the toolchain's compiler,
    and the {!Solver.Ctx.toolchain} subset {!Solver.Ctx.create} /
    {!Solver.Env.make_env} take. *)

val toolchain_names_of_handle :
  Source.Reporepo.entry list -> string -> string list
(** [toolchain_names_of_handle entries h] is the (possibly empty) list of
    toolchain names that the handle [h] points at: the value of its
    [x-oi-toolchain] field plus [h] itself if [h] is registered as a toolchain
    definition. Used by callers that need to partition a target set by toolchain
    (e.g. [oi build --all] across overlays with conflicting toolchains). *)

val pick_toolchain :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  data_dir:string ->
  conf:Solver.Ctx.conf ->
  install:bool ->
  override:string option ->
  handles:string list ->
  ?reporter:Build_progress.reporter ->
  unit ->
  Toolchain.info option
(** [pick_toolchain] is the single source of truth for picking which toolchain a
    command runs against. Selection precedence (first match wins):

    + [override = Some h]: resolve [h] directly. The [--toolchain=NAME] flag.
    + Implicit pickup from [handles]: scan each handle's latest reporepo entry
      for an [x-oi-toolchain] field, and check whether the handle itself names a
      toolchain definition ([x-oi-toolchain-name]). A unique non-empty result
      wins; multiple distinct names hard-error with a "pass --toolchain=NAME to
      disambiguate" hint.
    + Reporepo default: the entry flagged [x-oi-default-toolchain: true].
    + Nothing matched: hard-error. The reporepo is expected to declare a
      default; [oi repo lint] enforces this.

    [install:true] runs {!Toolchain.ensure_installed} on the chosen toolchain;
    the actual source-build of the toolchain prefix is driven by
    {!Oi.Aux_install.ensure} via {!Build_pipeline.aux_installer}.
    [install:false] returns the [info] without preparing its on-disk prefix.
    Compose with {!solver_inputs} when the caller also needs the [conf] /
    [Ctx.toolchain] views. The lower-level [Toolchain.resolve] handles a single
    named handle without precedence rules. *)

val strip_compiler_roots_for_override :
  override:string option ->
  toolchain:Toolchain.info option ->
  OpamPackage.Name.t list ->
  OpamPackage.Name.t list
(** [strip_compiler_roots_for_override ~override ~toolchain names] removes
    compiler-family root names (i.e. those in [tc.root_names]) from [names] when
    [override] is set. Used to substitute the explicit [--toolchain=NAME]'s
    compiler pins for whatever the call-site's overlays / project would
    otherwise have asked for, avoiding [conflict-class] failures on
    [ocaml-core-compiler]. No-op when [override] is [None] (the toolchain came
    from implicit / default pickup, so its roots already line up with the
    handles in scope). *)

(** {1 Sources} *)

val classify_with_args :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  cache:Cache.t ->
  ?refresh:bool ->
  ?reporter:Build_progress.reporter ->
  string list ->
  Project.Script.dep list * Project.Url.t
(** [classify_with_args] partitions every [--with=…] token in one pass: URLs get
    cloned into the pin cache and produce pins + solver roots; opam package
    specs come back as already-parsed {!Project.Script.dep}. *)

val filter_compatible_overlays :
  reporepo_path:string ->
  ?override:string option ->
  toolchain:Toolchain.info option ->
  string list ->
  string list
(** [filter_compatible_overlays ~reporepo_path ?override ~toolchain handles]
    drops project-declared reporepo overlays from [handles] when they're tagged
    with an [x-oi-toolchain] that doesn't match the active toolchain handle.
    Overlays without [x-oi-toolchain] and overlays not present in the reporepo
    (URL-only handles) are kept.

    [?override] reflects whether the active toolchain was {b explicitly} chosen
    ([--toolchain=NAME]) rather than auto-picked. When set, the filter is a
    no-op: the user has overridden the project's toolchain preference, so we
    trust them to know that their declared overlays still apply. Default is
    [None] (auto-pick mode), in which case the filter runs. *)

(** {1 Build pipeline} *)

val cache_urls :
  cache:Cache.t -> source_remote:D10.Layer.remote option -> OpamUrl.t list
(** [cache_urls] for opam's [pull_tree]/[pull_file] to probe before falling back
    to upstream: always includes the local {!Source.Mirror}; with a remote
    [source_remote], also the registry's [sources/] subtree. *)

val fetch_layer_hashes :
  ?reporter:Build_progress.reporter ->
  ?jobs:int ->
  session:D10.Sysops.Http.session ->
  remote:D10.Layer.remote ->
  d10:D10.Config.t ->
  index:D10.Layer.remote_index ->
  hashes:string list ->
  pkg_of:(string, string) Hashtbl.t ->
  unit ->
  unit
(** [fetch_layer_hashes] fetches a deduplicated list of layer hashes directly.
    Used by [oi build]'s merged-plan flow, which collects layer hashes from the
    unified {!D10ir.Plan.t} (post-{!D10ir.Plan.merge}) and fires one fetch round
    against the registry instead of N per-group rounds. [pkg_of] maps a hash to
    its display label. *)

val assemble_prefix :
  sys:D10.Sysops.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:_ Eio.Time.clock ->
  cache:Cache.t ->
  os_key:string ->
  layer_hashes:string list ->
  string
(** [assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes]
    hardlink-assembles a consumer prefix from [layer_hashes] (in topo order) and
    returns its absolute path. *)
