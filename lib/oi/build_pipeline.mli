(** Unified solve / merge pipeline.

    Every command that needs a build plan ([oi show], [oi build], [oi run]) goes
    through {!solve}. The function takes a {!request} (target tokens + scope),
    drives the solver and elaborator per-group, emits one d10ir recipe per
    successful group, and merges them into a single {!D10ir.Plan.t}.
    Single-target requests merge a 1-element recipe list — same code path, no
    special case.

    Per-group failures (solver errors, cycles, empty groups after toolchain
    stripping) land in [group_result.error] rather than raising, so a 50-root
    [oi build @overlay] with one bad root still returns 49 successful groups and
    a merged plan derived from them.

    {!solve} is purely the "decide what to build" half of the pipeline. Running
    the merged plan ([Direct.run] + fetch + archive prewarm) lives in {!build}.
*)

type env = {
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  sys : D10.Sysops.t;
  os_key : string;
  cache : Cache.t;
  data_dir : string;
  http_session : D10.Sysops.Http.session;
}
(** {1 Environment}

    Same shape as [Harness.env] minus [format] (a presentation concern). Lives
    here so non-cmdliner callers can construct it. *)

(** {1 Targets and requests} *)

(** Parsed target token. {!parse} converts CLI strings into this shape; callers
    may also construct values directly. *)
type target =
  | Plain of string
      (** Bare package name or [name.version] / [name>=ver] atom. One singleton
          group, no extra overlay handles in scope. *)
  | Group of { tokens : string list; handles : string list }
      (** Multiple package specs that solve together as one group, with
          [handles] in scope for the solve. Used both by [oi run] (target +
          [--with] roots, no extra handles) and by [oi build] (each pre-expanded
          reporepo solve group, with its own handle scope). *)
  | Overlay_pkg of { handle : string; spec : string }
      (** [@HANDLE/PKG_SPEC]. Sugar for
          [Group { tokens = [spec]; handles = [handle] }]. *)
  | Overlay_all of string
      (** Bare [@HANDLE] — fanned out into one group per package the overlay's
          [x-root-packages] declares (or one group per package the overlay ships
          when [x-root-packages] is empty). *)

val parse : string -> target
(** [parse "utop"] = [Plain "utop"]; [parse "@avsm"] = [Overlay_all "avsm"];
    [parse "@avsm/karakeep"] = [Overlay_pkg ...]. Never produces {!Group} — that
    constructor is for callers that want to combine names into one solve. *)

type request = {
  targets : target list;
  with_repos : string list;
      (** Extra overlay handles in scope for every group's solve. *)
  pins : Project.pin list;
  extra_repos : Project.extra_repo list;
      (** Pre-resolved extras (CLI [--with-repo] URLs + project's [x-repos:]);
          the solver consumes these directly. *)
  constraints : OpamFormula.version_constraint OpamPackage.Name.Map.t;
      (** Cross-group constraints (e.g. from [--with foo=1.2.3]). *)
  toolchain_override : string option;
      (** [--toolchain=NAME] — when set, strips compiler-family roots from each
          group's solve. *)
  toolchain : Toolchain.info option;
      (** Pre-resolved toolchain. When [Some _], {!solve} skips its internal
          {!Pipeline.pick_toolchain} call and uses this directly — used by
          callers (e.g. [oi run]) that resolved the toolchain earlier in their
          flow and want to make sure the same one is in effect. *)
  conf : Solver.Ctx.conf;  (** Solver conf (host or [--os]-overridden). *)
  local_packages_dir : string option;
      (** Optional local [packages/] tree (e.g. URL-project pin materialisation)
          that should sit ahead of every solve's dirs. *)
  project_root : string option;
      (** When set, pin URL resolution consults / refreshes
          [<project_root>/_oi/oi.lock] before materialisation. *)
  force_source : bool;
      (** Skip d10 cache classification during planning so every package becomes
          [Source] regardless of whether its layer is locally cached. Used by
          recipe-emission flows ([oi ir emit]) so {!Recipe_emitter.emit}
          materialises an archive for every package; the alternative would be an
          empty recipe against a fully-warm cache. *)
  refresh : bool;
}

(** {1 Solve groups and per-group results} *)

type group = {
  label : string;
      (** Human-readable group identifier (e.g. [@avsm/karakeep] or
          ["utop, uchar"]). Used by the reporter and the audit. *)
  tokens : string list;
      (** The original raw tokens that produced this group. *)
  names : OpamPackage.Name.t list;
  handles : string list;
      (** Overlay handles in scope for this group's solve (group's own [@h]
          tokens + every [request.with_repos] entry). *)
  group_constraints : OpamFormula.version_constraint OpamPackage.Name.Map.t;
}

(** Why a group did not produce a recipe. *)
type group_error =
  | Solve_failed of { msg : string; log_path : string }
  | Cycle of OpamPackage.t list list
  | Empty_after_strip
      (** Every token was a compiler-family root that [--toolchain=NAME]
          stripped. *)
  | Elaborate_failed of { msg : string }
  | Emit_failed of { msg : string }

type group_result = {
  group : group;
  toolchain : Toolchain.info option;
  pkgs_dir : string list;  (** [packages_dirs] used for this group's solve. *)
  pkgs : OpamPackage.t list;  (** Solved package set (empty on failure). *)
  exec_plan : Plan.t option;
      (** Elaborated plan (the [Plan.t] returned by {!Plan.elaborate}). Carries
          every package's [Plan.package_plan], which now includes [opam] so
          callers that want metadata (e.g. [oi show]'s tree + summary view)
          don't need a separate [Plan.graph]. *)
  recipe : D10ir.Plan.t option;
      (** Source-only recipe consumed by {!build} via {!D10ir.Plan.merge}. Empty
          when every package in the group is [Binary] (cache-restorable). *)
  error : (unit, group_error) result;
}

(** {1 The [solve] entry point} *)

type solved = {
  groups : group_result list;
      (** One entry per group, in the order they were processed. *)
  merged : D10ir.Plan.t option;
      (** [Some _] when at least one group emitted a recipe; the merge of every
          successful group's recipe (deduped by layer hash). [None] when every
          group failed. *)
  toolchain : Toolchain.info option;
      (** The single toolchain selected for the whole batch — same one that
          {!Pipeline.pick_toolchain} would return. *)
}

val solve : env -> ?reporter:Build_progress.reporter -> request -> solved
(** [solve] Solve every group in [request.targets], elaborate each into an
    {!Plan.t}, emit one in-memory {!D10ir.Plan.t} per successful group, and
    merge the recipes via {!D10ir.Plan.merge}.

    The function never raises for a per-group failure — those land in
    [group_result.error]. It can still raise for environment-level problems
    (missing reporepo, toolchain not resolvable, etc.) since those affect the
    whole batch.

    [reporter] receives [Phase_started Solving], one [Solve_started/Finished]
    pair per group, and an [Aggregate] progress event after each group.
    {!Plan_ready} on the merged plan and {!Total_estimate} are NOT fired here —
    those belong to the build-execution phase, which is a separate function.

    Results are persistently cached under [<env.cache>/solve-cache/] keyed by
    the reporepo HEAD + each in-scope handle's overlay commit + the request's
    targets / constraints / conf / pins. A cache hit skips every per-group
    [Solver.Ctx.create] + [Plan.of_solution] + [Plan.elaborate] +
    [Recipe_emitter.emit] pass and returns the prior {!solved} directly. The
    cache is bypassed when [request.refresh = true] or when any [packages_dir]
    can't be signature-stamped (e.g. transient pin tree). *)

(** {1 Build execution on the merged plan} *)

type build_inputs = {
  solved : solved;
  layer_remote : D10.Layer.remote option;
      (** Registry to pull pre-built binary layers from. [None] forces every
          layer to be built from source. *)
  source_remote : D10.Layer.remote option;
      (** Registry to pull pre-baked source archives from. [None] requires every
          needed archive to already exist locally (produced by [oi repo bump]);
          a missing archive is a hard error. *)
  jobs : int option;  (** Build parallelism. [None] uses the d10ir default. *)
  upload_archive_url : string option;
      (** [Some "s3://oiu/"]: after the build completes, mirror every
          {b freshly built} layer to
          [<URL>/<os_key>/layers/<hash>.tar.zst] (and its companion
          [<hash>.json] manifest) via [s3cmd put]. Also publishes the
          d10ir source-manifest sidecars under [<URL>/d10ir/<sha>.json]
          and the per-invocation build manifest under
          [<URL>/<os_key>/builds/<YYYY>/<MM>/...]. [None] disables the
          upload entirely. Per-layer failures are logged via the
          reporter but do not abort the build — the layer is already in
          the local cache regardless of upload outcome. *)
  archive_sources : bool;
      (** When [true], also PUT the consolidated source tarballs at
          [<URL>/d10ir/<sha>.tar.zst]. Default [false]: only the JSON
          sidecars are uploaded, on the assumption that upstream remotes
          can be re-fetched on demand. Flip on for self-contained,
          upstream-deletion-proof buckets. *)
  snapshot_reporepo : bool;
      (** When [true], tarball the reporepo at the solve-time HEAD
          commit and PUT it to [<URL>/reporepo/<commit>.tar.zst].
          Default [false]: a downstream consumer is expected to clone
          the reporepo from its own URL. Flip on when the reporepo is
          private and the bucket needs to be the canonical archival
          source. *)
}

val build :
  env ->
  ?reporter:Build_progress.reporter ->
  build_inputs ->
  D10ir.Direct.result option
(** [build] Run the unified post-merge fetch + archive prefetch +
    [D10ir.Direct.run] on [solved.merged].

    Returns [None] when [solved.merged = None] (every group failed its solve /
    elaborate / emit, so there's nothing to build). Otherwise [Some result]
    where [result] is {!D10ir.Direct.result}; per-node failures live in
    [result], the caller maps them back to its own [solved.groups[i].exec_plan]
    for end-of-run accounting.

    Fires every event in the build half of the {!Build_progress} surface:
    [Total_estimate] (with the deduped fetch / build counts derived from
    [solved.merged]), [Phase_started Fetching],
    per-{!Fetch_started}/[Fetch_progress]/[Fetch_finished], then
    [Phase_started Building] / [Build _] / [Build_summary]. *)

val layer_hashes : solved -> string list
(** [layer_hashes] Layer hashes from the first (and usually only) successful
    group's [exec_plan], in topological order. Empty when [solved.groups] has no
    successful entry. The shape every single-target caller ([oi env], [oi run],
    [oi self], [oi sync], [Script_runner]) uses to feed
    {!Pipeline.assemble_prefix}. *)

val root_layer_hashes : solved -> string list
(** [root_layer_hashes] Layer hashes of the user-requested packages across every
    successful group, deduped. Differs from {!layer_hashes} by filtering
    [exec_plan.packages] down to the names listed in each group's [group.names]
    — i.e. the seed packages the solver was asked about, not the transitive
    closure.

    Use this (not [solved.merged.roots]) anywhere you want "the binaries the
    user actually asked for": [oi install] copies them to a prefix;
    [oi build --dist=DIR] does the same into [DIR]. [solved.merged.roots] would
    under-report here because the d10ir merge only carries [Source]-mode
    packages, so it goes empty whenever every requested package is already in
    the local layer cache. *)
