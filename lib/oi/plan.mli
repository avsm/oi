(** Build plan: dependency graph of package actions, and the fully-resolved
    execution plan derived from it.

    Two-stage:
    - {!of_solution} returns a {!graph} (the action DAG with [method_] / layer
      hashes decided per package). This is the structural plan suitable for
      dry-run output and layer-cache lookups.
    - {!elaborate} expands a {!graph} into a {!t} with all opam variables,
      filters, and commands turned into concrete shell commands. Packages are
      listed in topological order; the executor schedules them as a DAG, with
      each package's dep promises gating its own work, so there is no notion of
      a discrete "stage" or "group" past this point.

    Build directories are deterministic under the oi cache root at
    [{cache_root}/build/_build/{pkg}-{short_hash}/], with the shared prefix at
    [{cache_root}/build/prefix/]. Each package lists the dependency layers that
    must be hardlinked into the prefix before building. *)

[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** {1 Action graph}

    The full dependency DAG with per-package build phases. When [~d10] is
    provided to {!build}, packages whose layers already exist in the cache are
    marked as [Binary] and restored directly instead of being built from source.
*)

type node = {
  pkg : OpamPackage.t;
  opam : OpamFile.OPAM.t;
  method_ : Identity.method_;
  deps : OpamPackage.Name.t list;
      (** Direct dependencies within the plan (determines execution order). *)
  layer_hash : string;
      (** Content hash of the package plus its transitive in-plan deps. *)
}
(** A single package action in the plan. *)

type graph
(** A DAG of nodes keyed by package name. *)

exception Cycle of OpamPackage.t list list
(** Raised by {!of_solution} when the solved package set contains a dependency
    cycle (one or more strongly-connected components in the in-plan dep graph
    with size > 1, or a self-loop). Each inner list is one cycle in discovery
    order; multiple distinct cycles surface as separate inner lists.

    Cycles are an upstream-metadata bug: opam-0install solves cyclic
    [{= version}] depend-pairs without complaint, but {!Execute.run}'s
    fiber-per-package model deadlocks because each package's [await_dep] sleeps
    on a promise the cycle prevents from ever resolving. Callers should catch
    this and skip the offending solve group rather than letting it wedge. *)

val pp_cycles : OpamPackage.t list list Fmt.t
(** Render a list of cycles, one per line. *)

val of_solution :
  Solver.Ctx.t ->
  ?d10:D10.Config.t ->
  packages_dirs:string list ->
  OpamPackage.t list ->
  graph
(** [of_solution ctx ?d10 ~packages_dirs pkgs] builds an action graph from a
    topologically-sorted package list. When [d10] is given, checks the layer
    cache and marks cached packages as [Binary]. Raises {!Cycle} when the
    in-plan dep graph contains a cycle. *)

val nodes : graph -> node list
(** [nodes g] is the list of all nodes in topological order. *)

val overlay_of_pkg :
  packages_dirs:string list -> OpamPackage.t -> D10.Overlay.t option
(** [overlay_of_pkg ~packages_dirs pkg] resolves the overlay that contributed
    [pkg]'s opam file, by walking [packages_dirs] for a
    [<dir>/<name>/<name.version>/opam] hit. Returns [None] for packages from a
    non-overlay source (pin-depends, raw URL, local). *)

val layer_hashes : graph -> string list
(** [layer_hashes g] returns the layer hash for each package in the plan, in
    topological order. Used for prefix assembly. *)

(** {1 Executable plan}

    Fully resolved build/install commands in topological order. Each package
    knows its direct in-plan deps via {!package_plan.dep_layers}; the executor
    treats this as a DAG of fibers gated on per-package promises. *)

type source_info = { url : string; checksums : string list }
type patch = { file : string; filter : string option }
type subst = string

type package_plan = {
  pkg : string;
  opam : OpamFile.OPAM.t;
      (** Parsed opam metadata for this package. Carried so callers that need
          [synopsis], [license], [homepage], [depends:], etc. can read them
          without re-loading the opam file from disk — and so
          {!Build_pipeline.group_result} can drop its [build_plan] field once
          every consumer of {!node.opam} moves to {!package_plan.opam}. *)
  layer_hash : string;
  method_ : Identity.method_;
  dep_layers : Identity.dep list;
  source : source_info option;
  extra_sources : (string * source_info) list;
  extra_files : (string * string) list;
      (** Opam [extra-files:] entries: pairs of [(basename, src_path)] where
          [src_path] is the absolute path to the file in
          [<packages_dir>/<name>/<name.version>/files/<basename>]. Copied into
          [build_dir] before patches are applied, so inline patches the recipe
          references actually exist when [patch -i] runs. *)
  patches : patch list;
  substs : subst list;
  subst_vars : (string * string) list;
  build_commands : string list list;
  install_commands : string list list;
  install_file : string;
  env : string array;
  build_dir : string;
  prefix : string;
  overlay : D10.Overlay.t option;
      (** Reporepo overlay that contributed this package's opam file (e.g.
          [{ handle = "avsm"; version = "" }]), or [None] when the package came
          from a non-overlay source like a pin-depends tree. *)
  opam_path : string option;
      (** Absolute path to the opam file the solver picked for this package (the
          [<packages_dir>/<name>/<name.version>/opam] hit from
          {!find_pkg_source_dir}). Used by {!Provenance} to compute the opam
          [sha256] and origin path. [None] if the package's opam file isn't
          locatable on disk. *)
  pkgs_dir : string option;
      (** The [<packages_dir>] root that contained [opam_path]. Distinct from
          [opam_path] so {!Origin.of_packages_dir} can examine the directory
          shape (v2/<handle>/packages, pins/sets/<hash>/packages, …). [None] iff
          [opam_path] is. *)
  depexts : string list;
      (** Declared system packages active under the build's filter env. Names
          are taken straight from [OpamFile.OPAM.depexts] and only retained for
          the entries whose filter evaluates true. Empty if the package declares
          none. *)
  d10_archive : string option;
      (** Pre-baked d10ir source archive sha256, if the opam file declares
          [x-d10-archive] (filter-evaluated for the current OS). When
          [Some sha], the consolidated archive at
          [<cache>/d10ir/archives/<sha>.tar.zst] is the build input — fetch it
          instead of running the source/extras/patches/substs pipeline. The
          archive contains pre-applied patches and substituted [.in] files using
          the planning-prefix sentinel; d10ir's per-node rebase still runs.
          [None] for packages without [x-d10-archive] (pre-bake fallback to the
          regular fetch flow). *)
}

type t = {
  packages : package_plan list;
      (** Every package in the plan, in topological order. The executor walks
          this list to spawn one fiber per package; each fiber awaits its
          [dep_layers]'s promises before starting its own work. *)
  cache_root : string;
  os_key : string;
  ocaml_version : string;
  build_prefix : string;
      (** Planning sentinel: the prefix path that {!Solver.Ctx.resolve} baked
          into every package's commands and env at resolution time. The executor
          doesn't actually install into this path — it computes a per-package
          staging dir under [<cache_root>/build/staging/<hash>/] and
          string-rebases [build_prefix → staging] in commands and env before
          running the build. Hermetic per-package installs eliminate the
          shared-prefix mutex and let restores / installs run in parallel. *)
  external_layer_hashes : string list;
      (** Layer hashes of toolchain packages the host provides (non-relocatable
          toolchain switch). Not in {!packages} — they are not built by the
          d10ir executor and have no d10 layer — but their hashes still appear
          in consumer nodes' [dep_layer_hashes] (so consumer layers invalidate
          when the toolchain changes). The d10ir recipe surfaces them via
          [D10ir.Plan.external_layers] so the executor and validator treat the
          references as already-satisfied. *)
}

val elaborate :
  Solver.Ctx.t ->
  packages_dirs:string list ->
  cache_root:string ->
  os_key:string ->
  ocaml_version:string ->
  ?build_prefix:string ->
  ?reporter:Build_progress.reporter ->
  graph ->
  t
(** [elaborate ctx ~packages_dirs ~cache_root ~os_key ~ocaml_version
     ?build_prefix ?reporter g] turns an action graph into a fully-resolved
    execution plan.

    [packages_dirs] is the same list passed to the solver, used to attribute
    each package to its source directory so the resulting plan (and any layer
    built from it) can be tagged with the overlay that contributed the opam
    file. [?build_prefix] defaults to [{cache_root}/build/prefix]; pass a
    different path to build into a fixed location (e.g. a toolchain prefix).

    [?reporter] receives a [Status "Elaborating plan"] event when work begins.
    Plan elaboration is fast (in-memory walk), so no per-node [Aggregate] events
    are emitted — the count fraction belongs to the actual layer-build phase
    that follows. Defaults to {!Build_progress.null}. *)

val pp : t Fmt.t
