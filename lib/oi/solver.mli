(** Solver subsystem: synthetic opam state, environment, persistent cache, and
    the entry points that wrap [opam-0install]. *)

[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** {1 Synthetic opam switch state}

    Owned by {!Ctx}. Builds an [OpamSwitchState.t] backed by a build prefix and
    package repositories without using ~/.opam, plus the platform configuration
    ({!Ctx.conf}) the rest of the pipeline keys off. *)

module Ctx : sig
  (** {2 Platform configuration} *)

  type conf = {
    arch : string;
    os : string;
    os_distribution : string;
    os_version : string;
    os_family : string;
    ocaml_version : string;
    jobs : int;
  }

  (** {2 Context} *)

  type t

  type toolchain = {
    install_prefix : string;
    hash : string;
        (** Content hash of the toolchain's solved opam files + platform conf,
            used by {!Cache} to key consumer solves on the toolchain identity
            without enumerating its full package set. *)
    relocatable : bool;
        (** [true] when the compiler can be installed into the consumer prefix
            rather than at [install_prefix]. {!create} skips the
            [mark_installed] pre-population in that case so the consumer solve
            actually builds the compiler, and {!switch_env} skips the toolchain
            PATH/lib layering — the binary cache then matches what no-toolchain
            mode would produce. The version pinning via [packages]/[root_names]
            still applies. *)
    packages : OpamPackage.Set.t;
    root_names : OpamPackage.Name.Set.t;
        (** Solver-root subset of [packages]. The consumer solve uses this to
            force the originally-specified toolchain roots into the solution
            (via [conflict-class] those then exclude the wrong compiler) without
            dragging in every transitive dep — adding the full [packages] set as
            roots pulls in oxcaml meta packages that conflict with each other.
        *)
  }
  (** Subset of {!Toolchain.info} that the solver subsystem actually needs.
      Keeps {!Ctx} independent of the toolchain resolution pipeline while still
      letting {!create} layer env vars and mark toolchain packages as
      pre-installed. *)

  val create :
    prefix:string ->
    packages_dirs:string list ->
    conf:conf ->
    ?toolchain:toolchain ->
    ?reporter:Build_progress.reporter ->
    unit ->
    t
  (** [?reporter] receives [Status] events bracketing the opam state load (one
      [Status "Loading opam state"] before the [packages_dirs] walk, one
      [Status "Loaded N packages …"] after). Defaults to {!Build_progress.null}.

      [?toolchain] pins a fixed-prefix OCaml toolchain (compiler, ocamlfind,
      ocamlbuild, ...) to this context. When set:
      - the switch env prepends toolchain [bin], [lib], and [lib/stublibs] to
        [PATH], [OCAMLPATH], and [CAML_LD_LIBRARY_PATH] so consumer builds
        resolve the compiler from the toolchain prefix;
      - toolchain packages are recorded so downstream code (solver constraints,
        execute-skip) can special-case them. *)

  val conf : t -> conf
  (** Platform configuration this context was created with. *)

  val toolchain : t -> toolchain option
  (** Fixed-prefix toolchain layered onto this context, or [None] when the
      context was built without one. *)

  val resolve :
    t ->
    OpamFile.OPAM.t ->
    ?local:OpamVariable.variable_contents option OpamVariable.Map.t ->
    OpamFilter.env
  (** Variable resolver for a package. *)

  val resolve_commands :
    t ->
    test:bool ->
    doc:bool ->
    dev_setup:bool ->
    ?build_dir:string ->
    OpamFile.OPAM.t ->
    string list list
  (** Returns the fully resolved build commands with all opam variables expanded
      and filters evaluated. When [?build_dir] is given, it overrides
      [%{build}%] to that path. *)

  val compilation_env : t -> OpamFile.OPAM.t -> string array
  (** Full build environment: sanitized MAKEFLAGS, package-specific vars, and
      the switch environment. *)

  val resolve_substs : t -> OpamFile.OPAM.t -> (string * string) list
  (** Sorted association list mapping opam variable names to their resolved
      values, for expanding [.in] files at execution time. *)

  val mark_installed :
    t ->
    OpamPackage.t ->
    OpamFile.OPAM.t ->
    OpamFile.Dot_config.t option ->
    unit
  (** Mutate the synthetic switch state to record [pkg] as installed, so the
      solver pre-treats it as satisfied. Used to pre-populate the toolchain
      packages on a non-relocatable {!toolchain} so consumer solves don't try to
      rebuild the compiler. *)

  val synthetic_config :
    t -> OpamPackage.t -> OpamFile.OPAM.t -> OpamFile.Dot_config.t option
  (** Hard-coded .config for well-known compiler packages (currently [ocaml]) so
      that variables like [ocaml:native] can be resolved at plan time, before
      the package has actually been built. *)

  val platform_env : t -> OpamFilter.env
  (** Variable resolver for platform/global variables only (no per-package
      scope). Suitable for filtering dependency formulas during solving and
      topo-sorting. *)

  val init_opam : root:string -> unit
  (** Initialise opam's global config with an isolated root directory. *)
end

(** {1 OCaml-specific prefix environment}

    Wraps {!Ctx.switch_env} with dune-cache vars and renders the result for
    spawning subprocesses, writing [.envrc] files, etc. *)

module Env : sig
  val env_vars :
    ?toolchain:Ctx.toolchain ->
    prefix:string ->
    dune_cache_root:string ->
    unit ->
    (string * string) list
  (** [env_vars ~prefix ~dune_cache_root ()] returns the [(KEY, VALUE)] list
      that activates the assembled [prefix] for OCaml tooling: [PATH],
      [OCAMLPATH], [CAML_LD_LIBRARY_PATH], [OCAMLFIND_*], plus [DUNE_CACHE_ROOT]
      pointing at the shared cache. Adding [toolchain] also layers the
      fixed-prefix compiler's [bin] / [lib] paths in front of the consumer
      prefix's. *)

  val envrc_content :
    ?toolchain:Ctx.toolchain ->
    prefix:string ->
    ?tools:string ->
    dune_cache_root:string ->
    unit ->
    string
  (** [.envrc] contents activating [prefix]. When [tools] is given, its [bin/]
      subdirectory is prepended to [PATH] ahead of [prefix/bin]. The tools
      [lib/] is intentionally NOT wired into OCAMLLIB / OCAMLPATH /
      OCAMLFIND_DESTDIR — dev tools stay visible as binaries on PATH but
      invisible to the main project's compiler. *)

  val make_env :
    ?toolchain:Ctx.toolchain ->
    prefix:string ->
    ?tools:string ->
    dune_cache_root:string ->
    unit ->
    string array
  (** Like {!envrc_content} but returns an environment array suitable for
      [Eio.Process.spawn]. *)
end

(** {1 Persistent solve memo}

    Memoises {!solve} by digesting [conf], every [packages_dir] paired with its
    containing repository's [HEAD] commit, the constraints, and the target
    names. On a hit the stored result is loaded with [Marshal] instead of
    re-running 0install. Only successful solves are cached.

    A parallel "layer hashes" memo stores the topo-sorted list of d10 layer
    hashes a successful solve produced; a subsequent identical [oi run] can skip
    {!Ctx.create} / {!solve} / {!Plan.of_solution} entirely when every cached
    layer is still in the d10 cache. *)

module Memo : sig
  val key :
    ?test:OpamPackage.Name.Set.t ->
    ?doc:OpamPackage.Name.Set.t ->
    sys:D10.Sysops.t ->
    conf:Ctx.conf ->
    packages_dirs:string list ->
    constraints:OpamFormula.version_constraint OpamTypes.name_map ->
    names:OpamPackage.Name.t list ->
    ?toolchain:Ctx.toolchain ->
    unit ->
    string option
  (** MD5 hex digest used as the memo key, or [None] if any [packages_dir] is
      not under a git working tree (in which case the caller should skip both
      {!lookup} and {!store}). [git rev-parse HEAD] results are memoised
      process-wide. [sys] is needed to spawn [git rev-parse]/[git status] under
      the Eio fiber tree.

      [test] / [doc] enter the digest so a [+test/+doc] solve and a base solve
      don't collide in the memo — the closures differ. *)

  val lookup : cache_root:string -> key:string -> OpamPackage.t list option
  (** Read the memoised package list for [key], or [None] when the entry is
      absent / unreadable. *)

  val store :
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    cache_root:string ->
    key:string ->
    OpamPackage.t list ->
    unit
  (** Persist [pkgs] under [key]; the next matching {!lookup} returns it without
      re-running 0install. *)
end

(** {1 Solving} *)

val solve :
  ?test:OpamPackage.Name.Set.t ->
  ?doc:OpamPackage.Name.Set.t ->
  ?reporter:Build_progress.reporter ->
  sys:D10.Sysops.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cache_root:string ->
  Ctx.t ->
  packages_dirs:string list ->
  constraints:OpamFormula.version_constraint OpamTypes.name_map ->
  OpamPackage.Name.t list ->
  (OpamPackage.t list, string) result
(** [solve] Resolve the dependency closure for [names]. Returns packages in
    topological order. Successful solves are persisted via {!Memo} and re-used
    when an identical input is presented again.

    [test] / [doc] enable [{with-test}] / [{with-doc}] dependency filters for
    the named packages — typically the solve roots, so
    [opam install --with-test foo] semantics. Empty by default. Both feed into
    the memo key, so [+test/+doc] and base solves coexist without colliding.

    The compiler pin always comes from the toolchain set on {!Ctx.t} (consumer
    solves go through {!Pipeline.pick_toolchain}, which returns the
    [x-oi-default-toolchain] entry when no [--toolchain] is given). Solves
    against a [Ctx] without a toolchain raise — the no-toolchain branch was
    removed when the default-toolchain wiring went in. *)

val raw_solve :
  ?test:OpamPackage.Name.Set.t ->
  ?doc:OpamPackage.Name.Set.t ->
  env:(string -> OpamVariable.variable_contents option) ->
  packages_dirs:string list ->
  constraints:OpamFormula.version_constraint OpamTypes.name_map ->
  OpamPackage.Name.t list ->
  (OpamPackage.t list, string) result
(** [raw_solve] Lower-level entrypoint. Runs [opam-0install] over
    [packages_dirs] with exactly the [constraints] and [env] supplied — no
    auto-pinning, no {!Ctx}, no memo. *)

val direct_deps_within :
  packages_dirs:string list ->
  conf:Ctx.conf ->
  OpamPackage.t ->
  OpamPackage.Name.Set.t ->
  OpamPackage.Name.Set.t
(** [direct_deps_within ~packages_dirs ~conf pkg in_solution] is the set of
    direct dep names of [pkg] that appear in [in_solution], filtered by the
    platform variables in [conf]. *)

val find_opam_file : string list -> OpamPackage.t -> OpamFile.OPAM.t option
(** [find_opam_file packages_dirs pkg] searches [packages_dirs] in order for the
    opam file of [pkg]. *)

val filter_env : Ctx.conf -> OpamFilter.env
(** Filter environment built from a synthetic platform configuration. *)

val topo_sort :
  packages_dirs:string list ->
  conf:Ctx.conf ->
  OpamPackage.t list ->
  OpamPackage.t list
(** [topo_sort] Re-order [pkgs] in dependency-first topological order under
    [conf]'s filter env. Stable on already-sorted inputs. *)
