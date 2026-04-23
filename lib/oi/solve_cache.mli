(** Persistent memoisation of {!Solve.solve} results.

    The cache is keyed on an MD5 digest of everything the solver
    reads: the platform [conf], each [packages_dir] paired with its
    containing git repository's [HEAD] commit, the version
    constraints, and the target names. On a hit the stored
    [OpamPackage.t list] is loaded from disk with [Marshal] instead of
    re-running 0install.

    Only successful solves are cached.

    Every [packages_dir] passed in must sit inside a git working
    tree — oi's opam repositories always come from git remotes, and
    [git rev-parse HEAD] is fast enough to call on every solve. If
    any directory isn't git-backed (e.g. a synthesised pin-set tree)
    {!key} returns [None] and the solve runs uncached. *)

val key :
  conf:Opam_ctx.conf ->
  packages_dirs:string list ->
  constraints:OpamFormula.version_constraint OpamTypes.name_map ->
  names:OpamPackage.Name.t list ->
  string option
(** [key ~conf ~packages_dirs ~constraints ~names] is the MD5 hex
    digest used as the cache key, or [None] if any [packages_dir] is
    not under a git working tree (in which case the caller should
    skip both {!lookup} and {!store}).

    [git rev-parse HEAD] results are memoised process-wide by
    directory path, so a run that solves many groups over the same
    set of overlays invokes git once per directory. *)

val lookup :
  cache_root:string -> key:string -> OpamPackage.t list option
(** [lookup ~cache_root ~key] returns the cached solution, or [None]
    on miss, Marshal error, or any I/O failure. A corrupt file is
    unlinked so subsequent lookups fall through cleanly. *)

val store :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cache_root:string ->
  key:string ->
  OpamPackage.t list ->
  unit
(** [store ~fs ~cache_root ~key pkgs] Marshals [pkgs] to a temp file
    and atomically renames it into place. Any I/O error is swallowed
    — the caller already has the correct result. *)

(** {1 Layer-hashes cache}

    Once a solve's output has been turned into a topo-sorted list of
    d10 layer hashes by {!Action.plan}, the hashes are persisted
    under a parallel key. A subsequent [oi run] with identical inputs
    can then skip {!Opam_ctx.create}, {!Solve.solve} and
    {!Action.plan} entirely — provided every stored layer hash is
    still present in the d10 cache (checked by the caller). *)

val lookup_layers :
  cache_root:string -> key:string -> string list option

val store_layers :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cache_root:string ->
  key:string ->
  string list ->
  unit
