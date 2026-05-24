(** Produce a single tarball per package_plan that the d10ir Direct executor can
    unpack and build from.

    Pipeline:
    + {!Execute.fetch_phase}: primary source + extra sources → [p.build_dir].
    + {!Execute.copy_extra_files}: opam [extra-files:] → [p.build_dir].
    + {!Execute.apply_patches}: [patch -p1] for each patch.
    + {!Execute.apply_substs}: opam [%{var}%] substitutions written into the
      source tree (using the planning-prefix sentinel for [%{prefix}%] and
      friends).
    + tar [p.build_dir] into a deterministic archive, sha256 it, move to the
      cache.

    Archives are stored in [<cache_root>/d10ir/archives/<sha256>.tar]. The same
    archive is reused across packages whose contents hash to the same sha256. *)

type built = {
  path : string;  (** Absolute path to the archive file. *)
  sha256 : string;  (** sha256 of the archive contents. *)
  strip_components : int;  (** 0 — we tar from inside the source root. *)
  subst_files : string list;
      (** [p.substs] echoed back so the d10ir node can declare which files
          contain the prefix sentinel. *)
}

val build :
  ?reporter:Build_progress.reporter ->
  proc_mgr:_ Eio.Process.mgr ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  d10:D10.Config.t ->
  cache_root:string ->
  ?cache_urls:OpamUrl.t list ->
  Plan.package_plan ->
  built
(** [build ?reporter ~proc_mgr ~fs ~d10 ~cache_root ?cache_urls p] runs the full
    source-prep pipeline for [p] and produces an archive. Used by
    {!Recipe_emitter.emit} at recipe-emit time when the solver context is
    available. [?reporter] receives a [Status "Baking archive for <pkg>"] event
    when work begins. *)

val build_no_solve :
  ?reporter:Build_progress.reporter ->
  proc_mgr:_ Eio.Process.mgr ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  d10:D10.Config.t ->
  cache_root:string ->
  ?cache_urls:OpamUrl.t list ->
  platform:Osrel.t ->
  name:string ->
  version:string ->
  pkg_dir:string ->
  opam_path:string ->
  unit ->
  built
(** [build_no_solve ?reporter ~proc_mgr ~fs ~d10 ~cache_root ?cache_urls
     ~platform ~name ~version ~pkg_dir ~opam_path ()] is the solver-free variant
    of {!build}. Reads the opam file at [opam_path], fetches the source, applies
    extra-files and filter-evaluated patches, runs opam substitutions against a
    deterministic synthetic env (rooted at the cache_root's build prefix), and
    tars the result into a content-addressed archive.

    Used by [oi repo bump]: every package in the freshly materialised overlay
    gets baked without going through the solver, so URL pinning and archive
    mirroring complete in one pass regardless of whether the overlay's
    transitive deps are currently solvable. *)
