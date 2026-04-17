[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Pin-depends realisation: fetch each pin's source, extract its
    [<pkgname>.opam], rewrite [url:] to point at the local clone (with
    a resolved revision baked in), and emit a synthetic opam [packages/]
    directory the solver can consume ahead of all other repositories.

    Synthesized layout follows the standard opam packages tree:

      <root>/packages/<name>/<name>.<version>/opam

    The rewritten [url:] block points at the local source tree with a
    checksum (tarballs) or commit sha (git). Layer-cache identity
    therefore incorporates the resolved revision, not the symbolic URL. *)

val materialize :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  cache:Cache.t ->
  ?refresh:bool ->
  Project.pin list ->
  string option
(** [materialize ~fs ~sys ~cache ?refresh pins] returns [Some packages_dir]
    (an absolute path to a synthesized [packages/] tree) when [pins] is
    non-empty, [None] when [pins = []].

    When [refresh] is [true] (default [false]), any cached pin source is
    discarded and re-fetched. Otherwise, a cached source is reused only
    while younger than {!Repo.refresh_max_age}; older caches are re-fetched
    so upstream commits on git pins become visible.

    Raises [Oi.Error.config_error] if a pin URL cannot be fetched, the
    expected [.opam] is missing from the fetched tree, or the file is
    malformed. *)
