(** Materialise [pin-depends] declarations into a synthesised [packages/] tree
    the solver can consume.

    A [pin] gives the solver a verbatim path to a package's opam file at a
    user-chosen version/URL, side-stepping whatever the configured opam
    repositories ship. Materialisation clones each pin's source if needed and
    produces a [packages/<name>/<name.version>/opam] layout under the cache root
    that takes priority over every other repo in the solver's search path.

    {!resolve_pins} runs first, normalising URLs to a sha-pinned form (recorded
    in [_oi/oi.lock]) so the cache key is deterministic; {!materialize} then
    fetches and stages each pin. Re-exported as [Source.Pin]. *)

val materialize :
  ?reporter:Build_progress.reporter ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  cache:Cache.t ->
  ?refresh:bool ->
  Project.pin list ->
  string option
(** [materialize ?reporter ~fs ~sys ~cache ?refresh pins] returns
    [Some packages_dir] (an absolute path to a synthesized [packages/] tree)
    when [pins] is non-empty, [None] when [pins = []].

    [?reporter] receives a [Status "Materialising N pin(s)"] event followed by
    one [Status "Fetching pin <pkg>"] per pin and an
    [Aggregate { phase = Fetching; total; current }] tick after each pin is
    materialised. *)

val resolve_pins :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  ?project_root:string ->
  Project.pin list ->
  Project.pin list
(** [resolve_pins ~fs ~sys ?project_root pins] pins the URL of every entry in
    [pins] to a deterministic form (sha-pinned git URL or unchanged tarball),
    consulting/refreshing [<project_root>/_oi/oi.lock] when supplied. With no
    [project_root], or with an empty [pins], the input is returned unchanged.
    The lock file is transient state under [_oi/] — never committed; deleting it
    forces a re-resolution on the next [oi build].

    Errors when a pin's URL cannot be resolved (network down, ref gone). *)
