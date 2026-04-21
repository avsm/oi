(** Opam repository management.

    Wraps opam's {!OpamRepository} backend for cloning and refreshing individual
    opam-repository remotes. Higher-level logic (the set of "base" repositories,
    project extras, and overlay resolution) lives in {!Reporepo} and the CLI
    surface. *)

val refresh_max_age : float
(** Seconds. Default [86_400.0] (24h). A repo clone older than this is pulled
    again on next use. *)

val cache_fresh : refresh:bool -> sentinel:string -> max_age:float -> bool
(** [cache_fresh ~refresh ~sentinel ~max_age] is [true] when a cache entry
    guarded by [sentinel] (a marker file whose presence proves the entry was
    populated cleanly) is still fresh: the sentinel exists and is younger than
    [max_age] seconds. Returns [false] unconditionally when [refresh] is [true].
    Shared by {!Pin} and {!Url_project} so their sentinel-based TTL policies
    stay in lockstep. *)

val repo_dir : data_dir:string -> string -> string
(** [repo_dir ~data_dir name] is the local clone path for a repo named [name].
*)

val ensure_extra :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  data_dir:string ->
  ?refresh:bool ->
  Project.extra_repo list ->
  string list
(** [ensure_extra ~fs ~data_dir extras] clones/updates each entry using the same
    age/force semantics as {!ensure_one}. Each entry is cloned into
    [data_dir/repos/<name>] — two entries with the same name collide by design
    (callers should deduplicate). Returns one [packages/] directory per entry in
    input order. *)

val ensure_one :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  refresh:bool ->
  label:string ->
  url:string ->
  dir:string ->
  unit
(** Low-level clone/update primitive shared with the reporepo materialiser.
    Clones [url] into [dir] if empty, otherwise refreshes when [refresh] is true
    or the clone is older than {!refresh_max_age}. *)
