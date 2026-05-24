(** Content-addressed cache of opam source archives keyed by checksum.

    The mirror lives at [<cache_root>/mirror/<algo>/<XX>/<hash>] and is the
    file:// [cache_url] every opam fetch consults first, before falling back to
    the registry or the upstream URL. It exists for two reasons:

    - [oi build --archives-only] populates it ahead of build time so a later
      offline build never hits the network.
    - Successful registry pulls store the fetched blob here so the next solver
      run that asks for the same source skips opam's download cache entirely.

    Re-exported as [Source.Mirror] for callers that still spell the old
    [Oi.Source.Mirror.X] form. *)

val dir : cache:Cache.t -> string
(** [dir ~cache] is the absolute mirror-root path: [{cache_root}/mirror]. *)

val url : cache:Cache.t -> OpamUrl.t
(** [url ~cache] is a [file://] URL pointing at {!dir}, suitable for use as a
    [cache_url] in [OpamRepository.pull_*]. *)

val remote_url : registry:string -> OpamUrl.t
(** [remote_url ~registry] is the registry's mirror endpoint as an [OpamUrl.t].
*)

type stats = { count : int; total_size : int64 }

val stats : cache:Cache.t -> stats
(** Walk the mirror directory and report blob count + total size. *)

val export : cache:Cache.t -> dst:Eio.Fs.dir_ty Eio.Path.t -> int
(** [export ~cache ~dst] hardlink-copies the mirror tree to [<dst>/sources/].
    Returns the number of blobs copied. *)

val import_from_opam_cache :
  fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> OpamHash.t list -> int
(** [import_from_opam_cache ~fs ~cache_root checksums] copies fetched-source
    blobs out of opam's download-cache and into the mirror at
    [<cache_root>/mirror/<algo>/<XX>/<hash>] for each declared checksum. Returns
    the number of blobs newly added; [0] if nothing was imported (no checksums
    supplied, or no cached file found). Idempotent. *)

type archive = { url : OpamUrl.t; checksums : OpamHash.t list; pkg : string }
(** One downloadable source entity: either an [url {…}] block or an
    [extra-source] entry. [pkg] is the [name.version] label, only used for
    progress and failure messages. *)

val collect_archives :
  packages_dirs:string list -> OpamPackage.t list -> archive list
(** [collect_archives ~packages_dirs pkgs] resolves each [pkg]'s opam file from
    the first matching [packages_dirs] entry, then extracts its archives.
    Deduped by URL so packages sharing a mirror tarball contribute one fetch.
    Drives [oi build --archives-only] against the solver's resolved set. *)

val archives_of_opam_file : path:string -> pkg:string -> archive list
(** [archives_of_opam_file ~path ~pkg] parses the opam file at [path] directly,
    labelling each archive with [pkg]. Returns [[]] for unreadable or sourceless
    files. Drives [oi build --archives-only --every-version], which walks the
    reporepo's filesystem rather than the solver. *)

val dedup_by_url : archive list -> archive list
(** [dedup_by_url xs] is a first-occurrence dedup of [xs] keyed on the URL
    string. Use after concatenating per-group results (e.g.
    [oi build --archives-only --all]) where the same archive is referenced
    across overlapping solves. *)

type fetch_summary = {
  fetched : int;
  cached : int;
  failed : (string * string) list;  (** [(url, error_message)]. *)
  bytes_added : int64;
}

val fetch_archives :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cache:Cache.t ->
  ?on_progress:(fetched:int -> total:int -> current:string option -> unit) ->
  archive list ->
  fetch_summary
(** [fetch_archives ~fs ~cache ?on_progress xs] fetches each archive in [xs] and
    deposits it into the mirror. Skips entries whose first declared checksum is
    already present (the [cached] tally). On a hard failure (after retries),
    records the URL + message in [failed] and moves on — no exception is raised.
    [on_progress] receives [current=Some label] just before each fetch and
    [current=None] after the last; [label] is the host + basename of the URL,
    suitable for an in-place progress line. *)

type origin =
  | Local_mirror of string
      (** Resolved file path on disk under a [file://] cache_url. *)
  | Other
      (** Either the remote registry or the upstream URL — opam's [pull_tree]
          will resolve the actual source via [cache_urls] without our
          involvement. Also returned when the source has no checksums (e.g.
          [git+...] URLs, which can't live in a content-addressed mirror). *)

val source_origin :
  cache_urls:OpamUrl.t list -> checksums:OpamHash.t list -> origin
(** [source_origin ~cache_urls ~checksums] probes each [file://] cache_url for
    any of [checksums] via {!Sys.file_exists}, returning [Local_mirror path] on
    the first hit and [Other] otherwise. Used by {!Execute} to log whether a
    fetch hits the local mirror — HTTP cache_urls are intentionally not probed
    (a per-package HEAD round-trip just to refine the log line isn't worth it;
    opam's [pull_tree] handles the cache hierarchy itself). *)
