[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Source tarball mirror in opam download-cache format.

    Populated automatically during [oi registry build] as a side-effect
    of each successful source fetch; exported to the registry's top-level
    [sources/] directory by [oi registry export]. The on-disk layout is
    exactly what opam expects for a [cache_url]:

      <mirror>/<algo>/<first-2-chars-of-hash>/<full-hash>

    where [<algo>] is [md5], [sha256], or [sha512]. When opam declares
    multiple checksum algorithms for a single source, each path is a
    hardlink to the same inode (opam's convention).

    A sibling sqlite database [<mirror>/index.db] records metadata (size,
    package references, declared checksums) for fast queries — neither
    the filesystem alone nor opam knows which package a blob belongs to. *)

(** {1 Paths} *)

val dir : cache:Cache.t -> string
(** Absolute path to the mirror root: [{cache_root}/mirror]. *)

val url : cache:Cache.t -> OpamUrl.t
(** [file://<dir>], suitable for use as a [cache_url] in
    [OpamRepository.pull_*]. *)

val remote_url : registry:string -> OpamUrl.t
(** [<registry>/sources] as an [OpamUrl.t], suitable for use as a
    [cache_url] when a remote registry is configured. *)

(** {1 Write path} *)

val record :
  sys:D10.Sysops.t ->
  cache:Cache.t ->
  package:OpamPackage.t ->
  kind:[ `Main | `Extra of string ] ->
  url:OpamUrl.t ->
  checksums:OpamHash.t list ->
  unit
(** Promote a just-fetched source from opam's own download-cache into
    the mirror, and record metadata rows.

    Idempotent. If the file isn't present in opam's cache (e.g. it was
    a git pin with no url: section), silently does nothing. *)

(** {1 Queries} *)

type stats = { count : int; total_size : int64 }

val stats : cache:Cache.t -> stats
(** Count and total size of mirrored blobs. *)

val gc : cache:Cache.t -> int
(** Remove blobs whose [source_refs] is empty (orphaned from any
    package we've ever built). Returns the number of blobs removed. *)

val verify : sys:D10.Sysops.t -> cache:Cache.t -> (string * string) list
(** Re-hash every blob and compare against its declared checksums.
    Returns a list of [(hash, error)] for blobs that fail. *)

(** {1 Export} *)

val export : cache:Cache.t -> dst:Eio.Fs.dir_ty Eio.Path.t -> int
(** Copy/hardlink the mirror into [dst/sources/] for publication as
    part of a registry export. Returns the number of blobs exported. *)
