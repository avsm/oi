(** Content-addressed binary package layers.

    Each layer is a directory containing a filesystem snapshot ([fs/]) and build
    metadata ([layer.json]). The directory name is the layer's hash, computed
    from the opam [effective_part] of the package and all its direct
    dependencies.

    {2 On-disk structure}

    {v
    <cache>/layers/<os_key>/<hash>/
      layer.json     # {!meta} record as JSON
      fs/            # filesystem snapshot (hardlinked from build prefix)
        bin/
        lib/
        share/
        ...
    v}

    The [fs/] tree is a subset of the build prefix -- only files new or modified
    by this package's install step are captured (via {!Prefix.diff}). Both
    regular files and symlinks are preserved. *)

(** {1 Hash computation} *)

val hash : packages_dirs:string list -> OpamPackage.t list -> string
(** [hash ~packages_dirs pkgs] computes the layer hash for a set of packages.
    The hash is the MD5 of the concatenated per-package opam [effective_part]
    hashes (SHA-512). Callers should pass the package and its full transitive
    dependency closure so that any change in the dependency tree invalidates the
    cache. *)

(** {1 Layer metadata}

    Stored as [layer.json] inside each layer directory. *)

type meta = {
  package : string;  (** Package name.version (e.g. ["dune.3.22.1"]). *)
  exit_status : int;  (** Build exit status (0 = success). *)
  deps : string list;  (** Direct dependency name.versions. *)
  hashes : string list;  (** Layer hashes of direct dependencies. *)
  created : float;  (** Unix timestamp of layer creation. *)
  overlay_handle : string option;
      (** Reporepo overlay handle that contributed this package's opam
          file, or [None] for legacy builds and pin-depends-only
          packages. *)
  overlay_version : string option;
      (** Reporepo overlay version (e.g. ["20260418.6"]) when
          [overlay_handle] is set. Pins the layer to a specific
          reporepo snapshot. *)
}

val save_meta : _ Eio.Path.t -> meta -> unit
(** [save_meta path meta] writes [meta] as JSON to [path], creating parent
    directories as needed. *)

val load_meta : _ Eio.Path.t -> meta option
(** [load_meta path] reads and parses [layer.json] from [path]. Returns [None]
    if the file does not exist or cannot be parsed. *)

(** {1 Paths and queries} *)

val dir : Config.t -> hash:string -> Eio.Fs.dir_ty Eio.Path.t
(** [dir c ~hash] is [<root>/layers/<os_key>/<hash>]. *)

val json_path : Config.t -> hash:string -> Eio.Fs.dir_ty Eio.Path.t
(** [json_path c ~hash] is [<root>/layers/<os_key>/<hash>/layer.json]. *)

val fs_path : Config.t -> hash:string -> Eio.Fs.dir_ty Eio.Path.t
(** [fs_path c ~hash] is [<root>/layers/<os_key>/<hash>/fs]. *)

val exists : Config.t -> hash:string -> bool
(** [exists c ~hash] is [true] if [layer.json] exists for this hash. *)

val succeeded : Config.t -> hash:string -> bool
(** [succeeded c ~hash] is [true] if the layer exists and has [exit_status = 0].
    Used for cache hit detection. *)

(** {1 Storage and retrieval} *)

val store :
  Config.t ->
  hash:string ->
  prefix:string ->
  files:string list ->
  package:string ->
  deps:string list ->
  parent_hashes:string list ->
  exit_status:int ->
  ?overlay_handle:string ->
  ?overlay_version:string ->
  unit ->
  unit
(** [store c ~hash ~prefix ~files ~package ~deps ~parent_hashes ~exit_status]
    creates a layer at [<root>/layers/<os_key>/<hash>/]. Each file in [files]
    (relative paths within [prefix]) is hardlinked into [fs/]. Symlinks are
    preserved by recreating them with the same target. Writes [layer.json] with
    the provided metadata. [overlay_handle] / [overlay_version] are persisted
    so later indexing knows which reporepo entry contributed this package. *)

val restore : Config.t -> hash:string -> prefix:string -> unit
(** [restore c ~hash ~prefix] hardlinks the layer's [fs/] tree into [prefix] via
    {!Sysops.link_tree}. No-op if the layer has no [fs/] directory (e.g. virtual
    packages that install no files). *)

(** {1 Remote registry} *)

type remote = [ `Http_remote of string ]
(** A remote layer source. [`Http_remote url] fetches layers as
    [<url>/<os_key>/<hash>.tar.zst]. *)

(** {2 Index}

    Each os_key directory in the registry contains an [OINDEX.txt] file listing
    all available layers with their SHA-256 checksums and sizes:
    {v <sha256>  <hash>.tar.zst  <size_bytes> v}
    The format is compatible with [sha256sum(1)] (the size field is ignored by
    that tool). Clients fetch the index once per command and use it to determine
    which layers are available remotely, avoiding per-layer HTTP probes. *)

type index_entry = { sha256 : string; size : int64 }
type remote_index = (string, index_entry) Hashtbl.t

val fetch_remote_index : Config.t -> remote:remote -> remote_index
(** [fetch_remote_index c ~remote] downloads [OINDEX.txt] from [remote] for the
    current [os_key] and returns a map from layer hash to its SHA-256 checksum
    and size. Returns an empty table on failure. *)

val pull_remote :
  Config.t -> remote:remote -> hash:string -> ?sha256:string -> unit -> bool
(** [pull_remote c ~remote ?sha256 ~hash] downloads layer [hash] from [remote],
    optionally verifying the SHA-256 checksum of the downloaded archive. Returns
    [true] if the layer is now available with [exit_status = 0]. No-op (returns
    [true]) if the layer already exists locally. *)

(** {1 Export} *)

val export : Config.t -> hash:string -> dst:_ Eio.Path.t -> bool
(** [export c ~hash ~dst] creates [<dst>/<os_key>/<hash>.tar.zst] from local
    layer [hash]. Returns [true] if a new archive was created. Returns [false]
    if the layer doesn't exist locally or the archive already exists. *)

val export_all : Config.t -> dst:_ Eio.Path.t -> int
(** [export_all c ~dst] exports all succeeded layers for all os_keys to [dst].
    Writes [OINDEX.txt] for each os_key after exporting. Returns the number of
    newly exported layers. *)
