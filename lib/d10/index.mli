(** Layer index (SQLite).

    Provides fast lookup of cached layers by package name, binary name, or
    dependency chain. The index is a SQLite database stored at
    [<cache>/layers/index.db] and is rebuilt on demand by scanning the layer
    directories.

    {2 Schema}

    {v
    layers:          hash, os_key, arch, os, distro, os_version,
                     package_name, package_ver, exit_status, created,
                     overlay_handle, overlay_version
    layer_deps:      layer_hash, dep_name, dep_version, dep_hash
    layer_files:     layer_hash, path
    layer_binaries:  layer_hash, binary_name   (files under bin/)
    v}

    [overlay_handle] / [overlay_version] identify the reporepo overlay that
    contributed the opam file used to build a layer. Both are NULL for layers
    built before tagging was introduced or for packages that came from a
    pin-depends tree.

    The [layer_binaries] table enables [oi run <binary>] to quickly find which
    package provides a given binary without scanning layer trees. *)

(** {1 Database lifecycle} *)

type db
(** An open SQLite database handle. *)

val open_ : path:string -> db
(** [open_ ~path] opens (or creates) the index database at [path]. Tables are
    created if they don't already exist. *)

val close : db -> unit
(** [close db] closes the database handle. *)

(** {1 Indexing} *)

val rebuild : Config.t -> db -> unit
(** [rebuild c db] scans all layers under [<root>/layers/<os_key>/] and
    populates the index tables. Existing data for [c.os_key] is replaced
    atomically within a transaction. Each layer's [layer.json] is parsed for
    metadata, and its [fs/] tree is scanned for file paths and binary names. *)

(** {1 Queries} *)

val find_layer :
  db -> name:string -> version:string -> os_key:string -> (string * int) option
(** [find_layer db ~name ~version ~os_key] returns [(hash, exit_status)] for the
    layer matching the given package, or [None]. *)

val find_binary :
  db -> binary:string -> os_key:string -> (string * string * string) list
(** [find_binary db ~binary ~os_key] returns all layers that provide
    [bin/<binary>] or [sbin/<binary>], as
    [(package_name, package_version, layer_hash)], sorted by opam version
    descending (latest version first). *)

val search_binary :
  db ->
  pattern:string ->
  os_key:string ->
  (string * string * string * string * (string * string) option) list
(** [search_binary db ~pattern ~os_key] searches for binaries matching
    [pattern], returning
    [(binary_name, package_name, package_version, layer_hash, overlay)]. The
    [overlay] field is [Some (handle, version)] when the layer was tagged with a
    reporepo overlay, and [None] otherwise (pin-depends, pre-tagging layers).
    The pattern is matched exactly by default; use [*] as a wildcard (mapped to
    SQL [LIKE %]). Results are sorted by binary name then opam version
    descending. *)

val deps : db -> hash:string -> (string * string * string) list
(** [deps db ~hash] returns the direct dependencies of a layer as
    [(dep_name, dep_version, dep_hash)]. *)

val files : db -> hash:string -> string list
(** [files db ~hash] returns all file paths stored in the layer. *)

val all_layers : db -> os_key:string -> (string * string * string * int) list
(** [all_layers db ~os_key] returns all layers for a platform as
    [(hash, package_name, package_version, exit_status)]. *)

val all_binaries : db -> os_key:string -> (string * string * string) list
(** [all_binaries db ~os_key] returns all indexed binaries as
    [(binary_name, package_name, layer_hash)]. *)

val stats : db -> os_key:string -> int * int * int
(** [stats db ~os_key] returns [(num_layers, num_binaries, num_files)] for the
    given platform. *)

(** {1 Remote merge} *)

val merge_remote : db -> remote_path:string -> unit
(** [merge_remote db ~remote_path] imports layers from a remote index database
    into [db]. Only layers whose hash does not already exist in [db] are
    inserted (local entries take precedence). Associated deps, binaries, and
    files for new layers are also imported. *)
