(** Per-layer JSON manifest: success/failure record describing one attempted
    layer build.

    Replaces the older split between a per-layer-dir [layer.json],
    [provenance.json], and [recipe.json]: every field of those landed here. The
    on-disk path is [<cache>/layers/<os_key>/<hash>.json] (and the same relative
    shape under any export / S3 prefix). *)

type outcome = Ok | Fail | Timeout

val string_of_outcome : outcome -> string
(** [string_of_outcome o] renders [o] as the lowercase tag used on the wire
    (["ok"], ["fail"], ["timeout"]). *)

type tarball = { sha256 : string; size : int; key : string option }

type file_entry = {
  path : string;
  kind : string;
  size : int option;
  mode : string option;
  target : string option;
}

type dep = { name : string; version : string; hash : string }

type findlib_entry = {
  package_dir : string;
  findlib_pkg : string;
  archive : string option;
}

type source_archive = { sha256 : string; key : string option }

type recipe = {
  script : string;
  env : string list;
  substs : string list;
  subst_vars : (string * string) list;
}

type log_tail = { lines : int; truncated : bool; text : string }

type failure_info = {
  phase : string;
  exit_status : int option;
  duration_s : float;
  log : log_tail option;
}

type t = {
  schema : int;
  kind : string;
  outcome : outcome;
  hash : string;
  os_key : string;
  date : string;
  package : string;
  package_name : string;
  package_ver : string;
  method_ : string;
  overlay_handle : string option;
  overlay_version : string option;
  tarball : tarball option;
  files : file_entry list;
  binaries : string list;
  findlib : findlib_entry list;
  exit_status : int option;
  provenance : Provenance.t option;
  recipe : recipe option;
  failure : failure_info option;
  invocation_id : string option;
  source_archive : source_archive option;
  deps : dep list;
  depexts_declared : string list;
  build_env_ocaml_version : string option;
}

val codec : t Jsont.t
(** JSON codec for {!t}. *)

val dep_codec : dep Jsont.t
(** [dep_codec] exposed because [Manifest_build] reuses it for its
    [solve.resolved] list. *)

val files_of_fs_dir : string -> file_entry list
(** [files_of_fs_dir dir] enumerates [dir] recursively and returns a
    [file_entry] per regular file, symlink, or sub-directory found. *)

val success :
  hash:string ->
  os_key:string ->
  package:string ->
  package_name:string ->
  package_ver:string ->
  method_:string ->
  ?overlay_handle:string ->
  ?overlay_version:string ->
  tarball:tarball ->
  files:file_entry list ->
  binaries:string list ->
  findlib:findlib_entry list ->
  exit_status:int ->
  ?provenance:Provenance.t ->
  ?recipe:recipe ->
  ?source_archive:source_archive ->
  deps:dep list ->
  depexts_declared:string list ->
  ?build_env_ocaml_version:string ->
  unit ->
  t
(** Build a success manifest. *)

val path_for : cache_root:string -> os_key:string -> hash:string -> string
(** [path_for ~cache_root ~os_key ~hash] is the on-disk path of the JSON
    manifest for [hash] under [cache_root]'s [os_key/] directory. *)

val write : fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> t -> unit
(** [write ~fs ~cache_root m] writes [m] to
    [path_for ~cache_root ~os_key:m.os_key ~hash:m.hash], atomically and
    idempotently (no-op if the on-disk content already matches). *)

val try_read : path:string -> t option
(** [try_read ~path] decodes the manifest at [path]; [None] on missing file or
    decode error (with a debug log). *)
