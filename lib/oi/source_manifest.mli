(** Sidecar JSON for a d10ir source archive.

    Written next to every [<sha>.tar.zst] under [<cache>/d10ir/archives/].
    Records the precise ingredients that fed the bake: primary source URL (with
    the resolved git commit for git pins), [extra-sources:], [extra-files:]
    (with per-file sha256 so a server can verify them against the reporepo),
    [patches:], and [substs:]. The archive is content-addressed by sha256, so
    the sidecar is content-equivalent across any two packages whose bake inputs
    collapse to identical bytes. *)

type source = {
  kind : string;
  url : string option;
  ref_ : string option;
  commit_sha : string option;
  checksums : string list;
}

type extra_source = { name : string; url : string; checksums : string list }
type extra_file = { path : string; sha256 : string; size : int }
type patch = { file : string; filter : string option }

type t = {
  schema : int;
  kind : string;
  sha256 : string;
  date : string;
  package_name : string;
  package_version : string;
  overlay_handle : string option;
  overlay_version : string option;
  source : source;
  extra_sources : extra_source list;
  extra_files : extra_file list;
  patches : patch list;
  substs : string list;
  strip_components : int;
  size : int option;
}

val codec : t Jsont.t
(** [codec] (de)serialises {!t} as JSON. *)

val v :
  proc_mgr:_ Eio.Process.mgr ->
  build_dir:string ->
  name:string ->
  version:string ->
  ?overlay_handle:string ->
  ?overlay_version:string ->
  sha256:string ->
  ?size:int ->
  ?strip_components:int ->
  source:Plan.source_info option ->
  extra_sources:(string * Plan.source_info) list ->
  extra_files:(string * string) list ->
  patches:Plan.patch list ->
  substs:string list ->
  unit ->
  t
(** [v ~proc_mgr ~build_dir ~name ~version ?overlay_handle ?overlay_version
     ~sha256 ?size ?strip_components ~source ~extra_sources ~extra_files
     ~patches ~substs ()] constructs a source manifest from the bake inputs.
    Resolves the upstream git commit (via [git rev-parse HEAD] in [build_dir])
    for git-pinned sources and computes a sha256 + size for each [extra-files:]
    entry. *)

val path_for : archives_dir:string -> sha:string -> string
(** [path_for] is [<archives_dir>/<sha>.json]. *)

val write : fs:Eio.Fs.dir_ty Eio.Path.t -> archives_dir:string -> t -> unit
(** [write] persists [m] under [archives_dir]; no-op if the on-disk file is
    already present. *)

val try_read : path:string -> t option
(** [try_read ~path] decodes the sidecar at [path]; [None] on missing file or
    decode error. *)
