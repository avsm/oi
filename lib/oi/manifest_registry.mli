(** Per-[os_key] schema-version pointer at
    [<cache>/layers/<os_key>/registry.json].

    A tiny, write-once-per-schema-bump pointer that downstream consumers
    (indexers, registry UIs) read first to learn which schema variants the
    sidecars beside it use. *)

type t = {
  schema : int;
  kind : string;
  os_key : string;
  layer_manifest_schema : int;
  build_manifest_schema : int;
  source_manifest_schema : int;
  wrote_by : string;
  date : string;
}

val path_for : cache_root:string -> os_key:string -> string
(** [path_for ~cache_root ~os_key] is the on-disk path of the pointer file. *)

val ensure :
  clock:_ Eio.Time.clock ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cache_root:string ->
  os_key:string ->
  wrote_by:string ->
  unit
(** [ensure ~clock ~fs ~cache_root ~os_key ~wrote_by] writes (or refreshes) the
    pointer under [cache_root]'s [os_key/registry.json]. Idempotent: skips when
    an existing file already matches the current schema values.

    Serialised across processes by a per-os_key lockfile so concurrent [oi]
    invocations don't race on rename. *)
