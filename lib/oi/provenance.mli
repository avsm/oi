(** Per-layer content provenance — the immutable proof of what went into a
    successfully-built layer.

    Lives at [<cache>/layers/<os_key>/<hash>/provenance.json]. Written once by
    {!write} when the layer is committed and never overwritten. The file
    captures only content-addressed facts (opam origin, source URL+checksum, dep
    hashes) — caller context lives in the {!Audit} log instead, and the
    cache-internal [layer.json] sidecar (see {!D10.Layer.meta}) carries the
    fields the d10 cache itself needs to assemble prefixes.

    Together with the transitive closure of [deps[].hash] and the [opam.origin]
    pointer, a [provenance.json] is sufficient to refetch every input that
    contributed to the layer's hash. *)

type opam_info = {
  sha256 : string;
      (** Hex SHA-256 of the opam file's [effective_part] bytes — the same bytes
          [D10.Layer.hash] consumed when computing [layer_hash]. *)
  origin : Origin.t;
}

type source_info = {
  url : string;
  kind : string;  (** "git" | "tar" | "local" | "" — derived from [url]. *)
  checksums : string list;
}

type phases = {
  fetch : float option;
  build : float option;
  install : float option;
  restore : float option;
}

type build_env = { ocaml_version : string }

type t = {
  schema : int;  (** Always [1]. *)
  layer_hash : string;
  os_key : string;
  pkg : Identity.t;
  method_ : Identity.method_;
  built_at : float;  (** Unix seconds when this layer was committed. *)
  duration_s : float;
  phases : phases;
  opam : opam_info;
  source : source_info option;
  deps : Identity.dep list;
  depexts_declared : string list;
  build_env : build_env;
}

val pp : t Fmt.t
(** [pp ppf t] renders a one-line summary: layer hash, OS key, package, and
    build duration. *)

(** {1 Codecs}

    Leaf codecs are exposed so {!Manifest} can re-encode the same shapes inside
    its own envelope without restating the schema. *)

val codec : t Jsont.t
(** [codec] (de)serialises a {!t} provenance record as JSON. *)

val phases_codec : phases Jsont.t
(** [phases_codec] (de)serialises a {!phases} sub-record. *)

val opam_info_codec : opam_info Jsont.t
(** [opam_info_codec] (de)serialises the {!opam_info} sub-record. *)

val source_info_codec : source_info Jsont.t
(** (de)serialises the {!source_info} sub-record. *)

val build_env_codec : build_env Jsont.t
(** (de)serialises the {!build_env} sub-record. *)

(** {1 Storage} *)

val path : cache_root:string -> os_key:string -> hash:string -> string
(** [<cache>/layers/<os_key>/<hash>/provenance.json] — sidecar location for a
    single layer's provenance record. *)

val write : fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> t -> unit
(** Encode [r] and write to {!path}. The layer dir must already exist (committed
    by [D10.Layer.store]); silently no-ops if it doesn't. Errors are logged and
    swallowed: a logging failure must never abort the build. *)

val read_all :
  fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> os_key:string -> t list
(** Walk every [<cache>/layers/<os_key>/<hash>/provenance.json] and decode.
    Layer dirs without a [provenance.json] are silently skipped. Files that fail
    to decode are logged at debug and skipped. *)

val read_one :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cache_root:string ->
  os_key:string ->
  hash:string ->
  t option
(** [read_one ~fs ~cache_root ~os_key ~hash] reads a single [provenance.json] by
    layer hash. [None] when the file is missing or fails to decode. Pairs with
    {!read_all}. *)

val overlay_of_layer :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cache_root:string ->
  os_key:string ->
  hash:string ->
  D10.Overlay.t option
(** [overlay_of_layer] is a convenience that projects [load …] down to
    [opam.origin.overlay]. Used to feed [D10.Index.rebuild]'s [?overlay_for]
    callback so the d10 layer index can carry overlay attribution without
    [layer.json] carrying it. *)

(** {1 Helpers for producers} *)

val hash_opam_file : path:string -> string
(** Hex SHA-256 of [path]'s [effective_part]-equivalent bytes. Used by {!Plan}
    to populate {!opam_info.sha256}. Returns [""] if the file can't be read. *)
