let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.provenance"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* -- Types --------------------------------------------------------------- *)

type opam_info = { sha256 : string; origin : Origin.t }
type source_info = { url : string; kind : string; checksums : string list }

type phases = {
  fetch : float option;
  build : float option;
  install : float option;
  restore : float option;
}

type build_env = { ocaml_version : string }

type t = {
  schema : int;
  layer_hash : string;
  os_key : string;
  pkg : Identity.t;
  method_ : Identity.method_;
  built_at : float;
  duration_s : float;
  phases : phases;
  opam : opam_info;
  source : source_info option;
  deps : Identity.dep list;
  depexts_declared : string list;
  build_env : build_env;
}

let empty_phases =
  { fetch = None; build = None; install = None; restore = None }

(* -- Opam content hash --------------------------------------------------- *)

let hash_opam_file ~path =
  if not (Sys.file_exists path) then ""
  else
    try
      OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path))
      |> OpamFile.OPAM.effective_part |> OpamFile.OPAM.write_to_string
      |> (fun s -> Digest.string s)
      |> Digest.to_hex
    with exn ->
      Log.debug (fun m ->
          m "hash_opam_file %s: %s" path (Printexc.to_string exn));
      ""

(* -- Codec --------------------------------------------------------------- *)

let opam_info_codec =
  let open Jsont in
  Object.map ~kind:"opam" (fun sha256 origin -> { sha256; origin })
  |> Object.mem "sha256" string ~enc:(fun o -> o.sha256)
  |> Object.mem "origin" Origin.codec ~enc:(fun o -> o.origin)
  |> Object.finish

let source_info_codec =
  let open Jsont in
  Object.map ~kind:"source" (fun url kind checksums -> { url; kind; checksums })
  |> Object.mem "url" string ~enc:(fun s -> s.url)
  |> Object.mem "kind" string ~enc:(fun s -> s.kind)
  |> Object.mem "checksums" (list string) ~dec_absent:[]
       ~enc:(fun s -> s.checksums)
       ~enc_omit:(( = ) [])
  |> Object.finish

let phases_codec =
  let open Jsont in
  Object.map ~kind:"phases" (fun fetch build install restore ->
      { fetch; build; install; restore })
  |> Object.opt_mem "fetch" number ~enc:(fun p -> p.fetch)
  |> Object.opt_mem "build" number ~enc:(fun p -> p.build)
  |> Object.opt_mem "install" number ~enc:(fun p -> p.install)
  |> Object.opt_mem "restore" number ~enc:(fun p -> p.restore)
  |> Object.finish

let build_env_codec =
  let open Jsont in
  Object.map ~kind:"build_env" (fun ocaml_version -> { ocaml_version })
  |> Object.mem "ocaml_version" string ~enc:(fun e -> e.ocaml_version)
  |> Object.finish

let of_fields schema layer_hash os_key pkg method_ built_at duration_s
    phases opam source deps depexts_declared build_env =
  {
    schema;
    layer_hash;
    os_key;
    pkg;
    method_;
    built_at;
    duration_s;
    phases;
    opam;
    source;
    deps;
    depexts_declared;
    build_env;
  }

let codec =
  let open Jsont in
  Object.map ~kind:"provenance" of_fields
  |> Object.mem "schema" int ~enc:(fun r -> r.schema)
  |> Object.mem "layer_hash" string ~enc:(fun r -> r.layer_hash)
  |> Object.mem "os_key" string ~enc:(fun r -> r.os_key)
  |> Object.mem "pkg" Identity.codec ~enc:(fun r -> r.pkg)
  |> Object.mem "method" Identity.method_codec ~enc:(fun r -> r.method_)
  |> Object.mem "built_at" number ~enc:(fun r -> r.built_at)
  |> Object.mem "duration_s" number ~enc:(fun r -> r.duration_s)
  |> Object.mem "phases" phases_codec ~dec_absent:empty_phases
       ~enc:(fun r -> r.phases)
       ~enc_omit:(fun p -> p = empty_phases)
  |> Object.mem "opam" opam_info_codec ~enc:(fun r -> r.opam)
  |> Object.opt_mem "source" source_info_codec ~enc:(fun r -> r.source)
  |> Object.mem "deps" (list Identity.dep_codec) ~dec_absent:[]
       ~enc:(fun r -> r.deps)
       ~enc_omit:(( = ) [])
  |> Object.mem "depexts_declared" (list string) ~dec_absent:[]
       ~enc:(fun r -> r.depexts_declared)
       ~enc_omit:(( = ) [])
  |> Object.mem "build_env" build_env_codec ~enc:(fun r -> r.build_env)
  |> Object.finish

let pp ppf t =
  Fmt.pf ppf "@[<h>provenance %s %s %s %.1fs@]" t.layer_hash t.os_key
    (Identity.to_string t.pkg) t.duration_s

(* -- Storage ------------------------------------------------------------- *)

let path ~cache_root ~os_key ~hash =
  cache_root / "layers" / os_key / hash / "provenance.json"

let save_encoded ~fs ~dst s =
  try Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / dst) s
  with exn ->
    Log.warn (fun m -> m "provenance write %s: %s" dst (Printexc.to_string exn))

let write ~fs ~cache_root r =
  let layer_dir = cache_root / "layers" / r.os_key / r.layer_hash in
  if Sys.file_exists layer_dir then
    let dst = path ~cache_root ~os_key:r.os_key ~hash:r.layer_hash in
    match Jsont_bytesrw.encode_string ~format:Jsont.Indent codec r with
    | Ok s -> save_encoded ~fs ~dst s
    | Error e -> Log.warn (fun m -> m "provenance encode %s: %s" dst e)

let try_decode ~fs ~path : t option =
  try
    let s = Eio.Path.load Eio.Path.(fs / path) in
    match Jsont_bytesrw.decode_string ~locs:true ~file:path codec s with
    | Ok r -> Some r
    | Error e ->
        Log.debug (fun m -> m "provenance bad %s: %s" path e);
        None
  with exn ->
    Log.debug (fun m ->
        m "provenance read %s: %s" path (Printexc.to_string exn));
    None

(* Path of the unified layer-manifest sidecar (the new home of the
   [provenance] sub-object). Kept in sync with
   [Manifest_layer.path_for]. We deliberately don't import
   [Manifest_layer] here to keep [Provenance] free of the dependency
   cycle that would create. *)
let sidecar_path ~cache_root ~os_key ~hash =
  cache_root / "layers" / os_key / (hash ^ ".json")

(* Codec for "just the [provenance] field of a layer manifest", used
   by the sidecar-fallback path below. The rest of the manifest is
   ignored. *)
let sidecar_provenance_codec =
  let open Jsont in
  Object.map ~kind:"layer_manifest_provenance" Fun.id
  |> Object.opt_mem "provenance" codec ~enc:Fun.id
  |> Object.finish

let try_decode_from_sidecar ~fs ~path : t option =
  try
    let s = Eio.Path.load Eio.Path.(fs / path) in
    match
      Jsont_bytesrw.decode_string ~locs:false ~file:path
        sidecar_provenance_codec s
    with
    | Ok r -> r
    | Error e ->
        Log.debug (fun m -> m "sidecar provenance bad %s: %s" path e);
        None
  with exn ->
    Log.debug (fun m ->
        m "sidecar provenance read %s: %s" path (Printexc.to_string exn));
    None

(* Read [provenance] for a single layer. Looks first at the legacy
   per-layer [provenance.json] sidecar (still present in older caches)
   and falls back to the unified [<cache>/registry/<os_key>/layers/<hash>.json]
   sidecar that build_pipeline now writes. The legacy path will fade
   out as users rebuild; new builds emit only the unified one. *)
let read_one ~fs ~cache_root ~os_key ~hash =
  let p = path ~cache_root ~os_key ~hash in
  if Eio.Path.is_file Eio.Path.(fs / p) then try_decode ~fs ~path:p
  else
    let sp = sidecar_path ~cache_root ~os_key ~hash in
    if Eio.Path.is_file Eio.Path.(fs / sp) then
      try_decode_from_sidecar ~fs ~path:sp
    else None

let overlay_of_layer ~fs ~cache_root ~os_key ~hash =
  match read_one ~fs ~cache_root ~os_key ~hash with
  | None -> None
  | Some p -> p.opam.origin.overlay

let read_all ~(fs : Eio.Fs.dir_ty Eio.Path.t) ~cache_root ~os_key =
  let layers_dir = cache_root / "layers" / os_key in
  match Eio.Path.read_dir Eio.Path.(fs / layers_dir) with
  | exception Eio.Exn.Io _ -> []
  | hashes ->
      List.filter_map
        (fun hash ->
          (* Skip files mixed in with the hash-named subdirs (e.g.
             [.partial] sentinels or stale [.tmp] files) — every real
             layer dir is a flat hex string with no extension. *)
          if String.contains hash '.' then None
          else read_one ~fs ~cache_root ~os_key ~hash)
        hashes
