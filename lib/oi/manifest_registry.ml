(* Per-os_key schema-version pointer written at
   [<cache>/registry/<os_key>/registry.json] (and uploaded as
   [<base>/<os_key>/registry.json]).

   Tiny, write-once-per-schema-bump. A consumer reads this first to
   confirm it understands the layout it's about to ingest. *)

let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.manifest_registry"

module Log = (val Logs.src_log log_src : Logs.LOG)

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

let current : t -> t =
 fun base ->
  {
    base with
    schema = 1;
    kind = "oi.registry.v1";
    layer_manifest_schema = 1;
    build_manifest_schema = 1;
    source_manifest_schema = 1;
  }

let rfc3339_of_unix ts =
  let tm = Unix.gmtime ts in
  Fmt.str "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let codec : t Jsont.t =
  let open Jsont in
  Object.map ~kind:"oi.registry.v1"
    (fun schema kind os_key l_schema b_schema s_schema wrote_by date ->
      {
        schema;
        kind;
        os_key;
        layer_manifest_schema = l_schema;
        build_manifest_schema = b_schema;
        source_manifest_schema = s_schema;
        wrote_by;
        date;
      })
  |> Object.mem "schema" int ~enc:(fun r -> r.schema)
  |> Object.mem "kind" string ~enc:(fun r -> r.kind)
  |> Object.mem "os_key" string ~enc:(fun r -> r.os_key)
  |> Object.mem "layer_manifest_schema" int ~enc:(fun r ->
      r.layer_manifest_schema)
  |> Object.mem "build_manifest_schema" int ~enc:(fun r ->
      r.build_manifest_schema)
  |> Object.mem "source_manifest_schema" int ~enc:(fun r ->
      r.source_manifest_schema)
  |> Object.mem "wrote_by" string ~enc:(fun r -> r.wrote_by)
  |> Object.mem "date" string ~enc:(fun r -> r.date)
  |> Object.finish

let path_for ~cache_root ~os_key =
  cache_root / "layers" / os_key / "registry.json"

let is_current r =
  r.schema = 1
  && r.layer_manifest_schema = 1
  && r.build_manifest_schema = 1
  && r.source_manifest_schema = 1

let needs_write_at dst =
  if not (Sys.file_exists dst) then true
  else
    try
      let s = In_channel.with_open_text dst In_channel.input_all in
      match Jsont_bytesrw.decode_string ~locs:false ~file:dst codec s with
      | Stdlib.Ok r -> not (is_current r)
      | Error _ -> true
    with Sys_error _ | End_of_file -> true

let write_atomic ~fs ~dst s =
  let parent = Filename.dirname dst in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / parent);
  let tmp = Fmt.str "%s.tmp.%d" dst (Unix.getpid ()) in
  try
    Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / tmp) s;
    Sys.rename tmp dst
  with exn ->
    Log.warn (fun mlog ->
        mlog "registry pointer write %s: %s" dst (Printexc.to_string exn))

let lock_path_for ~cache_root ~os_key =
  cache_root / "layers" / os_key / ".registry.lock"

let write_current ~fs ~dst ~os_key ~wrote_by =
  let r =
    current
      {
        schema = 1;
        kind = "oi.registry.v1";
        os_key;
        layer_manifest_schema = 1;
        build_manifest_schema = 1;
        source_manifest_schema = 1;
        wrote_by;
        date = rfc3339_of_unix (Unix.gettimeofday ());
      }
  in
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent codec r with
  | Error e ->
      Log.warn (fun mlog -> mlog "registry pointer encode %s: %s" dst e)
  | Ok s -> write_atomic ~fs ~dst s

(* Idempotent: writes only when the existing file is missing or has a
   different [schema]/[kind] (so a schema bump is the only thing that
   triggers a re-write). Called from every successful build manifest
   write to keep the pointer in sync.

   Serialised across processes by a per-os_key lockfile next to the
   pointer; the double-check on [needs_write_at] after acquiring lets
   the second writer no-op when the first already produced the
   current-schema file. *)
let ensure ~clock ~fs ~cache_root ~os_key ~wrote_by =
  let dst = path_for ~cache_root ~os_key in
  let lock_path = lock_path_for ~cache_root ~os_key in
  D10.Lock.with_lock ~clock ~fs ~path:lock_path ~mode:Exclusive @@ fun _lock ->
  if needs_write_at dst then write_current ~fs ~dst ~os_key ~wrote_by
