(* Layer manifest — one JSON object per attempted layer build.

   Replaces the per-layer [.txt.zst] file-list sidecar AND folds in the
   information that today lives in [layer.json] + [provenance.json].
   Stored at [<cache>/registry/<os_key>/layers/<hash>.json] (and at
   [<base>/<os_key>/layers/<hash>.json] when uploaded). *)

let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.manifest_layer"

module Log = (val Logs.src_log log_src : Logs.LOG)

type outcome = Ok | Fail | Timeout

let string_of_outcome = function
  | Ok -> "ok"
  | Fail -> "fail"
  | Timeout -> "timeout"

let outcome_of_string = function
  | "ok" -> Stdlib.Ok Ok
  | "fail" -> Stdlib.Ok Fail
  | "timeout" -> Stdlib.Ok Timeout
  | s -> Stdlib.Error ("unknown outcome: " ^ s)

(* -- Records ------------------------------------------------------------ *)

type tarball = { sha256 : string; size : int; key : string option }

type file_entry = {
  path : string;
  kind : string;  (** "file" | "symlink" | "dir" *)
  size : int option;
  mode : string option;
  target : string option;  (** symlink target, when kind="symlink" *)
}

type dep = { name : string; version : string; hash : string }

type findlib_entry = {
  package_dir : string;
  findlib_pkg : string;
  archive : string option;
}

type source_archive = { sha256 : string; key : string option }

(* The "how to replay" record. Folds in what used to live in
   [<cache>/layers/<os_key>/<hash>/recipe.json] — the script + env
   needed to re-execute a build deterministically. Orthogonal to the
   [provenance] sub-object (which records WHERE the layer came from). *)
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
  method_ : string;  (** "Source" | "Binary" *)
  overlay_handle : string option;
  overlay_version : string option;
  (* Success-only *)
  tarball : tarball option;
  files : file_entry list;
  binaries : string list;
  findlib : findlib_entry list;
  exit_status : int option;
  provenance : Provenance.t option;
  recipe : recipe option;
  (* Failure-only *)
  failure : failure_info option;
  invocation_id : string option;
  (* Common *)
  source_archive : source_archive option;
  deps : dep list;
  depexts_declared : string list;
  build_env_ocaml_version : string option;
}

(* -- Helpers ------------------------------------------------------------ *)

let rfc3339_of_unix ts =
  let tm = Unix.gmtime ts in
  Fmt.str "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

(* -- Codecs ------------------------------------------------------------- *)

let outcome_codec =
  Jsont.map ~kind:"outcome"
    ~dec:(fun s ->
      match outcome_of_string s with Stdlib.Ok o -> o | Error e -> failwith e)
    ~enc:string_of_outcome Jsont.string

let tarball_codec =
  let open Jsont in
  Object.map ~kind:"tarball" (fun sha256 size key -> { sha256; size; key })
  |> Object.mem "sha256" string ~enc:(fun (t : tarball) -> t.sha256)
  |> Object.mem "size" int ~enc:(fun (t : tarball) -> t.size)
  |> Object.opt_mem "key" string ~enc:(fun (t : tarball) -> t.key)
  |> Object.finish

let file_entry_codec =
  let open Jsont in
  Object.map ~kind:"file_entry" (fun path kind size mode target ->
      { path; kind; size; mode; target })
  |> Object.mem "path" string ~enc:(fun (f : file_entry) -> f.path)
  |> Object.mem "kind" string ~enc:(fun (f : file_entry) -> f.kind)
  |> Object.opt_mem "size" int ~enc:(fun (f : file_entry) -> f.size)
  |> Object.opt_mem "mode" string ~enc:(fun (f : file_entry) -> f.mode)
  |> Object.opt_mem "target" string ~enc:(fun (f : file_entry) -> f.target)
  |> Object.finish

let dep_codec =
  let open Jsont in
  Object.map ~kind:"dep" (fun name version hash -> { name; version; hash })
  |> Object.mem "name" string ~enc:(fun (d : dep) -> d.name)
  |> Object.mem "version" string ~enc:(fun (d : dep) -> d.version)
  |> Object.mem "hash" string ~enc:(fun (d : dep) -> d.hash)
  |> Object.finish

let findlib_entry_codec =
  let open Jsont in
  Object.map ~kind:"findlib_entry" (fun package_dir findlib_pkg archive ->
      { package_dir; findlib_pkg; archive })
  |> Object.mem "package_dir" string ~enc:(fun (f : findlib_entry) ->
      f.package_dir)
  |> Object.mem "findlib_pkg" string ~enc:(fun (f : findlib_entry) ->
      f.findlib_pkg)
  |> Object.opt_mem "archive" string ~enc:(fun (f : findlib_entry) -> f.archive)
  |> Object.finish

let subst_pair_codec =
  let open Jsont in
  Object.map ~kind:"subst_var" (fun k v -> (k, v))
  |> Object.mem "name" string ~enc:(fun (k, _) -> k)
  |> Object.mem "value" string ~enc:(fun (_, v) -> v)
  |> Object.finish

let recipe_codec =
  let open Jsont in
  Object.map ~kind:"recipe" (fun script env substs subst_vars ->
      { script; env; substs; subst_vars })
  |> Object.mem "script" string ~enc:(fun (r : recipe) -> r.script)
  |> Object.mem "env" (list string) ~dec_absent:[]
       ~enc:(fun (r : recipe) -> r.env)
       ~enc_omit:(( = ) [])
  |> Object.mem "substs" (list string) ~dec_absent:[]
       ~enc:(fun (r : recipe) -> r.substs)
       ~enc_omit:(( = ) [])
  |> Object.mem "subst_vars" (list subst_pair_codec) ~dec_absent:[]
       ~enc:(fun (r : recipe) -> r.subst_vars)
       ~enc_omit:(( = ) [])
  |> Object.finish

let source_archive_codec =
  let open Jsont in
  Object.map ~kind:"source_archive" (fun sha256 key -> { sha256; key })
  |> Object.mem "sha256" string ~enc:(fun (s : source_archive) -> s.sha256)
  |> Object.opt_mem "key" string ~enc:(fun (s : source_archive) -> s.key)
  |> Object.finish

let log_tail_codec =
  let open Jsont in
  Object.map ~kind:"log_tail" (fun lines truncated text ->
      { lines; truncated; text })
  |> Object.mem "lines" int ~enc:(fun (l : log_tail) -> l.lines)
  |> Object.mem "truncated" bool ~enc:(fun (l : log_tail) -> l.truncated)
  |> Object.mem "text" string ~enc:(fun (l : log_tail) -> l.text)
  |> Object.finish

let failure_codec =
  let open Jsont in
  Object.map ~kind:"failure" (fun phase exit_status duration_s log ->
      { phase; exit_status; duration_s; log })
  |> Object.mem "phase" string ~enc:(fun (f : failure_info) -> f.phase)
  |> Object.opt_mem "exit_status" int ~enc:(fun (f : failure_info) ->
      f.exit_status)
  |> Object.mem "duration_s" number ~enc:(fun (f : failure_info) ->
      f.duration_s)
  |> Object.opt_mem "log" log_tail_codec ~enc:(fun (f : failure_info) -> f.log)
  |> Object.finish

let codec : t Jsont.t =
  let open Jsont in
  Object.map ~kind:"oi.layer.v1"
    (fun
      schema
      kind
      outcome
      hash
      os_key
      date
      package
      package_name
      package_ver
      method_
      overlay_handle
      overlay_version
      tarball
      files
      binaries
      findlib
      exit_status
      provenance
      recipe
      failure
      invocation_id
      source_archive
      deps
      depexts_declared
      build_env_ocaml_version
    ->
      {
        schema;
        kind;
        outcome;
        hash;
        os_key;
        date;
        package;
        package_name;
        package_ver;
        method_;
        overlay_handle;
        overlay_version;
        tarball;
        files;
        binaries;
        findlib;
        exit_status;
        provenance;
        recipe;
        failure;
        invocation_id;
        source_archive;
        deps;
        depexts_declared;
        build_env_ocaml_version;
      })
  |> Object.mem "schema" int ~enc:(fun r -> r.schema)
  |> Object.mem "kind" string ~enc:(fun r -> r.kind)
  |> Object.mem "outcome" outcome_codec ~enc:(fun r -> r.outcome)
  |> Object.mem "hash" string ~enc:(fun r -> r.hash)
  |> Object.mem "os_key" string ~enc:(fun r -> r.os_key)
  |> Object.mem "date" string ~enc:(fun r -> r.date)
  |> Object.mem "package" string ~enc:(fun r -> r.package)
  |> Object.mem "package_name" string ~enc:(fun r -> r.package_name)
  |> Object.mem "package_ver" string ~enc:(fun r -> r.package_ver)
  |> Object.mem "method" string ~enc:(fun r -> r.method_)
  |> Object.opt_mem "overlay_handle" string ~enc:(fun r -> r.overlay_handle)
  |> Object.opt_mem "overlay_version" string ~enc:(fun r -> r.overlay_version)
  |> Object.opt_mem "tarball" tarball_codec ~enc:(fun r -> r.tarball)
  |> Object.mem "files" (list file_entry_codec) ~dec_absent:[]
       ~enc:(fun r -> r.files)
       ~enc_omit:(( = ) [])
  |> Object.mem "binaries" (list string) ~dec_absent:[]
       ~enc:(fun r -> r.binaries)
       ~enc_omit:(( = ) [])
  |> Object.mem "findlib" (list findlib_entry_codec) ~dec_absent:[]
       ~enc:(fun r -> r.findlib)
       ~enc_omit:(( = ) [])
  |> Object.opt_mem "exit_status" int ~enc:(fun r -> r.exit_status)
  |> Object.opt_mem "provenance" Provenance.codec ~enc:(fun r -> r.provenance)
  |> Object.opt_mem "recipe" recipe_codec ~enc:(fun r -> r.recipe)
  |> Object.opt_mem "failure" failure_codec ~enc:(fun r -> r.failure)
  |> Object.opt_mem "invocation_id" string ~enc:(fun r -> r.invocation_id)
  |> Object.opt_mem "source_archive" source_archive_codec ~enc:(fun r ->
      r.source_archive)
  |> Object.mem "deps" (list dep_codec) ~dec_absent:[]
       ~enc:(fun r -> r.deps)
       ~enc_omit:(( = ) [])
  |> Object.mem "depexts_declared" (list string) ~dec_absent:[]
       ~enc:(fun r -> r.depexts_declared)
       ~enc_omit:(( = ) [])
  |> Object.opt_mem "build_env_ocaml_version" string ~enc:(fun r ->
      r.build_env_ocaml_version)
  |> Object.finish

(* -- Filesystem extraction --------------------------------------------- *)

(* Walks <fs_dir> and yields one [file_entry] per directory entry that
   matters for an indexer: regular files, symlinks, and directories.
   Mirrors the classification rules of [D10.Index.scan_files] but emits
   richer metadata (kind/size/mode/target). *)
let octal_mode_of stat = Fmt.str "%04o" (stat.Unix.st_perm land 0o7777)

let rec scan_fs ~fs_dir ~rel_prefix acc =
  let abs = if rel_prefix = "" then fs_dir else fs_dir / rel_prefix in
  match try Some (Sys.readdir abs) with Sys_error _ -> None with
  | None -> acc
  | Some entries ->
      Array.sort String.compare entries;
      Array.fold_left
        (fun acc name -> visit ~fs_dir ~rel_prefix ~name acc)
        acc entries

and visit ~fs_dir ~rel_prefix ~name acc =
  let rel = if rel_prefix = "" then name else rel_prefix / name in
  let abs = fs_dir / rel in
  match try Some (Unix.lstat abs) with Unix.Unix_error _ -> None with
  | None -> acc
  | Some stat -> (
      let mode = Some (octal_mode_of stat) in
      match stat.st_kind with
      | S_REG ->
          {
            path = rel;
            kind = "file";
            size = Some stat.st_size;
            mode;
            target = None;
          }
          :: acc
      | S_LNK ->
          let target =
            try Some (Unix.readlink abs) with Unix.Unix_error _ -> None
          in
          { path = rel; kind = "symlink"; size = None; mode; target } :: acc
      | S_DIR ->
          let acc =
            { path = rel; kind = "dir"; size = None; mode; target = None }
            :: acc
          in
          scan_fs ~fs_dir ~rel_prefix:rel acc
      | _ -> acc)

let files_of_fs_dir fs_dir =
  if not (Sys.file_exists fs_dir) then []
  else List.rev (scan_fs ~fs_dir ~rel_prefix:"" [])

(* -- Construction ------------------------------------------------------- *)

let success ~hash ~os_key ~package ~package_name ~package_ver ~method_
    ?overlay_handle ?overlay_version ~tarball ~files ~binaries ~findlib
    ~exit_status ?provenance ?recipe ?source_archive ~deps ~depexts_declared
    ?build_env_ocaml_version () =
  {
    schema = 1;
    kind = "oi.layer.v1";
    outcome = Ok;
    hash;
    os_key;
    date = rfc3339_of_unix (Unix.gettimeofday ());
    package;
    package_name;
    package_ver;
    method_;
    overlay_handle;
    overlay_version;
    tarball = Some tarball;
    files;
    binaries;
    findlib;
    exit_status = Some exit_status;
    provenance;
    recipe;
    failure = None;
    invocation_id = None;
    source_archive;
    deps;
    depexts_declared;
    build_env_ocaml_version;
  }

(* -- Storage ------------------------------------------------------------ *)

(* Layer manifest sidecar lives alongside the layer-hash directory it
   describes: [<cache>/layers/<os_key>/<hash>.json] sits next to
   [<cache>/layers/<os_key>/<hash>/], mirroring the export shape
   ([<output>/<os_key>/<hash>.tar.zst] + [<output>/<os_key>/<hash>.json]). *)
let registry_dir ~cache_root ~os_key = cache_root / "layers" / os_key

let path_for ~cache_root ~os_key ~hash =
  registry_dir ~cache_root ~os_key / (hash ^ ".json")

let same_digest a b = Digest.string a = Digest.string b

let matches_on_disk dst s =
  try same_digest s (In_channel.with_open_text dst In_channel.input_all)
  with Sys_error _ -> false

let write_payload ~fs ~dst s =
  let tmp = dst ^ ".tmp" in
  try
    Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / tmp) s;
    Sys.rename tmp dst
  with exn ->
    Log.warn (fun mlog ->
        mlog "layer manifest write %s: %s" dst (Printexc.to_string exn))

let write ~fs ~cache_root m =
  let dir = registry_dir ~cache_root ~os_key:m.os_key in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dir);
  let dst = path_for ~cache_root ~os_key:m.os_key ~hash:m.hash in
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent codec m with
  | Error e -> Log.warn (fun mlog -> mlog "layer manifest encode %s: %s" dst e)
  | Ok s ->
      (* Dedup on identical content (esp. for the failure-retry case). *)
      if matches_on_disk dst s then () else write_payload ~fs ~dst s

let try_read ~path : t option =
  if not (Sys.file_exists path) then None
  else
    try
      let s = In_channel.with_open_text path In_channel.input_all in
      match Jsont_bytesrw.decode_string ~locs:true ~file:path codec s with
      | Stdlib.Ok r -> Some r
      | Error e ->
          Log.debug (fun m -> m "layer manifest bad %s: %s" path e);
          None
    with exn ->
      Log.debug (fun m ->
          m "layer manifest read %s: %s" path (Printexc.to_string exn));
      None
