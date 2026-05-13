(* Sidecar JSON for a d10ir source archive.

   Written next to every [<sha>.tar.zst] under [<cache>/d10ir/archives/].
   Records the precise ingredients that fed the bake: primary source URL
   (with the *resolved* git commit for git pins), extra-sources,
   copy-in extra-files (with per-file sha256 so a server can verify them
   against the reporepo), patches, and substs.

   This is the unit a future indexer/UI walks when stitching binary
   provenance back to upstream source. The archive is content-addressed
   by sha256, so the sidecar is content-equivalent across any two packages
   whose bake inputs collapse to the same bytes; the first observer's
   record wins, which is fine because the upstream coordinates are
   identical by construction. *)

let log_src = Logs.Src.create "oi.source_manifest"

module Log = (val Logs.src_log log_src : Logs.LOG)

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

(* -- Helpers ------------------------------------------------------------- *)

let rfc3339_of_unix ts =
  let tm = Unix.gmtime ts in
  Fmt.str "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let strip_git_prefix url =
  if String.length url > 4 && String.sub url 0 4 = "git+" then
    String.sub url 4 (String.length url - 4)
  else url

let split_git_url url_s =
  let url = strip_git_prefix url_s in
  match String.index_opt url '#' with
  | None -> (url, None)
  | Some i ->
      let base = String.sub url 0 i in
      let frag = String.sub url (i + 1) (String.length url - i - 1) in
      (base, if frag = "" then None else Some frag)

let url_backend_kind url_s =
  match OpamUrl.parse_opt ~handle_suffix:true url_s with
  | None -> "unknown"
  | Some u -> (
      match u.OpamUrl.backend with
      | `git -> "git"
      | `http -> "archive"
      | `rsync -> "path"
      | `hg | `darcs -> "vcs")

let resolve_git_commit ~proc_mgr ~build_dir =
  if not (Sys.file_exists (Filename.concat build_dir ".git")) then None
  else
    try
      let out =
        Eio.Process.parse_out proc_mgr Eio.Buf_read.take_all
          [ "git"; "-C"; build_dir; "rev-parse"; "HEAD" ]
      in
      let s = String.trim out in
      if String.length s = 40 then Some s else None
    with _ -> None

let sha256_and_size path =
  try
    let stat = Unix.stat path in
    let h = OpamHash.contents (OpamHash.compute ~kind:`SHA256 path) in
    Some (h, stat.Unix.st_size)
  with _ -> None

(* -- Codecs -------------------------------------------------------------- *)

let source_codec =
  let open Jsont in
  Object.map ~kind:"source" (fun kind url ref_ commit_sha checksums ->
      { kind; url; ref_; commit_sha; checksums })
  |> Object.mem "kind" string ~enc:(fun (s : source) -> s.kind)
  |> Object.opt_mem "url" string ~enc:(fun (s : source) -> s.url)
  |> Object.opt_mem "ref" string ~enc:(fun (s : source) -> s.ref_)
  |> Object.opt_mem "commit_sha" string ~enc:(fun (s : source) -> s.commit_sha)
  |> Object.mem "checksums" (list string) ~dec_absent:[]
       ~enc:(fun (s : source) -> s.checksums)
       ~enc_omit:(( = ) [])
  |> Object.finish

let extra_source_codec =
  let open Jsont in
  Object.map ~kind:"extra_source" (fun name url checksums ->
      { name; url; checksums })
  |> Object.mem "name" string ~enc:(fun (s : extra_source) -> s.name)
  |> Object.mem "url" string ~enc:(fun (s : extra_source) -> s.url)
  |> Object.mem "checksums" (list string) ~dec_absent:[]
       ~enc:(fun (s : extra_source) -> s.checksums)
       ~enc_omit:(( = ) [])
  |> Object.finish

let extra_file_codec =
  let open Jsont in
  Object.map ~kind:"extra_file" (fun path sha256 size ->
      { path; sha256; size })
  |> Object.mem "path" string ~enc:(fun (f : extra_file) -> f.path)
  |> Object.mem "sha256" string ~enc:(fun (f : extra_file) -> f.sha256)
  |> Object.mem "size" int ~enc:(fun (f : extra_file) -> f.size)
  |> Object.finish

let patch_codec =
  let open Jsont in
  Object.map ~kind:"patch" (fun file filter -> { file; filter })
  |> Object.mem "file" string ~enc:(fun (p : patch) -> p.file)
  |> Object.opt_mem "filter" string ~enc:(fun (p : patch) -> p.filter)
  |> Object.finish

let codec : t Jsont.t =
  let open Jsont in
  Object.map ~kind:"oi.source.v1"
    (fun
      schema
      kind
      sha256
      date
      package_name
      package_version
      overlay_handle
      overlay_version
      source
      extra_sources
      extra_files
      patches
      substs
      strip_components
      size
    ->
      {
        schema;
        kind;
        sha256;
        date;
        package_name;
        package_version;
        overlay_handle;
        overlay_version;
        source;
        extra_sources;
        extra_files;
        patches;
        substs;
        strip_components;
        size;
      })
  |> Object.mem "schema" int ~enc:(fun r -> r.schema)
  |> Object.mem "kind" string ~enc:(fun r -> r.kind)
  |> Object.mem "sha256" string ~enc:(fun r -> r.sha256)
  |> Object.mem "date" string ~enc:(fun r -> r.date)
  |> Object.mem "package_name" string ~enc:(fun r -> r.package_name)
  |> Object.mem "package_version" string ~enc:(fun r -> r.package_version)
  |> Object.opt_mem "overlay_handle" string ~enc:(fun r -> r.overlay_handle)
  |> Object.opt_mem "overlay_version" string ~enc:(fun r -> r.overlay_version)
  |> Object.mem "source" source_codec ~enc:(fun r -> r.source)
  |> Object.mem "extra_sources"
       (list extra_source_codec)
       ~dec_absent:[]
       ~enc:(fun r -> r.extra_sources)
       ~enc_omit:(( = ) [])
  |> Object.mem "extra_files" (list extra_file_codec) ~dec_absent:[]
       ~enc:(fun r -> r.extra_files)
       ~enc_omit:(( = ) [])
  |> Object.mem "patches" (list patch_codec) ~dec_absent:[]
       ~enc:(fun r -> r.patches)
       ~enc_omit:(( = ) [])
  |> Object.mem "substs" (list string) ~dec_absent:[]
       ~enc:(fun r -> r.substs)
       ~enc_omit:(( = ) [])
  |> Object.mem "strip_components" int ~dec_absent:0 ~enc:(fun r ->
         r.strip_components)
  |> Object.opt_mem "size" int ~enc:(fun r -> r.size)
  |> Object.finish

(* -- Construction -------------------------------------------------------- *)

let source_of ~proc_mgr ~build_dir (src : Plan.source_info option) =
  match src with
  | None ->
      {
        kind = "none";
        url = None;
        ref_ = None;
        commit_sha = None;
        checksums = [];
      }
  | Some s ->
      let kind = url_backend_kind s.url in
      let base_url, ref_ =
        if kind = "git" then split_git_url s.url
        else (strip_git_prefix s.url, None)
      in
      let commit_sha =
        if kind = "git" then resolve_git_commit ~proc_mgr ~build_dir else None
      in
      {
        kind;
        url = Some base_url;
        ref_;
        commit_sha;
        checksums = s.checksums;
      }

let extra_source_of (name, (s : Plan.source_info)) =
  { name; url = strip_git_prefix s.url; checksums = s.checksums }

let extra_file_of (basename, src_path) =
  match sha256_and_size src_path with
  | None -> None
  | Some (sha256, size) -> Some { path = basename; sha256; size }

let patch_of (p : Plan.patch) = { file = p.file; filter = p.filter }

let make ~proc_mgr ~build_dir ~name ~version ?overlay_handle ?overlay_version
    ~sha256 ?size ?(strip_components = 0) ~source ~extra_sources ~extra_files
    ~patches ~substs () =
  {
    schema = 1;
    kind = "oi.source.v1";
    sha256;
    date = rfc3339_of_unix (Unix.gettimeofday ());
    package_name = name;
    package_version = version;
    overlay_handle;
    overlay_version;
    source = source_of ~proc_mgr ~build_dir source;
    extra_sources = List.map extra_source_of extra_sources;
    extra_files = List.filter_map extra_file_of extra_files;
    patches = List.map patch_of patches;
    substs;
    strip_components;
    size;
  }

(* -- Storage ------------------------------------------------------------- *)

let path_for ~archives_dir ~sha = Filename.concat archives_dir (sha ^ ".json")

let write ~fs ~archives_dir m =
  let dst = path_for ~archives_dir ~sha:m.sha256 in
  if Sys.file_exists dst then ()
  else
    match Jsont_bytesrw.encode_string ~format:Jsont.Indent codec m with
    | Error e ->
        Log.warn (fun mlog -> mlog "source manifest encode %s: %s" dst e)
    | Ok s -> (
        let tmp = dst ^ ".tmp" in
        try
          Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / tmp) s;
          Sys.rename tmp dst
        with exn ->
          Log.warn (fun mlog ->
              mlog "source manifest write %s: %s" dst (Printexc.to_string exn)))

let try_read ~path : t option =
  if not (Sys.file_exists path) then None
  else
    try
      let s =
        In_channel.with_open_text path (fun ic ->
            In_channel.input_all ic)
      in
      match Jsont_bytesrw.decode_string ~locs:true ~file:path codec s with
      | Ok r -> Some r
      | Error e ->
          Log.debug (fun m -> m "source manifest bad %s: %s" path e);
          None
    with exn ->
      Log.debug (fun m ->
          m "source manifest read %s: %s" path (Printexc.to_string exn));
      None
