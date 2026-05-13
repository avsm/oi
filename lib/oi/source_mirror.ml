[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(* Source-archive mirror (sqlite-indexed cache used by registry
   builds). Re-exported as [Source.Mirror]. *)

let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.source.mirror"

module Log = (val Logs.src_log log_src : Logs.LOG)

let dir ~cache = Cache.root_s cache / "mirror"
let url ~cache = OpamUrl.of_string ("file://" ^ dir ~cache)

let remote_url ~registry =
  let trimmed =
    let n = String.length registry in
    if n > 0 && registry.[n - 1] = '/' then String.sub registry 0 (n - 1)
    else registry
  in
  OpamUrl.of_string (trimmed ^ "/sources")

let mkdir_p ~fs d =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / d)

let file_size path =
  try Int64.of_int (Unix.stat path).Unix.st_size with Unix.Unix_error _ -> 0L

let resolve_symlink path =
  try
    if (Unix.lstat path).Unix.st_kind = Unix.S_LNK then Unix.realpath path
    else path
  with Unix.Unix_error _ -> path

let dst_is_valid dst =
  try
    let _ = Unix.stat dst in
    true
  with Unix.Unix_error _ -> false

(* Concurrency-safe content-addressed write. Two registry containers
   building the same source in parallel share the bind-mounted
   [/out/sources/] tree, so two processes will race on the same
   [dst]. Both [Unix.link src dst] and [Unix.rename tmp dst] are
   atomic on POSIX; we lean on that and add a per-PID + nonce tmp
   name so the two callers never share a tmp path. EEXIST is treated
   as "someone else won the race", which is fine since the file is
   content-addressed: same hash means same bytes. *)
let unique_tmp dst =
  Fmt.str "%s.%d.%d.tmp" dst (Unix.getpid ()) (Random.bits ())

let rec stream_chunks ~buf ic oc =
  let n = input ic buf 0 (Bytes.length buf) in
  if n > 0 then begin
    output oc buf 0 n;
    stream_chunks ~buf ic oc
  end

let stream_ic_to_path ic dst =
  let oc = open_out_bin dst in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> stream_chunks ~buf:(Bytes.create 65536) ic oc)

(* Stream bytes from [src] to [dst] file path. Returns [true] on success. *)
let copy_file_bytes_to src dst =
  try
    let ic = open_in_bin src in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> stream_ic_to_path ic dst);
    true
  with Sys_error _ | Unix.Unix_error _ -> false

(* Cross-device or filesystem-without-hardlinks fallback: byte-copy via a
   unique tmp path, then atomically link tmp → dst. *)
let copy_then_link ~src ~dst =
  let tmp = unique_tmp dst in
  (try Unix.unlink tmp with Unix.Unix_error _ -> ());
  if copy_file_bytes_to src tmp then begin
    (match Unix.link tmp dst with
    | () -> ()
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    try Sys.remove tmp with Sys_error _ -> ()
  end
  else try Sys.remove tmp with Sys_error _ -> ()

let link_or_copy ~fs ~src ~dst =
  let src = resolve_symlink src in
  if dst_is_valid dst then ()
  else begin
    mkdir_p ~fs (Filename.dirname dst);
    (* Fast path: a single hardlink straight to [dst]. Atomic; if another
       writer beat us we get EEXIST and we're done. *)
    match Unix.link src dst with
    | () -> ()
    | exception Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    | exception
        Unix.Unix_error
          ((Unix.EXDEV | Unix.EMLINK | Unix.EPERM | Unix.EOPNOTSUPP), _, _) ->
        copy_then_link ~src ~dst
  end

(* Disk-walk helpers — no SQLite metadata; the on-disk layout
   [<dir>/<algo>/<XX>/<full-hash>] is the single source of truth. *)

let blobs_under_shard ~algo shard_dir =
  Sys.readdir shard_dir |> Array.to_list
  |> List.filter_map (fun hash ->
      let path = shard_dir / hash in
      if Sys.is_directory path then None else Some (algo, hash, path))

let blobs_under_algo ~mirror_dir algo =
  let algo_dir = mirror_dir / algo in
  if not (Sys.file_exists algo_dir) then []
  else
    Sys.readdir algo_dir |> Array.to_list
    |> List.filter (fun s -> Sys.is_directory (algo_dir / s))
    |> List.concat_map (fun shard -> blobs_under_shard ~algo (algo_dir / shard))

let walk_blobs ~mirror_dir =
  List.concat_map (blobs_under_algo ~mirror_dir) [ "md5"; "sha256"; "sha512" ]

type stats = { count : int; total_size : int64 }

let stats ~cache =
  let mirror_dir = dir ~cache in
  if not (Sys.file_exists mirror_dir) then { count = 0; total_size = 0L }
  else
    let blobs = walk_blobs ~mirror_dir in
    let total =
      List.fold_left (fun acc (_, _, p) -> Int64.add acc (file_size p)) 0L blobs
    in
    { count = List.length blobs; total_size = total }

let export ~cache ~dst =
  let src_dir = dir ~cache in
  if not (Sys.file_exists src_dir) then 0
  else
    let fs = Cache.fs cache in
    let dst_root = Eio.Path.(dst / "sources") in
    let dst_s = Eio.Path.native_exn dst_root in
    mkdir_p ~fs dst_s;
    let blobs = walk_blobs ~mirror_dir:src_dir in
    List.iter
      (fun (algo, hash, src) ->
        let shard =
          if String.length hash >= 2 then String.sub hash 0 2 else hash
        in
        let dp = dst_s / algo / shard / hash in
        link_or_copy ~fs ~src ~dst:dp)
      blobs;
    List.length blobs

(* Locate a fetched blob in opam's own download cache. opam keys
   each blob by its checksums under [download_cache/<algo>/<XX>/<hash>],
   same layout we use for the mirror. Walk the declared checksums
   and return the first one that exists on disk. *)
let opam_cached_blob checksums =
  let root =
    OpamRepositoryPath.download_cache OpamStateConfig.(!r.root_dir)
    |> OpamFilename.Dir.to_string
  in
  List.find_map
    (fun ck ->
      let path = List.fold_left ( / ) root (OpamHash.to_path ck) in
      if Sys.file_exists path then Some path else None)
    checksums

(* Deposit [src] under every declared checksum so future lookups against any
   algo hit the mirror. Always also deposit under the SHA-256 path computed
   from the actual bytes so [sources/sha256/] is consistent even when the
   recipe only declared md5. Returns the count of newly-deposited entries. *)
let deposit_under_checksums ~fs ~mirror_dir ~src checksums =
  let promoted = ref 0 in
  let put dst =
    if not (Sys.file_exists dst) then begin
      link_or_copy ~fs ~src ~dst;
      incr promoted
    end
  in
  List.iter
    (fun ck ->
      let dst = List.fold_left ( / ) mirror_dir (OpamHash.to_path ck) in
      put dst)
    checksums;
  (if not (List.exists (fun ck -> OpamHash.kind ck = `SHA256) checksums) then
     try
       let sha256_hash = OpamHash.compute ~kind:`SHA256 src in
       let dst =
         List.fold_left ( / ) mirror_dir (OpamHash.to_path sha256_hash)
       in
       put dst
     with Sys_error _ | Failure _ -> ());
  !promoted

let import_from_opam_cache ~fs ~cache_root checksums =
  match checksums with
  | [] -> 0
  | _ -> (
      match opam_cached_blob checksums with
      | None ->
          Log.debug (fun m ->
              m
                "import_from_opam_cache: no opam-cached blob for %d \
                 checksum(s), skipping"
                (List.length checksums));
          0
      | Some src ->
          let mirror_dir = cache_root / "mirror" in
          deposit_under_checksums ~fs ~mirror_dir ~src checksums)

(* -- Bulk fetch into the mirror (used by [oi build --archives-only]) --- *)

type archive = { url : OpamUrl.t; checksums : OpamHash.t list; pkg : string }

type fetch_summary = {
  fetched : int;
  cached : int;
  failed : (string * string) list;
  bytes_added : int64;
}

let read_opam path =
  try Some (OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path)))
  with Sys_error _ | Failure _ -> None

let archive_of_url ~pkg u =
  { url = OpamFile.URL.url u; checksums = OpamFile.URL.checksum u; pkg }

(* Drop checksum-less entries: a content-addressed mirror needs a hash
   to key on. In practice this skips git+ pins (the commit hash takes
   the place of an integrity check), which are resolved by clone, not
   by archive download. *)
let archives_of_opam ~pkg opam =
  let main = match OpamFile.OPAM.url opam with None -> [] | Some u -> [ u ] in
  let extras = List.map snd (OpamFile.OPAM.extra_sources opam) in
  main @ extras
  |> List.map (archive_of_url ~pkg)
  |> List.filter (fun a -> a.checksums <> [])

let archives_of_opam_file ~path ~pkg =
  match read_opam path with
  | None ->
      Log.info (fun m -> m "skipping unreadable opam file: %s" path);
      []
  | Some opam -> archives_of_opam ~pkg opam

let dedup_by_url archives =
  let seen : (string, unit) Hashtbl.t = Hashtbl.create 256 in
  List.filter
    (fun a ->
      let key = OpamUrl.to_string a.url in
      if Hashtbl.mem seen key then false
      else (
        Hashtbl.add seen key ();
        true))
    archives

(* Compact progress label: hostname + final path component. The full URL
   is too long for a single-line in-place progress sink. *)
let rec drop_leading_slashes s =
  if String.length s > 0 && s.[0] = '/' then
    drop_leading_slashes (String.sub s 1 (String.length s - 1))
  else s

let strip_url_scheme s =
  match String.index_opt s ':' with
  | None -> s
  | Some i ->
      let rest = String.sub s (i + 1) (String.length s - i - 1) in
      drop_leading_slashes rest

let label_of_url (u : OpamUrl.t) =
  let no_scheme = strip_url_scheme (OpamUrl.to_string u) in
  let host, rest =
    match String.index_opt no_scheme '/' with
    | None -> (no_scheme, "")
    | Some i ->
        ( String.sub no_scheme 0 i,
          String.sub no_scheme (i + 1) (String.length no_scheme - i - 1) )
  in
  let basename =
    match String.rindex_opt rest '/' with
    | None -> rest
    | Some i -> String.sub rest (i + 1) (String.length rest - i - 1)
  in
  if basename = "" then host else host ^ "/" ^ basename

let collect_archives ~packages_dirs pkgs =
  let opam_path_for pkg =
    let name_s = OpamPackage.Name.to_string (OpamPackage.name pkg) in
    let pkg_s = OpamPackage.to_string pkg in
    List.find_opt Sys.file_exists
      (List.map (fun d -> d / name_s / pkg_s / "opam") packages_dirs)
  in
  pkgs
  |> List.concat_map (fun pkg ->
      match opam_path_for pkg with
      | None -> []
      | Some path ->
          archives_of_opam_file ~path ~pkg:(OpamPackage.to_string pkg))
  |> dedup_by_url

let mirror_has ~mirror_dir checksums =
  List.exists
    (fun ck ->
      Sys.file_exists (List.fold_left ( / ) mirror_dir (OpamHash.to_path ck)))
    checksums

type origin = Local_mirror of string | Other

(* Probe each [file://] [cache_url] for any of [checksums]. Returns
   [Local_mirror path] on the first hit, [Other] otherwise. We
   deliberately don't probe HTTP(S) cache URLs: opam's [pull_tree]
   will consult them itself, and a per-package HEAD probe just to
   refine a log line is a wasted round-trip. The [Other] case
   therefore means "either the registry or upstream — opam will
   decide" and the actual fetch goes through [cache_urls] as usual. *)
let probe_local_cache_url checksums (cu : OpamUrl.t) =
  let s = OpamUrl.to_string cu in
  if not (String.starts_with ~prefix:"file://" s) then None
  else
    let base = String.sub s 7 (String.length s - 7) in
    List.find_map
      (fun ck ->
        let path = List.fold_left ( / ) base (OpamHash.to_path ck) in
        if Sys.file_exists path then Some (Local_mirror path) else None)
      checksums

let source_origin ~cache_urls ~checksums =
  if checksums = [] then Other
  else
    match List.find_map (probe_local_cache_url checksums) cache_urls with
    | Some o -> o
    | None -> Other

let fetch_one ~fs ~mirror_dir ~cache_root ~cache_dir ~tmp_dir a =
  let tmp = tmp_dir / Fmt.str "%d.%d.bin" (Unix.getpid ()) (Random.bits ()) in
  (try Unix.unlink tmp with Unix.Unix_error _ -> ());
  let dst_file = OpamFilename.of_string tmp in
  let result =
    try
      OpamRepository.pull_file a.pkg ~cache_dir ~cache_urls:[] ~silent_hits:true
        dst_file a.checksums [ a.url ]
      |> OpamProcess.Job.run
    with exn -> OpamTypes.Not_available (None, Printexc.to_string exn)
  in
  let outcome =
    match result with
    | OpamTypes.Result () | OpamTypes.Up_to_date () -> (
        try
          let _ = import_from_opam_cache ~fs ~cache_root a.checksums in
          (* Size lookup goes through the mirror because opam may have
             served from its download-cache without writing to [tmp]. *)
          let bytes =
            match a.checksums with
            | ck :: _ ->
                file_size
                  (List.fold_left ( / ) mirror_dir (OpamHash.to_path ck))
            | [] -> 0L
          in
          `Fetched bytes
        with exn -> `Failed (Printexc.to_string exn))
    | OpamTypes.Not_available (_, msg) -> `Failed msg
  in
  (try Sys.remove tmp with Sys_error _ -> ());
  outcome

let process_one_archive ~fs ~mirror_dir ~cache_root ~cache_dir ~tmp_dir ~fetched
    ~cached ~failed ~bytes_added a =
  if mirror_has ~mirror_dir a.checksums then incr cached
  else
    match fetch_one ~fs ~mirror_dir ~cache_root ~cache_dir ~tmp_dir a with
    | `Fetched bytes ->
        incr fetched;
        bytes_added := Int64.add !bytes_added bytes
    | `Failed msg ->
        Log.info (fun m ->
            m "fetch failed: %s -> %s" (OpamUrl.to_string a.url) msg);
        failed := (OpamUrl.to_string a.url, msg) :: !failed

let fetch_archives ~fs ~cache
    ?(on_progress = fun ~fetched:_ ~total:_ ~current:_ -> ()) archives =
  let mirror_dir = dir ~cache in
  let cache_root = Cache.root_s cache in
  let cache_dir =
    OpamRepositoryPath.download_cache OpamStateConfig.(!r.root_dir)
  in
  let tmp_dir = mirror_dir / ".incoming" in
  mkdir_p ~fs mirror_dir;
  mkdir_p ~fs tmp_dir;
  let total = List.length archives in
  let fetched = ref 0 and cached = ref 0 and failed = ref [] in
  let bytes_added = ref 0L in
  let done_count () = !fetched + !cached + List.length !failed in
  List.iter
    (fun a ->
      on_progress ~fetched:(done_count ()) ~total
        ~current:(Some (label_of_url a.url));
      process_one_archive ~fs ~mirror_dir ~cache_root ~cache_dir ~tmp_dir
        ~fetched ~cached ~failed ~bytes_added a)
    archives;
  on_progress ~fetched:total ~total ~current:None;
  (try Unix.rmdir tmp_dir with Unix.Unix_error _ -> ());
  {
    fetched = !fetched;
    cached = !cached;
    failed = List.rev !failed;
    bytes_added = !bytes_added;
  }
