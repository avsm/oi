[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Source tarball mirror in opam download-cache format. *)

let log_src = Logs.Src.create "oi.source_mirror"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

(* -- Paths --------------------------------------------------------------- *)

let dir ~cache = Cache.root_s cache / "mirror"
let db_path ~cache = dir ~cache / "index.db"
let url ~cache = OpamUrl.of_string ("file://" ^ dir ~cache)

let remote_url ~registry =
  let trimmed =
    let n = String.length registry in
    if n > 0 && registry.[n - 1] = '/' then String.sub registry 0 (n - 1)
    else registry
  in
  OpamUrl.of_string (trimmed ^ "/sources")

(* Path inside the mirror for a given checksum, e.g.
   {mirror}/sha256/ab/abcd…. *)
let path_of_checksum ~cache ck =
  List.fold_left ( / ) (dir ~cache) (OpamHash.to_path ck)

(* -- Sqlite schema ------------------------------------------------------- *)

let schema =
  {|
  CREATE TABLE IF NOT EXISTS sources (
    sha256    TEXT PRIMARY KEY,
    size      INTEGER NOT NULL,
    added_at  REAL NOT NULL
  );

  CREATE TABLE IF NOT EXISTS source_checksums (
    algo    TEXT NOT NULL,
    value   TEXT NOT NULL,
    sha256  TEXT NOT NULL REFERENCES sources(sha256) ON DELETE CASCADE,
    PRIMARY KEY (algo, value)
  );
  CREATE INDEX IF NOT EXISTS idx_source_checksums_sha256
    ON source_checksums(sha256);

  CREATE TABLE IF NOT EXISTS source_refs (
    sha256          TEXT NOT NULL REFERENCES sources(sha256) ON DELETE CASCADE,
    package_name    TEXT NOT NULL,
    package_version TEXT NOT NULL,
    url             TEXT NOT NULL,
    kind            TEXT NOT NULL CHECK (kind IN ('main','extra')),
    extra_name      TEXT NOT NULL DEFAULT '',
    PRIMARY KEY (sha256, package_name, package_version, kind, extra_name)
  );
  CREATE INDEX IF NOT EXISTS idx_source_refs_pkg
    ON source_refs(package_name, package_version);
|}

let exec db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      Fmt.failwith "source_mirror sqlite: %s: %s\nSQL: %s"
        (Sqlite3.Rc.to_string rc) (Sqlite3.errmsg db) sql

let rec mkdir_p d =
  if d = "/" || d = "." || d = "" || Sys.file_exists d then ()
  else begin
    mkdir_p (Filename.dirname d);
    try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let open_db ~cache =
  mkdir_p (dir ~cache);
  let db = Sqlite3.db_open (db_path ~cache) in
  exec db schema;
  exec db "PRAGMA journal_mode=WAL";
  exec db "PRAGMA synchronous=NORMAL";
  exec db "PRAGMA foreign_keys=ON";
  db

let close_db db = ignore (Sqlite3.db_close db)

let with_db ~cache f =
  let db = open_db ~cache in
  Fun.protect ~finally:(fun () -> close_db db) (fun () -> f db)

let quote s =
  let s = String.split_on_char '\'' s |> String.concat "''" in
  Fmt.str "'%s'" s

(* -- File operations ----------------------------------------------------- *)

let file_size path =
  try Int64.of_int (Unix.stat path).Unix.st_size with Unix.Unix_error _ -> 0L

(* Hardlink [src] → [dst]; fall back to byte-copy across filesystems or
   when the filesystem refuses hardlinks. Idempotent: if [dst] already
   exists we assume it's the right bytes (content-addressed path).

   If [src] is a symlink, resolve to the underlying file before
   linking. Opam writes the "best" checksum as a real file and the
   others as relative symlinks pointing at it; [Unix.link] on Linux
   doesn't dereference symlinks, so naively linking a symlink produces
   another symlink with the same (now-wrong) relative target. *)
let resolve_symlink path =
  try
    if (Unix.lstat path).Unix.st_kind = Unix.S_LNK then Unix.realpath path
    else path
  with Unix.Unix_error _ -> path

(* True when [dst] exists as something we want to keep (regular file or
   a symlink that resolves to one). Dangling symlinks — left behind by
   the pre-[resolve_symlink] version of this code — count as absent and
   get overwritten on the next build. *)
let dst_is_valid dst =
  try
    let _ = Unix.stat dst in
    true
  with Unix.Unix_error _ -> false

let link_or_copy ~src ~dst =
  let src = resolve_symlink src in
  if dst_is_valid dst then ()
  else begin
    mkdir_p (Filename.dirname dst);
    (* Clear a stale broken symlink / orphan tmp at [dst] so that
       [rename] below can succeed. [Unix.unlink] works on symlinks
       (including broken ones) and silently no-ops on absent paths. *)
    (try Unix.unlink dst with Unix.Unix_error _ -> ());
    let tmp = dst ^ ".tmp" in
    (try Unix.unlink tmp with Unix.Unix_error _ -> ());
    let linked =
      try
        Unix.link src tmp;
        true
      with
      | Unix.Unix_error
          ((Unix.EXDEV | Unix.EMLINK | Unix.EPERM | Unix.EOPNOTSUPP), _, _)
      ->
        false
    in
    if not linked then begin
      let ic = open_in_bin src in
      Fun.protect ~finally:(fun () -> close_in_noerr ic) @@ fun () ->
      let oc = open_out_bin tmp in
      Fun.protect ~finally:(fun () -> close_out_noerr oc) @@ fun () ->
      let buf = Bytes.create 65536 in
      let rec loop () =
        let n = input ic buf 0 (Bytes.length buf) in
        if n > 0 then begin
          output oc buf 0 n;
          loop ()
        end
      in
      loop ()
    end;
    try Unix.rename tmp dst
    with Unix.Unix_error _ -> (
      (* Lost a race with another writer; trust the existing file. *)
      try Sys.remove tmp with Sys_error _ -> ())
  end

(* -- Locating the file in opam's download-cache ------------------------- *)

let opam_cache_root () =
  OpamRepositoryPath.download_cache OpamStateConfig.(!r.root_dir)
  |> OpamFilename.Dir.to_string

let opam_cache_file checksums =
  let root = opam_cache_root () in
  List.find_map
    (fun ck ->
      let path = List.fold_left ( / ) root (OpamHash.to_path ck) in
      if Sys.file_exists path then Some path else None)
    checksums

(* -- Public: record ------------------------------------------------------ *)

let record ~sys:_ ~cache ~package ~kind ~url ~checksums =
  match checksums with
  | [] ->
      Log.debug (fun m ->
          m "no checksums for %s, nothing to mirror"
            (OpamPackage.to_string package))
  | _ -> (
      match opam_cache_file checksums with
      | None ->
          Log.debug (fun m ->
              m "no cached blob for %s, skipping mirror"
                (OpamPackage.to_string package))
      | Some src_path ->
          let sha256_hash = OpamHash.compute ~kind:`SHA256 src_path in
          let sha256 = OpamHash.contents sha256_hash in
          let size = file_size src_path in
          with_db ~cache @@ fun db ->
          exec db "BEGIN TRANSACTION";
          exec db
            (Fmt.str
               "INSERT OR IGNORE INTO sources (sha256, size, added_at) VALUES \
                (%s, %Ld, %f)"
               (quote sha256) size (Unix.time ()));
          (* Deposit under every declared checksum's path. *)
          let declared = ref [] in
          List.iter
            (fun ck ->
              let dst = path_of_checksum ~cache ck in
              link_or_copy ~src:src_path ~dst;
              let algo = OpamHash.(string_of_kind (kind ck)) in
              let value = OpamHash.contents ck in
              declared := (algo, value) :: !declared;
              exec db
                (Fmt.str
                   "INSERT OR IGNORE INTO source_checksums (algo, value, \
                    sha256) VALUES (%s, %s, %s)"
                   (quote algo) (quote value) (quote sha256)))
            checksums;
          (* Always ensure sha256 path exists, even if caller only passed md5. *)
          if not (List.exists (fun (a, _) -> a = "sha256") !declared) then begin
            let dst = path_of_checksum ~cache sha256_hash in
            link_or_copy ~src:src_path ~dst;
            exec db
              (Fmt.str
                 "INSERT OR IGNORE INTO source_checksums (algo, value, sha256) \
                  VALUES ('sha256', %s, %s)"
                 (quote sha256) (quote sha256))
          end;
          let extra_name, kind_s =
            match kind with `Main -> ("", "main") | `Extra n -> (n, "extra")
          in
          exec db
            (Fmt.str
               "INSERT OR IGNORE INTO source_refs (sha256, package_name, \
                package_version, url, kind, extra_name) VALUES (%s, %s, %s, \
                %s, %s, %s)"
               (quote sha256)
               (quote (OpamPackage.Name.to_string (OpamPackage.name package)))
               (quote
                  (OpamPackage.Version.to_string (OpamPackage.version package)))
               (quote (OpamUrl.to_string url))
               (quote kind_s) (quote extra_name));
          exec db "COMMIT")

(* -- Public: stats ------------------------------------------------------- *)

type stats = { count : int; total_size : int64 }

let stats ~cache =
  if not (Sys.file_exists (db_path ~cache)) then { count = 0; total_size = 0L }
  else
    with_db ~cache @@ fun db ->
    let stmt =
      Sqlite3.prepare db "SELECT COUNT(*), COALESCE(SUM(size), 0) FROM sources"
    in
    let r =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
          let count = Sqlite3.Data.to_int_exn (Sqlite3.column stmt 0) in
          let total_size =
            match Sqlite3.column stmt 1 with
            | Sqlite3.Data.INT n -> n
            | Sqlite3.Data.FLOAT f -> Int64.of_float f
            | _ -> 0L
          in
          { count; total_size }
      | _ -> { count = 0; total_size = 0L }
    in
    ignore (Sqlite3.finalize stmt);
    r

(* -- Public: list -------------------------------------------------------- *)

type entry = {
  sha256 : string;
  size : int64;
  package_name : string;
  package_version : string;
  kind : [ `Main | `Extra of string ];
  url : string;
}

let list ~cache ?package () =
  if not (Sys.file_exists (db_path ~cache)) then []
  else
    with_db ~cache @@ fun db ->
    let out = ref [] in
    let cb row =
      match row with
      | [| sha256; size; name; version; url; kind_s; extra_name |] ->
          let kind =
            match kind_s with
            | "main" -> `Main
            | "extra" -> `Extra extra_name
            | other -> `Extra other
          in
          let size = try Int64.of_string size with _ -> 0L in
          out :=
            {
              sha256;
              size;
              package_name = name;
              package_version = version;
              kind;
              url;
            }
            :: !out
      | _ -> ()
    in
    let where =
      match package with
      | None -> ""
      | Some p -> Fmt.str " WHERE r.package_name = %s" (quote p)
    in
    ignore
      (Sqlite3.exec_not_null_no_headers db ~cb
         (Fmt.str
            "SELECT s.sha256, s.size, r.package_name, r.package_version, \
             r.url, r.kind, r.extra_name FROM source_refs r JOIN sources s ON \
             s.sha256 = r.sha256%s ORDER BY r.package_name, r.package_version, \
             r.kind, r.extra_name"
            where));
    List.rev !out

(* -- Public: gc ---------------------------------------------------------- *)

(* Collect all (algo, value) rows referencing a given sha256; we need
   them to know which physical paths to unlink. *)
let checksums_for_sha256 db sha256 =
  let out = ref [] in
  let cb row =
    match row with [| algo; value |] -> out := (algo, value) :: !out | _ -> ()
  in
  ignore
    (Sqlite3.exec_not_null_no_headers db ~cb
       (Fmt.str "SELECT algo, value FROM source_checksums WHERE sha256 = %s"
          (quote sha256)));
  !out

let gc ~cache =
  if not (Sys.file_exists (db_path ~cache)) then 0
  else
    with_db ~cache @@ fun db ->
    let orphans = ref [] in
    let cb row =
      match row with [| sha |] -> orphans := sha :: !orphans | _ -> ()
    in
    ignore
      (Sqlite3.exec_not_null_no_headers db ~cb
         "SELECT s.sha256 FROM sources s LEFT JOIN source_refs r ON r.sha256 = \
          s.sha256 WHERE r.sha256 IS NULL");
    let removed = ref 0 in
    List.iter
      (fun sha ->
        let cks = checksums_for_sha256 db sha in
        List.iter
          (fun (algo, value) ->
            let shard =
              if String.length value >= 2 then String.sub value 0 2 else value
            in
            let path = dir ~cache / algo / shard / value in
            try
              Unix.unlink path;
              incr removed
            with Unix.Unix_error _ -> ())
          cks;
        exec db (Fmt.str "DELETE FROM sources WHERE sha256 = %s" (quote sha)))
      !orphans;
    !removed

(* -- Public: verify ------------------------------------------------------ *)

let verify ~sys:_ ~cache =
  if not (Sys.file_exists (db_path ~cache)) then []
  else
    with_db ~cache @@ fun db ->
    let bad = ref [] in
    let cb row =
      match row with
      | [| sha256; algo; value |] ->
          let shard =
            if String.length value >= 2 then String.sub value 0 2 else value
          in
          let path = dir ~cache / algo / shard / value in
          if not (Sys.file_exists path) then
            bad := (sha256, Fmt.str "missing: %s" path) :: !bad
          else begin
            let kind =
              match algo with
              | "md5" -> `MD5
              | "sha256" -> `SHA256
              | "sha512" -> `SHA512
              | other ->
                  bad := (sha256, Fmt.str "unknown algo: %s" other) :: !bad;
                  `SHA256
            in
            let got = OpamHash.contents (OpamHash.compute ~kind path) in
            if got <> value then
              bad :=
                (sha256, Fmt.str "%s mismatch at %s: %s" algo path got) :: !bad
          end
      | _ -> ()
    in
    ignore
      (Sqlite3.exec_not_null_no_headers db ~cb
         "SELECT sha256, algo, value FROM source_checksums");
    !bad

(* -- Public: merge_remote ------------------------------------------------ *)

(* Open [index_path] (create if missing — schema runs on open), ATTACH
   [remote_path], and INSERT OR IGNORE every row from the three mirror
   tables. Foreign keys between the three cascade on delete but not on
   insert — we rely on sha256 PKs/UNIQUEs and ON CONFLICT IGNORE. *)
let merge_remote ~index_path ~remote_path =
  mkdir_p (Filename.dirname index_path);
  let db = Sqlite3.db_open index_path in
  Fun.protect ~finally:(fun () -> close_db db) @@ fun () ->
  exec db schema;
  exec db "PRAGMA journal_mode=WAL";
  exec db "PRAGMA synchronous=NORMAL";
  exec db "PRAGMA foreign_keys=ON";
  exec db (Fmt.str "ATTACH DATABASE %s AS remote" (quote remote_path));
  (try
     exec db "BEGIN TRANSACTION";
     exec db "INSERT OR IGNORE INTO sources SELECT * FROM remote.sources";
     exec db
       "INSERT OR IGNORE INTO source_checksums SELECT * FROM \
        remote.source_checksums";
     exec db
       "INSERT OR IGNORE INTO source_refs SELECT * FROM remote.source_refs";
     exec db "COMMIT"
   with e ->
     (try exec db "ROLLBACK" with _ -> ());
     raise e);
  exec db "DETACH DATABASE remote"

(* -- Public: export ------------------------------------------------------ *)

(* Copy the mirror to [dst/sources]. Database is copied as-is; blobs are
   hardlinked where possible (same fs) and byte-copied otherwise, using
   [link_or_copy]. *)
let export ~cache ~dst =
  let src_dir = dir ~cache in
  if not (Sys.file_exists src_dir) then 0
  else
    let dst_root = Eio.Path.(dst / "sources") in
    let dst_s = Eio.Path.native_exn dst_root in
    mkdir_p dst_s;
    (* Copy database. *)
    if Sys.file_exists (db_path ~cache) then
      link_or_copy ~src:(db_path ~cache) ~dst:(dst_s / "index.db");
    (* Walk algo dirs: md5/, sha256/, sha512/. *)
    let algo_dirs = [ "md5"; "sha256"; "sha512" ] in
    let count = ref 0 in
    List.iter
      (fun algo ->
        let algo_src = src_dir / algo in
        if Sys.file_exists algo_src then begin
          let shards = Sys.readdir algo_src in
          Array.iter
            (fun shard ->
              let shard_src = algo_src / shard in
              if Sys.is_directory shard_src then begin
                let files = Sys.readdir shard_src in
                Array.iter
                  (fun hash ->
                    let sp = shard_src / hash in
                    if not (Sys.is_directory sp) then begin
                      let dp = dst_s / algo / shard / hash in
                      link_or_copy ~src:sp ~dst:dp;
                      incr count
                    end)
                  files
              end)
            shards
        end)
      algo_dirs;
    !count
