[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

let log_src = Logs.Src.create "d10.index"

module Log = (val Logs.src_log log_src : Logs.LOG)

type db = Sqlite3.db

let exec db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | rc ->
      Fmt.failwith "layer_index sqlite: %s: %s" (Sqlite3.Rc.to_string rc) sql

let schema =
  {|
  CREATE TABLE IF NOT EXISTS layers (
    hash            TEXT PRIMARY KEY,
    os_key          TEXT NOT NULL,
    arch            TEXT NOT NULL,
    os              TEXT NOT NULL,
    distro          TEXT NOT NULL,
    os_version      TEXT NOT NULL,
    package_name    TEXT NOT NULL,
    package_ver     TEXT NOT NULL,
    exit_status     INTEGER NOT NULL,
    created         REAL NOT NULL,
    overlay_handle  TEXT,
    overlay_version TEXT
  );

  CREATE TABLE IF NOT EXISTS layer_deps (
    layer_hash    TEXT NOT NULL,
    dep_name      TEXT NOT NULL,
    dep_version   TEXT NOT NULL,
    dep_hash      TEXT NOT NULL,
    FOREIGN KEY (layer_hash) REFERENCES layers(hash)
  );

  CREATE TABLE IF NOT EXISTS layer_binaries (
    layer_hash    TEXT NOT NULL,
    binary_name   TEXT NOT NULL,
    FOREIGN KEY (layer_hash) REFERENCES layers(hash)
  );

  CREATE TABLE IF NOT EXISTS layer_files (
    layer_hash    TEXT NOT NULL,
    path          TEXT NOT NULL,
    FOREIGN KEY (layer_hash) REFERENCES layers(hash)
  );

  CREATE INDEX IF NOT EXISTS idx_layers_os_key ON layers(os_key);
  CREATE INDEX IF NOT EXISTS idx_layers_arch ON layers(arch);
  CREATE INDEX IF NOT EXISTS idx_layers_distro ON layers(distro);
  CREATE INDEX IF NOT EXISTS idx_layers_name ON layers(package_name, os_key);
  -- idx_layers_overlay is created in [open_] after the ALTER TABLE
  -- migration has added overlay_handle / overlay_version to pre-existing
  -- schemas; creating it here would fail on older DBs.
  CREATE INDEX IF NOT EXISTS idx_binaries_name ON layer_binaries(binary_name);
  CREATE INDEX IF NOT EXISTS idx_deps_hash ON layer_deps(layer_hash);
  CREATE INDEX IF NOT EXISTS idx_files_hash ON layer_files(layer_hash);
|}

let rec mkdir_p dir =
  if dir = "/" || dir = "." || dir = "" || Sys.file_exists dir then ()
  else begin
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

(* Idempotent ALTER TABLE for older index DBs that predate the
   overlay_handle / overlay_version columns. SQLite has no
   [ADD COLUMN IF NOT EXISTS]; we simply ignore the duplicate-column
   error. *)
let try_add_column db sql =
  match Sqlite3.exec db sql with
  | Sqlite3.Rc.OK -> ()
  | Sqlite3.Rc.ERROR -> ()
  | rc ->
      Fmt.failwith "layer_index sqlite: %s: %s" (Sqlite3.Rc.to_string rc) sql

let open_ ~path =
  mkdir_p (Filename.dirname path);
  let db = Sqlite3.db_open path in
  exec db schema;
  try_add_column db "ALTER TABLE layers ADD COLUMN overlay_handle TEXT";
  try_add_column db "ALTER TABLE layers ADD COLUMN overlay_version TEXT";
  exec db
    "CREATE INDEX IF NOT EXISTS idx_layers_overlay ON layers(overlay_handle, \
     overlay_version)";
  exec db "PRAGMA journal_mode=WAL";
  exec db "PRAGMA synchronous=NORMAL";
  db

let close db = ignore (Sqlite3.db_close db)

let quote s =
  let s = String.split_on_char '\'' s |> String.concat "''" in
  Fmt.str "'%s'" s

(* -- Indexing ------------------------------------------------------------- *)

(* Scan a layer's fs/ directory for files, return relative paths *)
let scan_files ~fs fs_dir =
  let files = ref [] in
  let base = Eio.Path.(fs / fs_dir) in
  let rec scan rel_dir =
    let dir = if rel_dir = "" then base else Eio.Path.(base / rel_dir) in
    if Sysops.file_exists dir then
      List.iter
        (fun name ->
          let rel =
            if rel_dir = "" then name else Filename.concat rel_dir name
          in
          try
            let st = Eio.Path.stat ~follow:false Eio.Path.(dir / name) in
            if st.kind = `Regular_file then files := rel :: !files
            else if st.kind = `Directory then scan rel
          with Eio.Exn.Io _ -> ())
        (Eio.Path.read_dir dir)
  in
  scan "";
  !files

let parse_pkg_string s =
  try
    let pkg = OpamPackage.of_string s in
    ( OpamPackage.Name.to_string (OpamPackage.name pkg),
      OpamPackage.Version.to_string (OpamPackage.version pkg) )
  with Failure _ -> (s, "")

let rebuild (c : Config.t) db =
  let layers_dir = Eio.Path.(c.root / "layers" / c.os_key) in
  let os_key = c.os_key in
  if not (Sysops.file_exists layers_dir) then ()
  else begin
    let parts = Os_key.of_string os_key in
    let { Os_key.distro; os_version; arch; os } = parts in
    Log.info (fun m ->
        m "Indexing layers for %s (%s/%s/%s)" os_key distro arch os);
    exec db
      (Fmt.str
         "DELETE FROM layer_files WHERE layer_hash IN (SELECT hash FROM layers \
          WHERE os_key = %s)"
         (quote os_key));
    exec db
      (Fmt.str
         "DELETE FROM layer_binaries WHERE layer_hash IN (SELECT hash FROM \
          layers WHERE os_key = %s)"
         (quote os_key));
    exec db
      (Fmt.str
         "DELETE FROM layer_deps WHERE layer_hash IN (SELECT hash FROM layers \
          WHERE os_key = %s)"
         (quote os_key));
    exec db (Fmt.str "DELETE FROM layers WHERE os_key = %s" (quote os_key));
    let entries = Eio.Path.read_dir layers_dir in
    exec db "BEGIN TRANSACTION";
    List.iter
      (fun hash ->
        let info =
          Layer.load_meta Eio.Path.(layers_dir / hash / "layer.json")
        in
        match info with
        | None -> ()
        | Some info ->
            let name, version = parse_pkg_string info.package in
            let null_or_quote = function None -> "NULL" | Some s -> quote s in
            exec db
              (Fmt.str
                 "INSERT OR REPLACE INTO layers (hash, os_key, arch, os, \
                  distro, os_version, package_name, package_ver, exit_status, \
                  created, overlay_handle, overlay_version) VALUES (%s, %s, \
                  %s, %s, %s, %s, %s, %s, %d, %f, %s, %s)"
                 (quote hash) (quote os_key) (quote arch) (quote os)
                 (quote distro) (quote os_version) (quote name) (quote version)
                 info.exit_status info.created
                 (null_or_quote info.overlay_handle)
                 (null_or_quote info.overlay_version));
            (* Insert deps *)
            List.iteri
              (fun i dep_s ->
                let dep_name, dep_version = parse_pkg_string dep_s in
                let dep_hash =
                  if i < List.length info.hashes then List.nth info.hashes i
                  else ""
                in
                exec db
                  (Fmt.str
                     "INSERT INTO layer_deps (layer_hash, dep_name, \
                      dep_version, dep_hash) VALUES (%s, %s, %s, %s)"
                     (quote hash) (quote dep_name) (quote dep_version)
                     (quote dep_hash)))
              info.deps;
            (* Scan files in fs/ *)
            let fs_dir = Eio.Path.(layers_dir / hash / "fs") in
            if Sysops.file_exists fs_dir then begin
              let files =
                scan_files ~fs:c.Config.fs (Eio.Path.native_exn fs_dir)
              in
              List.iter
                (fun path ->
                  exec db
                    (Fmt.str
                       "INSERT INTO layer_files (layer_hash, path) VALUES (%s, \
                        %s)"
                       (quote hash) (quote path));
                  (* Track binaries separately (bin/ and sbin/) *)
                  let bin_name =
                    if String.length path > 4 && String.sub path 0 4 = "bin/"
                    then Some (String.sub path 4 (String.length path - 4))
                    else if
                      String.length path > 5 && String.sub path 0 5 = "sbin/"
                    then Some (String.sub path 5 (String.length path - 5))
                    else None
                  in
                  Option.iter
                    (fun name ->
                      exec db
                        (Fmt.str
                           "INSERT INTO layer_binaries (layer_hash, \
                            binary_name) VALUES (%s, %s)"
                           (quote hash) (quote name)))
                    bin_name)
                files
            end)
      entries;
    exec db "COMMIT";
    Log.info (fun m ->
        m "Indexed %d layers for %s" (List.length entries) os_key)
  end

(* -- Queries -------------------------------------------------------------- *)

let find_layer db ~name ~version ~os_key =
  let stmt =
    Sqlite3.prepare db
      "SELECT hash, exit_status FROM layers WHERE package_name = ? AND \
       package_ver = ? AND os_key = ? LIMIT 1"
  in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT version));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT os_key));
  let result =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW ->
        Some
          ( Sqlite3.column_text stmt 0,
            Sqlite3.Data.to_int_exn (Sqlite3.column stmt 1) )
    | _ -> None
  in
  ignore (Sqlite3.finalize stmt);
  result

let find_binary db ~binary ~os_key =
  let results = ref [] in
  let cb row =
    match row with
    | [| name; version; hash |] -> results := (name, version, hash) :: !results
    | _ -> ()
  in
  ignore
    (Sqlite3.exec_not_null_no_headers db ~cb
       (Fmt.str
          "SELECT l.package_name, l.package_ver, l.hash FROM layer_binaries b \
           JOIN layers l ON b.layer_hash = l.hash WHERE b.binary_name = %s AND \
           l.os_key = %s AND l.exit_status = 0"
          (quote binary) (quote os_key)));
  (* Sort by opam version descending — latest version first *)
  List.sort
    (fun (_, v1, _) (_, v2, _) ->
      OpamPackage.Version.compare
        (OpamPackage.Version.of_string v2)
        (OpamPackage.Version.of_string v1))
    (List.rev !results)

let search_binary db ~pattern ~os_key =
  let results = ref [] in
  let some_if_set s = if s = "" then None else Some s in
  let cb row =
    match row with
    | [| binary; name; version; hash; oh; ov |] ->
        let overlay =
          match (some_if_set oh, some_if_set ov) with
          | Some h, Some v -> Some (h, v)
          | _ -> None
        in
        results := (binary, name, version, hash, overlay) :: !results
    | _ -> ()
  in
  (* Convert user wildcards: * → % *)
  let sql_pattern = String.map (fun c -> if c = '*' then '%' else c) pattern in
  let op = if String.contains sql_pattern '%' then "LIKE" else "=" in
  (* COALESCE the nullable overlay columns so [exec_not_null_no_headers]
     (which skips rows containing NULLs) still sees every match. *)
  ignore
    (Sqlite3.exec_not_null_no_headers db ~cb
       (Fmt.str
          "SELECT b.binary_name, l.package_name, l.package_ver, l.hash, \
           COALESCE(l.overlay_handle, ''), COALESCE(l.overlay_version, '') \
           FROM layer_binaries b JOIN layers l ON b.layer_hash = l.hash WHERE \
           b.binary_name %s %s AND l.os_key = %s AND l.exit_status = 0"
          op (quote sql_pattern) (quote os_key)));
  List.sort
    (fun (b1, _, v1, _, _) (b2, _, v2, _, _) ->
      let c = String.compare b1 b2 in
      if c <> 0 then c
      else
        OpamPackage.Version.compare
          (OpamPackage.Version.of_string v2)
          (OpamPackage.Version.of_string v1))
    (List.rev !results)

let deps db ~hash =
  let results = ref [] in
  let cb row =
    match row with
    | [| name; version; dep_hash |] ->
        results := (name, version, dep_hash) :: !results
    | _ -> ()
  in
  ignore
    (Sqlite3.exec_not_null_no_headers db ~cb
       (Fmt.str
          "SELECT dep_name, dep_version, dep_hash FROM layer_deps WHERE \
           layer_hash = %s ORDER BY dep_name"
          (quote hash)));
  List.rev !results

let files db ~hash =
  let results = ref [] in
  let cb row =
    match row with [| path |] -> results := path :: !results | _ -> ()
  in
  ignore
    (Sqlite3.exec_not_null_no_headers db ~cb
       (Fmt.str
          "SELECT path FROM layer_files WHERE layer_hash = %s ORDER BY path"
          (quote hash)));
  List.rev !results

let all_layers db ~os_key =
  let results = ref [] in
  let cb row =
    match row with
    | [| hash; name; version; status |] ->
        results := (hash, name, version, int_of_string status) :: !results
    | _ -> ()
  in
  ignore
    (Sqlite3.exec_not_null_no_headers db ~cb
       (Fmt.str
          "SELECT hash, package_name, package_ver, exit_status FROM layers \
           WHERE os_key = %s ORDER BY package_name, package_ver"
          (quote os_key)));
  List.rev !results

let all_binaries db ~os_key =
  let results = ref [] in
  let cb row =
    match row with
    | [| binary; name; version |] ->
        results := (binary, name, version) :: !results
    | _ -> ()
  in
  ignore
    (Sqlite3.exec_not_null_no_headers db ~cb
       (Fmt.str
          "SELECT b.binary_name, l.package_name, l.package_ver FROM \
           layer_binaries b JOIN layers l ON b.layer_hash = l.hash WHERE \
           l.os_key = %s AND l.exit_status = 0 ORDER BY b.binary_name"
          (quote os_key)));
  List.rev !results

(* -- Stats ---------------------------------------------------------------- *)

let stats db ~os_key =
  let get_count sql =
    let stmt = Sqlite3.prepare db sql in
    ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT os_key));
    let n =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Sqlite3.Data.to_int_exn (Sqlite3.column stmt 0)
      | _ -> 0
    in
    ignore (Sqlite3.finalize stmt);
    n
  in
  let n_layers = get_count "SELECT COUNT(*) FROM layers WHERE os_key = ?" in
  let n_binaries =
    get_count
      "SELECT COUNT(*) FROM layer_binaries b JOIN layers l ON b.layer_hash = \
       l.hash WHERE l.os_key = ?"
  in
  let n_files =
    get_count
      "SELECT COUNT(*) FROM layer_files f JOIN layers l ON f.layer_hash = \
       l.hash WHERE l.os_key = ?"
  in
  (n_layers, n_binaries, n_files)

(* -- Remote merge --------------------------------------------------------- *)

let merge_remote db ~remote_path =
  exec db (Fmt.str "ATTACH DATABASE %s AS remote" (quote remote_path));
  (* Bring older remote index snapshots up to the current schema so the
     column-position-sensitive [SELECT *] below keeps working. The
     remote_path is a locally-downloaded copy; migrating it is safe. *)
  try_add_column db "ALTER TABLE remote.layers ADD COLUMN overlay_handle TEXT";
  try_add_column db "ALTER TABLE remote.layers ADD COLUMN overlay_version TEXT";
  exec db
    "CREATE TEMP TABLE _new_hashes AS SELECT hash FROM remote.layers WHERE \
     hash NOT IN (SELECT hash FROM main.layers)";
  exec db
    "INSERT INTO layers SELECT * FROM remote.layers WHERE hash IN (SELECT hash \
     FROM _new_hashes)";
  exec db
    "INSERT INTO layer_deps SELECT * FROM remote.layer_deps WHERE layer_hash \
     IN (SELECT hash FROM _new_hashes)";
  exec db
    "INSERT INTO layer_binaries SELECT * FROM remote.layer_binaries WHERE \
     layer_hash IN (SELECT hash FROM _new_hashes)";
  exec db
    "INSERT INTO layer_files SELECT * FROM remote.layer_files WHERE layer_hash \
     IN (SELECT hash FROM _new_hashes)";
  exec db "DROP TABLE _new_hashes";
  exec db "DETACH DATABASE remote"
