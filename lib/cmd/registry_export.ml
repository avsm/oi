let log_src =
  Logs.Src.create "oi.cmd.registry_export" ~doc:"oi registry export command"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

(* Sqlite in WAL mode leaves [<path>-wal] and [<path>-shm] sidecars
   next to the main [.db] on close. Plain [Sys.remove path] doesn't
   touch them. *)
let unlink_sqlite_sidecars path =
  List.iter
    (fun s -> try Sys.remove (path ^ s) with Sys_error _ -> ())
    [ "-wal"; "-shm"; "-journal" ]

let remove_sqlite_scratch path =
  unlink_sqlite_sidecars path;
  try Sys.remove path with Sys_error _ -> ()

(* Collapse any WAL/SHM sidecars into [path] so the published index.db
   is self-contained. *)
let finalize_sqlite_for_publish path =
  if Sys.file_exists path then begin
    (try
       let db = Sqlite3.db_open path in
       Fun.protect
         ~finally:(fun () -> ignore (Sqlite3.db_close db))
         (fun () -> ignore (Sqlite3.exec db "PRAGMA journal_mode=DELETE"))
     with Sys_error _ -> ());
    unlink_sqlite_sidecars path
  end

let fetch_remote_to ~sys ~fs ~registry ~rel ~dst =
  if registry = "" then false
  else begin
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
      Eio.Path.(fs / Filename.dirname dst);
    D10.Sysops.Http.fetch sys
      ~url:(Layer_index.url_join registry rel)
      ~dst:Eio.Path.(fs / dst)
  end

let record_one_tarball db ~os_dir f =
  if not (Filename.check_suffix f ".tar.zst") then ()
  else
    let hash = Filename.chop_suffix f ".tar.zst" in
    let path = os_dir / f in
    try
      let sha256 = OpamHash.contents (OpamHash.compute ~kind:`SHA256 path) in
      let size = (Unix.stat path).Unix.st_size |> Int64.of_int in
      D10.Index.record_tarball db ~hash ~sha256 ~size
    with Unix.Unix_error _ | Sys_error _ -> ()

(* Sha-256 + byte size of every published [.tar.zst], recorded into
   [layers.tarball_sha256] / [layers.tarball_size] so a remote client
   can probe [Index.all_tarballs] (replacing the old OINDEX.txt
   sidecar). *)
let record_tarballs db ~output ~os_key =
  let os_dir = output / os_key in
  if not (Sys.file_exists os_dir) then ()
  else
    let files =
      try Sys.readdir os_dir |> Array.to_list with Sys_error _ -> []
    in
    List.iter (record_one_tarball db ~os_dir) files

(* [Oi.D10ir_archives.publish_all] hardlinks (or copies) every
   [<cache>/d10ir/archives/<sha>.tar.zst] into
   [<output>/d10ir-archives/<sha>.tar.zst]. Mirrors the layout
   {!D10ir.Registry.pull} expects. *)
let export_d10ir_archives = Oi.D10ir_archives.publish_all

let merge_remote_into ~sys ~fs ~registry ~output ~os_key db =
  let scratch = output / os_key / ".remote-index.db" in
  if
    fetch_remote_to ~sys ~fs ~registry ~rel:(os_key / "index.db") ~dst:scratch
  then begin
    (try D10.Index.merge_remote db ~remote_path:scratch
     with Failure msg ->
       Log.warn (fun m -> m "Failed to merge remote layer index: %s" msg));
    remove_sqlite_scratch scratch
  end
  else
    Log.info (fun m ->
        m "No remote layer index at %s/%s/index.db (skipping merge)" registry
          os_key)

let build_index ~fs ~sys ~cache ~os_key ~registry ~output ~d10 ~index_path =
  (try Sys.remove index_path with Sys_error _ -> ());
  let db = D10.Index.open_ ~fs ~path:index_path in
  let cache_root = Oi.Cache.root_s cache in
  let overlay_for ~hash =
    Oi.Provenance.overlay_of_layer ~fs ~cache_root ~os_key ~hash
  in
  (* [include_files = false]: per-file paths were 88% of index.db
     bytes on a typical export; [layer.json] inside each layer
     already lists what it ships and [oi search] / [find_binary] /
     [find_meta] don't need [layer_files]. *)
  D10.Index.rebuild d10 ~overlay_for ~include_files:false db;
  record_tarballs db ~output ~os_key;
  if registry <> "" then merge_remote_into ~sys ~fs ~registry ~output ~os_key db;
  let s = D10.Index.stats db ~os_key in
  D10.Index.close db;
  finalize_sqlite_for_publish index_path;
  s

let publish_d10ir_archives_summary ~cache ~output =
  let { Oi.D10ir_archives.linked; present; missing = _ } =
    export_d10ir_archives ~cache ~output
  in
  let n_d10ir = linked + present in
  if n_d10ir > 0 then
    Oi.Say.field "d10ir-archives"
      "%d archive(s) at %s/d10ir-archives/ (%d new, %d already present)" n_d10ir
      output linked present

let write_manifest_file ~fs ~output ~os_key manifest =
  let logs_dir = output / os_key / "logs" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / logs_dir);
  let path = logs_dir / "manifest.json" in
  match
    Jsont_bytesrw.encode_string ~format:Jsont.Indent Oi.Manifest.codec manifest
  with
  | Ok s ->
      Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / path) s;
      Oi.Say.field "manifest" "%d entry(ies) at %s" manifest.n_packages path
  | Error e -> Log.warn (fun m -> m "manifest encode failed: %s" e)

let copy_file ~fs ~src ~dst =
  let s = Eio.Path.load Eio.Path.(fs / src) in
  let parent = Filename.dirname dst in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / parent);
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / dst) s

let copy_builds_subtree ~fs ~cache_root ~output ~os_key =
  let src_root =
    Filename.concat cache_root
      (Filename.concat "layers" (Filename.concat os_key "builds"))
  in
  let dst_root =
    Filename.concat output (Filename.concat os_key "builds")
  in
  if not (Sys.file_exists src_root) then 0
  else
    let n = ref 0 in
    let rec walk rel =
      let abs_src = if rel = "" then src_root else src_root / rel in
      if Sys.is_directory abs_src then
        Array.iter
          (fun name ->
            let rel' = if rel = "" then name else rel / name in
            walk rel')
          (try Sys.readdir abs_src with Sys_error _ -> [||])
      else if Filename.check_suffix abs_src ".json" then begin
        let dst = dst_root / rel in
        copy_file ~fs ~src:abs_src ~dst;
        incr n
      end
    in
    walk "";
    !n

(* Copy [<cache>/layers/<os_key>/*.json] (the new layer manifests,
   stored flat next to each [<hash>/] dir) into
   [<output>/<os_key>/*.json] — alongside each
   [<hash>.tar.zst] that [D10.Layer.export_all] writes. *)
let copy_layer_manifests ~fs ~cache_root ~output ~os_key =
  let src_root =
    Filename.concat cache_root (Filename.concat "layers" os_key)
  in
  let dst_root = Filename.concat output os_key in
  if not (Sys.file_exists src_root) then 0
  else
    let n = ref 0 in
    let entries =
      try Sys.readdir src_root |> Array.to_list with Sys_error _ -> []
    in
    List.iter
      (fun name ->
        (* Top-level [registry.json] is copied by its own helper; skip
           the [builds/] subdir + the hash-named layer dirs; pick up
           only [<hash>.json] sidecars. *)
        if
          Filename.check_suffix name ".json"
          && name <> "registry.json"
        then begin
          let abs = src_root / name in
          if (try (Unix.lstat abs).st_kind = Unix.S_REG with _ -> false) then begin
            copy_file ~fs ~src:abs ~dst:(dst_root / name);
            incr n
          end
        end)
      entries;
    !n

(* Copy [<cache>/layers/<os_key>/registry.json] (the schema-version
   pointer) into [<output>/<os_key>/registry.json]. *)
let copy_registry_pointer ~fs ~cache_root ~output ~os_key =
  let src =
    Filename.concat cache_root
      (Filename.concat "layers" (Filename.concat os_key "registry.json"))
  in
  if not (Sys.file_exists src) then false
  else
    let dst = Filename.concat output (Filename.concat os_key "registry.json") in
    copy_file ~fs ~src ~dst;
    true

(* Manifest = Provenance ⨝ Audit. Provenance gives us one entry per
   successfully committed layer with its content fields; the build
   manifests under [registry/<os_key>/builds/] supply the [callers[]]
   history. Failed-build events surface either as caller entries or as
   the [outcome] field on the new layer manifest (P2). *)
let write_manifest_and_audit ~fs ~cache_root ~output ~os_key =
  let provs = Oi.Provenance.read_all ~fs ~cache_root ~os_key in
  let events = Oi.Audit.read_all ~fs ~cache_root ~os_key in
  if provs <> [] || events <> [] then begin
    let manifest =
      Oi.Manifest.from_logs ~os_key ~exported_at:(Unix.gettimeofday ()) provs
        events
    in
    write_manifest_file ~fs ~output ~os_key manifest
  end;
  let n_layers = copy_layer_manifests ~fs ~cache_root ~output ~os_key in
  if n_layers > 0 then
    Oi.Say.field "layers.json" "%d layer manifest(s) at %s/<hash>.json"
      n_layers (output / os_key);
  let n_builds = copy_builds_subtree ~fs ~cache_root ~output ~os_key in
  if n_builds > 0 then
    Oi.Say.field "builds" "%d invocation manifest(s) at %s/builds/" n_builds
      (output / os_key);
  if copy_registry_pointer ~fs ~cache_root ~output ~os_key then
    Oi.Say.field "registry" "%s/registry.json" (output / os_key)

let run ~fs ~clock ~sys ~os_key ~cache ~registry ~output =
  let d10 = Oi.Pipeline.d10 ~sys ~fs ~clock ~cache ~os_key in
  let dst = Eio.Path.(fs / output) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dst;
  let n_layers = D10.Layer.export_all d10 ~dst in
  Oi.Say.step "Exported %d layer(s) to %s" n_layers output;
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / output / os_key);
  let index_path = output / os_key / "index.db" in
  let s =
    build_index ~fs ~sys ~cache ~os_key ~registry ~output ~d10 ~index_path
  in
  Oi.Say.field "index" "%s: %d layers, %d binaries, %d tarball(s)" os_key
    s.layers s.binaries s.tarballs;
  publish_d10ir_archives_summary ~cache ~output;
  let cache_root = Oi.Cache.root_s cache in
  write_manifest_and_audit ~fs ~cache_root ~output ~os_key
(* No per-distro [handles/] report. Each export only sees its own
     oskey's slice of the build, so the report would be incomplete and
     [s3cmd put --skip-existing] would pin the first distro's slice in
     S3 forever. The cross-distro view belongs in a server-side process
     that scans the merged bucket. *)
