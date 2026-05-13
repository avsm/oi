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
    List.iter
      (fun f ->
        if Filename.check_suffix f ".tar.zst" then
          let hash = Filename.chop_suffix f ".tar.zst" in
          let path = os_dir / f in
          try
            let sha256 =
              OpamHash.contents (OpamHash.compute ~kind:`SHA256 path)
            in
            let size = (Unix.stat path).Unix.st_size |> Int64.of_int in
            D10.Index.record_tarball db ~hash ~sha256 ~size
          with Unix.Unix_error _ | Sys_error _ -> ())
      files

(* [Oi.D10ir_archives.publish_all] hardlinks (or copies) every
   [<cache>/d10ir/archives/<sha>.tar.zst] into
   [<output>/d10ir-archives/<sha>.tar.zst]. Mirrors the layout
   {!D10ir.Registry.pull} expects. *)
let export_d10ir_archives = Oi.D10ir_archives.publish_all

let run ~fs ~clock ~sys ~os_key ~cache ~registry ~output =
  let d10 = Oi.Pipeline.d10 ~sys ~fs ~clock ~cache ~os_key in
  let dst = Eio.Path.(fs / output) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dst;
  let n_layers = D10.Layer.export_all d10 ~dst in
  Oi.Say.step "Exported %d layer(s) to %s" n_layers output;
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / output / os_key);
  let index_path = output / os_key / "index.db" in
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
  if registry <> "" then begin
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
  end;
  let s = D10.Index.stats db ~os_key in
  D10.Index.close db;
  finalize_sqlite_for_publish index_path;
  Oi.Say.field "index" "%s: %d layers, %d binaries, %d tarball(s)" os_key
    s.layers s.binaries s.tarballs;
  let { Oi.D10ir_archives.linked; present; missing = _ } =
    export_d10ir_archives ~cache ~output
  in
  let n_d10ir = linked + present in
  if n_d10ir > 0 then
    Oi.Say.field "d10ir-archives"
      "%d archive(s) at %s/d10ir-archives/ (%d new, %d already present)" n_d10ir
      output linked present;
  (* Manifest = Provenance ⨝ Audit. Provenance gives us one entry per
     successfully committed layer with its content fields; the audit log
     gives us a [callers[]] history per layer. Failed-build events that
     have no corresponding layer surface as separate entries. *)
  let provs = Oi.Provenance.read_all ~fs ~cache_root ~os_key in
  let events = Oi.Audit.read_all ~fs ~cache_root ~os_key in
  if provs <> [] || events <> [] then begin
    let manifest =
      Oi.Manifest.from_logs ~os_key ~exported_at:(Unix.gettimeofday ()) provs
        events
    in
    let logs_dir = output / os_key / "logs" in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / logs_dir);
    let path = logs_dir / "manifest.json" in
    (match
       Jsont_bytesrw.encode_string ~format:Jsont.Indent Oi.Manifest.codec
         manifest
     with
    | Ok s ->
        Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / path) s;
        Oi.Say.field "manifest" "%d entry(ies) at %s" manifest.n_packages path
    | Error e -> Log.warn (fun m -> m "manifest encode failed: %s" e));
    if events <> [] then begin
      Oi.Audit.write_per_os ~fs ~output_dir:output ~os_key events;
      Oi.Say.field "audit" "%d event(s) at %s/audit.jsonl" (List.length events)
        (output / os_key)
    end
  end
(* No per-distro [handles/] report. Each export only sees its own
     oskey's slice of the build, so the report would be incomplete and
     [s3cmd put --skip-existing] would pin the first distro's slice in
     S3 forever. The cross-distro view belongs in a server-side process
     that scans the merged bucket. *)
