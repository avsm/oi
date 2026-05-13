let log_src = Logs.Src.create "oi.cmd.layer_index" ~doc:"oi layer index"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat
let remote_index_max_age = 3600.0 (* 1 hour *)

let url_join registry rel =
  let n = String.length registry in
  let stripped =
    if n > 0 && registry.[n - 1] = '/' then String.sub registry 0 (n - 1)
    else registry
  in
  stripped ^ "/" ^ rel

let count_on_disk_layers ~fs ~os_layer_dir =
  let dir_p = Eio.Path.(fs / os_layer_dir) in
  match Eio.Path.read_dir dir_p with
  | exception Eio.Exn.Io _ -> 0
  | entries ->
      List.fold_left
        (fun n name ->
          if Eio.Path.is_directory Eio.Path.(dir_p / name) then n + 1 else n)
        0 entries

let ensure_local ~sys ~fs ~clock ~cache ~os_key =
  let layers_dir = Oi.Cache.root_s cache / "layers" / os_key in
  let index_path = layers_dir / "index.db" in
  let d10 : D10.Config.t =
    { sys; fs; clock; root = Oi.Cache.root cache; os_key }
  in
  let cache_root = Oi.Cache.root_s cache in
  let overlay_for ~hash =
    Oi.Provenance.overlay_of_layer ~fs ~cache_root ~os_key ~hash
  in
  let rebuild reason =
    Log.info (fun m -> m "%s local index for %s" reason os_key);
    let db = D10.Index.open_ ~fs ~path:index_path in
    D10.Index.rebuild d10 ~overlay_for db;
    D10.Index.close db
  in
  if not (Eio.Path.is_file Eio.Path.(fs / index_path)) then rebuild "Building"
  else begin
    let disk = count_on_disk_layers ~fs ~os_layer_dir:layers_dir in
    let db = D10.Index.open_ ~fs ~path:index_path in
    let s = D10.Index.stats db ~os_key in
    let stamp = D10.Index.indexer_stamp db ~os_key in
    D10.Index.close db;
    if stamp <> Some D10.Index.indexer_version then
      Fmt.kstr rebuild "Refreshing (indexer %s, on-disk %s)"
        D10.Index.indexer_version
        (Stdlib.Option.value stamp ~default:"unstamped")
    else if disk > s.layers then
      Fmt.kstr rebuild "Refreshing (%d on-disk vs %d indexed)" disk s.layers
  end;
  index_path

let ensure_remote ?on_phase ~sys ~fs ~cache ~os_key ~registry () =
  if registry = "" then None
  else
    let cache_root = Oi.Cache.root_s cache in
    let os_dir = cache_root / "layers" / os_key in
    let local_path = os_dir / "remote-index.db" in
    let tmp_path = local_path ^ ".tmp" in
    let fresh =
      try
        let st = Unix.stat local_path in
        Unix.gettimeofday () -. st.Unix.st_mtime < remote_index_max_age
      with Unix.Unix_error _ -> false
    in
    if fresh then Some local_path
    else begin
      let url = url_join registry (os_key / "index.db") in
      let dst = Eio.Path.(fs / tmp_path) in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / os_dir);
      (try Unix.unlink tmp_path with Unix.Unix_error _ -> ());
      (* Visible status goes through [on_phase] so [oi run] can wire it
         into its preflight bar; otherwise it's a quiet [Log.info]. The
         previous [Log.app] leaked the message to stderr unconditionally,
         polluting [oi run]'s output. *)
      (match on_phase with
      | Some f -> f "Fetching registry index"
      | None ->
          Log.info (fun m ->
              m "Fetching registry index from %s (this may take a moment)..."
                url));
      let fmt_size n =
        if Int64.compare n 1_048_576L >= 0 then
          Fmt.str "%.1fMB" (Int64.to_float n /. 1_048_576.)
        else if Int64.compare n 1024L >= 0 then
          Fmt.str "%.0fKB" (Int64.to_float n /. 1024.)
        else Fmt.str "%LdB" n
      in
      let on_progress =
        Option.map
          (fun f ~received ~total ->
            match total with
            | Some t when Int64.compare t 0L > 0 ->
                Fmt.kstr f "Fetching registry index (%s / %s)"
                  (fmt_size received) (fmt_size t)
            | _ -> Fmt.kstr f "Fetching registry index (%s)" (fmt_size received))
          on_phase
      in
      if D10.Sysops.Http.fetch ?on_progress sys ~url ~dst then begin
        (try Unix.rename tmp_path local_path
         with Unix.Unix_error _ -> (
           try Unix.unlink tmp_path with Unix.Unix_error _ -> ()));
        Some local_path
      end
      else begin
        (try Unix.unlink tmp_path with Unix.Unix_error _ -> ());
        if Eio.Path.is_file Eio.Path.(fs / local_path) then begin
          Log.warn (fun m ->
              m "Failed to fetch registry index, using stale cache");
          Some local_path
        end
        else begin
          Log.warn (fun m ->
              m "Failed to fetch registry index from %s" registry);
          None
        end
      end
    end

let merge_remote_into_local ~fs ~index_path ~remote_path =
  let db = D10.Index.open_ ~fs ~path:index_path in
  (try D10.Index.merge_remote db ~remote_path
   with Failure msg -> (
     Log.warn (fun m ->
         m
           "Remote index merge failed (%s); removing %s so the next run \
            re-downloads it"
           msg remote_path);
     try Sys.remove remote_path with Sys_error _ -> ()));
  D10.Index.close db

let package_of_binary ?on_phase ~sys ~fs ~clock ~cache ~os_key ~registry name =
  let clk = (clock :> D10.Config.clk) in
  let index_path = ensure_local ~sys ~fs ~clock:clk ~cache ~os_key in
  (match ensure_remote ?on_phase ~sys ~fs ~cache ~os_key ~registry () with
  | Some remote_path -> merge_remote_into_local ~fs ~index_path ~remote_path
  | None -> ());
  if not (Eio.Path.is_file Eio.Path.(fs / index_path)) then None
  else
    let db = D10.Index.open_ ~fs ~path:index_path in
    let results = D10.Index.binaries_for db ~binary:name ~os_key in
    D10.Index.close db;
    match results with
    | (pkg, _, _, overlay) :: _ ->
        let handle = Option.map (fun (o : D10.Overlay.t) -> o.handle) overlay in
        Some (pkg, handle)
    | [] -> None
