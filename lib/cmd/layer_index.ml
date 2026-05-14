let log_src = Logs.Src.create "oi.cmd.layer_index" ~doc:"oi layer index"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

(* Kept for backwards-compatibility with [oi config]'s display; the
   value no longer drives any cache TTL since the remote index is now
   JSON-fetched on demand and memoised in-process by
   [D10.Remote_index]. *)
let remote_index_max_age = 3600.0

let url_join registry rel =
  let n = String.length registry in
  let stripped =
    if n > 0 && registry.[n - 1] = '/' then String.sub registry 0 (n - 1)
    else registry
  in
  stripped ^ "/" ^ rel

(* -- Local SQLite index ------------------------------------------------ *)

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

(* -- Remote index (index-full.json) ------------------------------------ *)

(* Common preamble: open a [D10.Config.t] pointed at the user's cache
   and call [D10.Remote_index.fetch_full]. Returns [None] when the
   registry doesn't publish an [index-full.json] (or the fetch fails). *)
let load_remote_full ?on_phase ~sys ~fs ~clock ~session ~cache ~os_key ~registry
    () =
  if registry = "" then None
  else begin
    (match on_phase with
    | Some f -> f "Fetching registry binary index"
    | None -> ());
    let cfg : D10.Config.t =
      { sys; fs; clock; root = Oi.Cache.root cache; os_key }
    in
    D10.Remote_index.fetch_full cfg ~session ~remote:(`Http_remote registry)
  end

let overlay_of_layer (l : D10.Remote_index.layer_full) : D10.Overlay.t option =
  match (l.overlay_handle, l.overlay_version) with
  | Some handle, version_opt ->
      Some
        {
          D10.Overlay.handle;
          version = Stdlib.Option.value version_opt ~default:"";
        }
  | None, _ -> None

let layer_matches_binary ~binary (l : D10.Remote_index.layer_full) =
  l.exit_status = 0 && List.exists (String.equal binary) l.binaries

let by_version_desc a b =
  OpamPackage.Version.compare
    (OpamPackage.Version.of_string b)
    (OpamPackage.Version.of_string a)

(* Map a single [layer_full] row to the shape [D10.Index.binaries_for]
   returns. Used to merge local + remote search results. *)
let row_of_layer (l : D10.Remote_index.layer_full) =
  (l.package_name, l.package_ver, l.hash, overlay_of_layer l)

let remote_binaries_for ~binary (full : D10.Remote_index.index_full) =
  full.layers
  |> List.filter (layer_matches_binary ~binary)
  |> List.map row_of_layer
  |> List.sort (fun (_, v1, _, _) (_, v2, _, _) -> by_version_desc v1 v2)

(* Glob-aware predicate: [pattern] uses [*] as the wildcard, matching
   the SQLite [LIKE] path in [D10.Index.search_binary]. Empty pattern
   matches everything. Implementation is the "find each non-wildcard
   chunk in order, left to right" walk; we factor the inner string
   search out so the glob driver stays a shallow loop (E010). *)
let find_substr ~haystack ~needle ~start =
  let hl = String.length haystack in
  let nl = String.length needle in
  let rec loop i =
    if i + nl > hl then None
    else if String.sub haystack i nl = needle then Some i
    else loop (i + 1)
  in
  loop start

let glob_walk ~parts ~s =
  let rec walk pos = function
    | [] -> true
    | [ last ] ->
        let ll = String.length last in
        let sl = String.length s in
        ll <= sl - pos && String.sub s (sl - ll) ll = last
    | "" :: rest -> walk pos rest
    | part :: rest -> (
        match find_substr ~haystack:s ~needle:part ~start:pos with
        | None -> false
        | Some i -> walk (i + String.length part) rest)
  in
  walk 0 parts

let glob_match ~pattern s =
  if pattern = "" || pattern = "*" then true
  else if not (String.contains pattern '*') then String.equal pattern s
  else glob_walk ~parts:(String.split_on_char '*' pattern) ~s

let remote_search_binary ~pattern (full : D10.Remote_index.index_full) =
  full.layers
  |> List.filter (fun (l : D10.Remote_index.layer_full) -> l.exit_status = 0)
  |> List.concat_map (fun (l : D10.Remote_index.layer_full) ->
      l.binaries
      |> List.filter (glob_match ~pattern)
      |> List.map (fun bin ->
          (bin, l.package_name, l.package_ver, l.hash, overlay_of_layer l)))
  |> List.sort (fun (b1, n1, v1, _, _) (b2, n2, v2, _, _) ->
      let c = String.compare b1 b2 in
      if c <> 0 then c
      else
        let c = String.compare n1 n2 in
        if c <> 0 then c else by_version_desc v1 v2)

let remote_search_package ~pattern (full : D10.Remote_index.index_full) =
  full.layers
  |> List.filter (fun (l : D10.Remote_index.layer_full) ->
      l.exit_status = 0 && glob_match ~pattern l.package_name)
  |> List.map row_of_layer
  |> List.sort (fun (n1, v1, _, _) (n2, v2, _, _) ->
      let c = String.compare n1 n2 in
      if c <> 0 then c else by_version_desc v1 v2)

(* -- High-level entry points consumed by [oi run] / [oi search] ------- *)

let local_binaries_for ~sys ~fs ~clock ~cache ~os_key ~binary =
  let index_path = ensure_local ~sys ~fs ~clock ~cache ~os_key in
  if not (Eio.Path.is_file Eio.Path.(fs / index_path)) then []
  else
    let db = D10.Index.open_ ~fs ~path:index_path in
    let r = D10.Index.binaries_for db ~binary ~os_key in
    D10.Index.close db;
    r

let package_of_binary ?on_phase ~sys ~fs ~clock ~session ~cache ~os_key
    ~registry name =
  let clk = (clock :> D10.Config.clk) in
  let first_match rows =
    match rows with
    | (pkg, _, _, overlay) :: _ ->
        let handle =
          Stdlib.Option.map (fun (o : D10.Overlay.t) -> o.handle) overlay
        in
        Some (pkg, handle)
    | [] -> None
  in
  match
    first_match
      (local_binaries_for ~sys ~fs ~clock:clk ~cache ~os_key ~binary:name)
  with
  | Some _ as r -> r
  | None -> (
      match
        load_remote_full ?on_phase ~sys ~fs ~clock:clk ~session ~cache ~os_key
          ~registry ()
      with
      | None -> None
      | Some full -> first_match (remote_binaries_for ~binary:name full))
