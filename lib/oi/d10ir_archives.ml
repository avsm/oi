[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(* Publishing helper for the consolidated source archives that live
   under [<cache>/d10ir/archives/<sha>.tar.zst]. Used by:
   - [oi build --export DIR]: ships every locally-baked archive
     alongside the layer tarballs.
   - [oi repo bake [HANDLE] --to=DIR]: bakes a handle's archives
     and ships just those to [DIR/d10ir-archives/].

   Hardlink-first; falls back to a streamed copy when the dst is on
   a different filesystem. The destination path matches what
   {!D10ir.Registry.pull} expects:
     [<base_url>/d10ir-archives/<sha>.tar.zst] *)

let ( / ) = Filename.concat
let local_dir ~cache = Cache.root_s cache / "d10ir" / "archives"
let dst_dir ~output = output / "d10ir-archives"

let copy_stream ic oc =
  let buf = Bytes.create 65536 in
  let rec loop () =
    let n = In_channel.input ic buf 0 (Bytes.length buf) in
    if n > 0 then begin
      Out_channel.output oc buf 0 n;
      loop ()
    end
  in
  loop ()

let copy_file ~src ~dst =
  try
    In_channel.with_open_bin src (fun ic ->
        Out_channel.with_open_bin dst (fun oc -> copy_stream ic oc));
    true
  with Sys_error _ | Unix.Unix_error _ -> false

(* Single-file publish: hardlink, falling back to a streamed copy.
   Already-present (same name) at [dst] short-circuits. *)
let publish_one ~src ~dst =
  if Sys.file_exists dst then false
  else
    try
      Unix.link src dst;
      true
    with Unix.Unix_error _ -> copy_file ~src ~dst

(* List every [<sha>.tar.zst] in [<cache>/d10ir/archives/] with its
   on-disk size. Returns [(sha, size_bytes)] pairs sorted by sha. *)
let entry_of_name ~src name =
  match Filename.chop_suffix_opt ~suffix:".tar.zst" name with
  | None -> None
  | Some sha ->
      let path = src / name in
      let size = try (Unix.stat path).st_size with Unix.Unix_error _ -> 0 in
      Some (sha, size)

let list ~cache =
  let src = local_dir ~cache in
  if not (Sys.file_exists src) then []
  else
    Sys.readdir src |> Array.to_list
    |> List.filter_map (entry_of_name ~src)
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)

(* Result of a publish pass. [linked]: archives newly hardlinked/copied
   into the destination this invocation. [present]: archives already
   there from a prior run. [missing]: shas the caller named (e.g. via a
   reporepo opam's [x-d10-archive]) for which no local cache entry
   exists — those are silently skipped during publish but surfaced here
   so callers can warn. *)
type publish_counts = { linked : int; present : int; missing : int }

let zero_counts = { linked = 0; present = 0; missing = 0 }

let ensure_dst output =
  let dst = dst_dir ~output in
  (try Unix.mkdir dst 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  dst

(* Process [names] under [src]: hardlink-or-copy each existing
   [.tar.zst] into [dst]. Counts linked / already-present / missing
   (named but not in [src]). *)
let publish_files ~src ~dst names =
  List.fold_left
    (fun acc name ->
      let sp = src / name in
      let dp = dst / name in
      if not (Sys.file_exists sp) then { acc with missing = acc.missing + 1 }
      else if Sys.file_exists dp then { acc with present = acc.present + 1 }
      else if publish_one ~src:sp ~dst:dp then
        { acc with linked = acc.linked + 1 }
      else acc)
    zero_counts names

(* Publish every [<sha>.tar.zst] and its sibling [<sha>.json] source
   manifest under [<cache>/d10ir/archives/] into
   [<output>/d10ir-archives/]. Idempotent on repeat invocations. The
   [.json] sidecars are the unit a future indexer walks when stitching
   binary provenance back to upstream source, so they MUST ship
   alongside the tarballs (and unconditionally — see
   [Oi.Source_manifest]). *)
let publish_all ~cache ~output =
  let src = local_dir ~cache in
  if not (Sys.file_exists src) then zero_counts
  else
    let dst = ensure_dst output in
    let entries =
      try
        Sys.readdir src |> Array.to_list
        |> List.filter (fun n ->
               Filename.check_suffix n ".tar.zst"
               || Filename.check_suffix n ".json")
      with Sys_error _ -> []
    in
    publish_files ~src ~dst entries

(* Publish only the [<sha>.tar.zst]s named in [shas]. Used by
   [oi repo bake [HANDLE] --to=DIR] which publishes the archives the
   reporepo overlays reference, leaving stale entries from older runs
   out of [DIR]. *)
let publish_shas ~cache ~output shas =
  if shas = [] then zero_counts
  else
    let src = local_dir ~cache in
    let dst = ensure_dst output in
    publish_files ~src ~dst (List.map (fun sha -> sha ^ ".tar.zst") shas)
