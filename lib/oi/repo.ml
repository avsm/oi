[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Opam repository management using opam's repository libraries.

    Handles git, HTTP (index.tar.gz), and other backends via
    {!OpamRepository.pull_tree}. *)

let log_src = Logs.Src.create "oi.repo"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

let refresh_max_age = 86_400.0

(* -- Repo pull using opam libraries -------------------------------------- *)

let pull_repo ~label ~url_s ~dst =
  let url = OpamUrl.parse ~handle_suffix:true url_s in
  let dst_dir = OpamFilename.Dir.of_string dst in
  OpamFilename.mkdir dst_dir;
  let repo_name = OpamRepositoryName.of_string label in
  let module B =
    (val OpamRepository.find_backend_by_kind url.OpamUrl.backend
        : OpamRepositoryBackend.S)
  in
  let result =
    B.fetch_repo_update repo_name dst_dir url |> OpamProcess.Job.run
  in
  match result with
  | OpamRepositoryBackend.Update_full tmp_dir ->
      (* Full fetch: move contents from temp dir to destination *)
      if
        OpamFilename.Dir.to_string tmp_dir <> OpamFilename.Dir.to_string dst_dir
      then begin
        OpamFilename.rmdir dst_dir;
        OpamFilename.move_dir ~src:tmp_dir ~dst:dst_dir
      end
  | OpamRepositoryBackend.Update_patch _ ->
      (* Incremental update: already applied by the backend *)
      B.repo_update_complete dst_dir url |> OpamProcess.Job.run
  | OpamRepositoryBackend.Update_empty -> ()
  | OpamRepositoryBackend.Update_err exn ->
      Fmt.failwith "Failed to fetch repo %s: %s" label (Printexc.to_string exn)

(* Bump the mtime of [dir] to "now" after a successful pull, so the age
   check in [dir_needs_refresh] sees a fresh timestamp. *)
let touch_dir dir =
  try
    let now = Unix.time () in
    Unix.utimes dir now now
  with Unix.Unix_error _ -> ()

(* -- Repo dirs ----------------------------------------------------------- *)

let repo_dir ~data_dir name = data_dir / "repos" / name

(* -- Freshness check ----------------------------------------------------- *)

(* [true] when the directory is older than [refresh_max_age], or cannot be
   stat'd (treated as missing/stale). *)
let dir_needs_refresh dir =
  try
    let st = Unix.stat dir in
    Unix.time () -. st.Unix.st_mtime > refresh_max_age
  with Unix.Unix_error _ -> true

(* -- Ensure repos are cloned and fresh ----------------------------------- *)

(* Recursive rmdir that doesn't care whether anything is actually
   there; used to clear stale clones before re-pulling. *)
let rec rmtree path =
  try
    if Sys.is_directory path then begin
      Sys.readdir path |> Array.iter (fun n -> rmtree (path / n));
      try Unix.rmdir path with Unix.Unix_error _ -> ()
    end
    else try Unix.unlink path with Unix.Unix_error _ -> ()
  with Sys_error _ -> ()

(* Common clone/pull logic shared by [ensure] and [ensure_extra]. *)
let ensure_one ~refresh ~label ~url ~dir =
  let pkg_dir = dir / "packages" in
  if not (Sys.file_exists pkg_dir) then begin
    (* Nothing useful in [dir] even if it exists — a previous clone
       that either crashed mid-fetch or targeted a different URL.
       Nuke it so the subsequent pull runs against a clean target;
       opam's git backend otherwise tries an incremental update over
       an unrelated tree and silently keeps the stale content. *)
    if Sys.file_exists dir then begin
      Log.info (fun m ->
          m "Re-cloning %s (existing clone at %s has no packages/)" label dir);
      rmtree dir
    end;
    Log.info (fun m -> m "Cloning %s from %s..." label url);
    pull_repo ~label ~url_s:url ~dst:dir;
    touch_dir dir
  end
  else if refresh || dir_needs_refresh dir then begin
    Log.info (fun m -> m "Updating %s..." label);
    try
      pull_repo ~label ~url_s:url ~dst:dir;
      touch_dir dir
    with exn ->
      Log.warn (fun m ->
          m "Failed to update %s: %s" label (Printexc.to_string exn))
  end

let ensure_extra ~data_dir ?(refresh = false) extras =
  List.map
    (fun (e : Project.extra_repo) ->
      let dir = repo_dir ~data_dir e.name in
      ensure_one ~refresh ~label:e.name ~url:e.url ~dir;
      dir / "packages")
    extras

