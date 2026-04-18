[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Opam repository management using opam's repository libraries.

    Handles git, HTTP (index.tar.gz), and other backends via
    {!OpamRepository.pull_tree}. *)

let log_src = Logs.Src.create "oi.repo"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

type remote = { name : string; url : string }
type config = { remotes : remote list; default : string }

let remotes =
  [
    {
      name = "relocatable";
      url = "https://github.com/dra27/opam-repository.git#relocatable";
    };
    { name = "default"; url = "https://github.com/ocaml/opam-repository.git" };
  ]

let config = { remotes; default = "relocatable" }
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

let ordered_remotes c =
  let default_remote = List.filter (fun r -> r.name = c.default) c.remotes in
  let rest = List.filter (fun r -> r.name <> c.default) c.remotes in
  default_remote @ rest

let packages_dirs ~data_dir =
  List.filter_map
    (fun r ->
      let dir = repo_dir ~data_dir r.name / "packages" in
      if Sys.file_exists dir then Some dir else None)
    (ordered_remotes config)

(* -- Freshness check ----------------------------------------------------- *)

(* [true] when the directory is older than [refresh_max_age], or cannot be
   stat'd (treated as missing/stale). *)
let dir_needs_refresh dir =
  try
    let st = Unix.stat dir in
    Unix.time () -. st.Unix.st_mtime > refresh_max_age
  with Unix.Unix_error _ -> true

(* -- Ensure repos are cloned and fresh ----------------------------------- *)

(* Common clone/pull logic shared by [ensure] and [ensure_extra]. *)
let ensure_one ~refresh ~label ~url ~dir =
  let pkg_dir = dir / "packages" in
  if not (Sys.file_exists pkg_dir) then begin
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

let ensure ~data_dir ?(refresh = false) () =
  List.iter
    (fun r ->
      let dir = repo_dir ~data_dir r.name in
      ensure_one ~refresh ~label:r.name ~url:r.url ~dir)
    config.remotes

let ensure_extra ~data_dir ?(refresh = false) extras =
  List.map
    (fun (e : Project.extra_repo) ->
      let dir = repo_dir ~data_dir e.name in
      ensure_one ~refresh ~label:e.name ~url:e.url ~dir;
      dir / "packages")
    extras

(* -- Pretty-printing ----------------------------------------------------- *)

let pp_config fmt c =
  Fmt.pf fmt "@[<v>";
  List.iter
    (fun r ->
      let marker = if r.name = c.default then "* " else "  " in
      Fmt.pf fmt "%s%a %s@," marker Fmt.(styled `Bold string) r.name r.url)
    c.remotes;
  Fmt.pf fmt "@]"
