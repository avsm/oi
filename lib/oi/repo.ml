[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
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

let needs_update ~dir ~max_age_seconds =
  let fetch_head = dir / ".git" / "FETCH_HEAD" in
  try
    let st = Unix.stat fetch_head in
    Unix.gettimeofday () -. st.Unix.st_mtime > max_age_seconds
  with Unix.Unix_error _ -> true

let one_day = 86400.0

(* -- Ensure repos are cloned and fresh ----------------------------------- *)

let ensure ~data_dir =
  List.iter
    (fun r ->
      let dir = repo_dir ~data_dir r.name in
      let pkg_dir = dir / "packages" in
      if not (Sys.file_exists pkg_dir) then begin
        Log.info (fun m -> m "Cloning %s from %s..." r.name r.url);
        pull_repo ~label:r.name ~url_s:r.url ~dst:dir
      end
      else if needs_update ~dir ~max_age_seconds:one_day then begin
        Log.info (fun m -> m "Updating %s..." r.name);
        try pull_repo ~label:r.name ~url_s:r.url ~dst:dir
        with exn ->
          Log.warn (fun m ->
              m "Failed to update %s: %s" r.name (Printexc.to_string exn))
      end)
    config.remotes

let ensure_extra ~data_dir urls =
  List.filter_map
    (fun url_s ->
      let hash = Digest.string url_s |> Digest.to_hex in
      let name = "extra-" ^ String.sub hash 0 8 in
      let dir = data_dir / "repos" / name in
      let pkg_dir = dir / "packages" in
      if not (Sys.file_exists pkg_dir) then begin
        Log.info (fun m -> m "Cloning extra repo %s..." url_s);
        pull_repo ~label:name ~url_s ~dst:dir
      end
      else if needs_update ~dir ~max_age_seconds:one_day then begin
        Log.info (fun m -> m "Updating extra repo %s..." name);
        try pull_repo ~label:name ~url_s ~dst:dir with _ -> ()
      end;
      if Sys.file_exists pkg_dir then Some pkg_dir else None)
    urls

(* -- Pretty-printing ----------------------------------------------------- *)

let pp_config fmt c =
  Fmt.pf fmt "@[<v>";
  List.iter
    (fun r ->
      let marker = if r.name = c.default then "* " else "  " in
      Fmt.pf fmt "%s%a %s@," marker Fmt.(styled `Bold string) r.name r.url)
    c.remotes;
  Fmt.pf fmt "@]"
