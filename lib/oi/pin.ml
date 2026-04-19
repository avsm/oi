[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Pin-depends realisation. *)

let log_src = Logs.Src.create "oi.pin"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

(* -- Fetching ------------------------------------------------------------ *)

(* Digest of the URL string. Stable per symbolic URL; for git URLs this is
   complemented later by the resolved HEAD commit. *)
let url_key (url : OpamUrl.t) =
  Digest.to_hex (Digest.string (OpamUrl.to_string url))

(* Sentinel that marks a src_dir as successfully populated. Guarding on
   this (not on the existence of [src_dir] itself) prevents a partial
   [pull_tree] from poisoning the cache: if the pull fails we leave the
   (possibly partial) directory alone, the sentinel is absent, and the
   next run re-enters the fetch branch below and wipes+retries. *)
let sentinel_path src_dir = src_dir / ".oi-pin-ok"

let fetch_pin ~fs ~cache ~refresh (pin : Project.pin) =
  let root = Cache.pins_dir cache in
  let src_dir = root / "sources" / url_key pin.url in
  let sentinel = sentinel_path src_dir in
  (* [--refresh] forces a re-fetch unconditionally, which is how users
     pick up new upstream commits on a git pin between age-based
     refreshes. *)
  if Repo.cache_fresh ~refresh ~sentinel ~max_age:Repo.refresh_max_age then
    src_dir
  else begin
    Log.info (fun m ->
        m "Fetching pin %s from %s"
          (OpamPackage.to_string pin.pkg)
          (OpamUrl.to_string pin.url));
    (* Clean slate: a partial prior fetch may have left junk behind.
       We didn't see a sentinel, so whatever is there is untrusted. *)
    if Sys.file_exists src_dir then
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / src_dir);
    let dst = OpamFilename.Dir.of_string src_dir in
    OpamFilename.mkdir dst;
    let cache_dir =
      OpamRepositoryPath.download_cache OpamStateConfig.(!r.root_dir)
    in
    let result =
      OpamRepository.pull_tree
        (OpamPackage.to_string pin.pkg)
        ~cache_dir dst [] [ pin.url ]
      |> OpamProcess.Job.run
    in
    match result with
    | OpamTypes.Result _ | OpamTypes.Up_to_date _ ->
        (* Touch sentinel via Eio so we don't regress on Unix.*. *)
        Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / sentinel) "";
        src_dir
    | OpamTypes.Not_available (_, msg) ->
        Error.config_error "pin %s: fetch failed (%s): %s"
          (OpamPackage.to_string pin.pkg)
          (OpamUrl.to_string pin.url)
          msg
  end

(* -- Resolved revision --------------------------------------------------- *)

(* For git URLs, resolve HEAD inside the fetched source tree. For
   anything else, just hash the URL string — tarball URLs that want
   stable identity should pin a checksum in the URL (e.g. #sha=...)
   or use a distinct URL per version. *)
let resolved_revision ~sys (pin : Project.pin) ~src_dir =
  match pin.url.OpamUrl.backend with
  | `git -> (
      try
        D10.Sysops.Cmd.run_out sys [ "git"; "-C"; src_dir; "rev-parse"; "HEAD" ]
      with Failure _ -> url_key pin.url)
  | _ -> url_key pin.url

(* -- Locate .opam file inside fetched tree ------------------------------- *)

(* Per opam convention, a package named <name> ships its opam file as
   either <name>.opam at the repo root or plain "opam". The pinned
   package's name is what counts — we ignore any stray other-name
   .opam files in a monorepo. *)
let locate_opam_file ~src_dir (pin : Project.pin) =
  let name = OpamPackage.Name.to_string (OpamPackage.name pin.pkg) in
  let candidates = [ src_dir / (name ^ ".opam"); src_dir / "opam" ] in
  match List.find_opt Sys.file_exists candidates with
  | Some p -> p
  | None ->
      Error.config_error "pin %s: no %s.opam or opam file at the root of %s"
        (OpamPackage.to_string pin.pkg)
        name src_dir

(* -- Rewrite opam file's url: to point at local source ------------------- *)

let rewrite_opam ~src_dir ~opam_path ~revision (pin : Project.pin) =
  let opam =
    try OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw opam_path))
    with exn ->
      Error.config_error "pin: failed to read %s: %s" opam_path
        (Printexc.to_string exn)
  in
  (* Append the resolved revision as a URL fragment so the serialized
     opam file changes whenever the upstream revision does. This is
     picked up by [OpamFile.OPAM.effective_part], which in turn feeds
     the D10 layer hash — so a new upstream commit on a git pin
     invalidates the layer cache for that pin (and its reverse deps). *)
  let local_url = OpamUrl.of_string (Fmt.str "file://%s#%s" src_dir revision) in
  let new_url = OpamFile.URL.create local_url in
  opam
  |> OpamFile.OPAM.with_url new_url
  (* Coerce name/version to match the pin entry — the original opam file
     may omit one or both, or carry values that don't match the pin's
     declared pkg.version. *)
  |> OpamFile.OPAM.with_name (OpamPackage.name pin.pkg)
  |> OpamFile.OPAM.with_version (OpamPackage.version pin.pkg)

(* -- Write synthesized packages tree ------------------------------------- *)

let write_repo_marker ~fs ~dir =
  let repo_path = dir / "repo" in
  let path = Eio.Path.(fs / repo_path) in
  Eio.Path.save ~create:(`If_missing 0o644) path "opam-version: \"2.0\"\n"

let write_pin_opam ~packages_dir (pin : Project.pin) opam =
  let name = OpamPackage.Name.to_string (OpamPackage.name pin.pkg) in
  let pkg_s = OpamPackage.to_string pin.pkg in
  let dir = packages_dir / name / pkg_s in
  OpamFilename.mkdir (OpamFilename.Dir.of_string dir);
  let target = dir / "opam" in
  OpamFile.OPAM.write (OpamFile.make (OpamFilename.raw target)) opam

(* -- Public entry point -------------------------------------------------- *)

type resolved = { pin : Project.pin; opam : OpamFile.OPAM.t; revision : string }

let materialize ~fs ~sys ~cache ?(refresh = false) pins =
  match pins with
  | [] -> None
  | _ ->
      (* Phase 1: fetch each pin, locate+rewrite its opam, compute rev.
         Revision is resolved *before* rewriting the opam so the rewritten
         [url:] carries the concrete commit/hash in its fragment. *)
      let resolved =
        List.map
          (fun (pin : Project.pin) ->
            let src_dir = fetch_pin ~fs ~cache ~refresh pin in
            let opam_path = locate_opam_file ~src_dir pin in
            let revision = resolved_revision ~sys pin ~src_dir in
            let opam = rewrite_opam ~src_dir ~opam_path ~revision pin in
            { pin; opam; revision })
          pins
      in
      (* Phase 2: set hash from sorted (name, version, revision) triples. *)
      let triples =
        List.map
          (fun r ->
            let name =
              OpamPackage.Name.to_string (OpamPackage.name r.pin.pkg)
            in
            let version = OpamPackage.version_to_string r.pin.pkg in
            (name, version, r.revision))
          resolved
        |> List.sort compare
      in
      let set_hash =
        let s =
          String.concat "\n"
            (List.map
               (fun (n, v, r) -> Printf.sprintf "%s\t%s\t%s" n v r)
               triples)
        in
        Digest.to_hex (Digest.string s)
      in
      (* Phase 3: materialize packages/ tree under sets/<set_hash>/. *)
      let root = Cache.pins_dir cache in
      let set_root = root / "sets" / set_hash in
      let packages_dir = set_root / "packages" in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / packages_dir);
      write_repo_marker ~fs ~dir:set_root;
      List.iter (fun r -> write_pin_opam ~packages_dir r.pin r.opam) resolved;
      Log.info (fun m ->
          m "Materialized %d pin(s) at %s" (List.length pins) packages_dir);
      Some packages_dir
