[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

let log_src = Logs.Src.create "oi.url_project"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

type t = {
  pins : Project.pin list;
  roots : string list;
  extra_repos : Project.extra_repo list;
  overlays : string list;
}

let empty = { pins = []; roots = []; extra_repos = []; overlays = [] }

type with_arg = Url of string | Dep of Script.dep

(* URL detection first, then fall through to the opam package parser.
   The prefix list stays here (not in {!Script}) because {!Script} is
   a string-parser lib and has no notion of a remote project. *)
let is_url s =
  List.exists
    (fun p -> String.starts_with ~prefix:p s)
    [ "http://"; "https://"; "git+"; "git://"; "git@"; "ssh://" ]

let classify s = if is_url s then Url s else Dep (Script.parse_cli_dep s)

let classify_all tokens =
  List.partition_map
    (fun s ->
      match classify s with Url u -> Left u | Dep d -> Right d)
    tokens

(* Promote a plain http(s) URL to [git+https://…] so [OpamUrl.parse]
   treats it as a git backend. [git+…] / [git://] / [git@…] / [ssh://]
   come through unchanged. *)
let to_opam_url s =
  let s =
    match s with
    | s when String.starts_with ~prefix:"http://" s -> "git+" ^ s
    | s when String.starts_with ~prefix:"https://" s -> "git+" ^ s
    | s -> s
  in
  OpamUrl.parse ~handle_suffix:true s

(* Use the same URL-hashed src_dir as {!Pin} so both paths share the
   clone on disk. If the user supplies the URL as both a [--with=URL]
   and a project [pin-depends:] entry, the sentinel machinery makes
   them no-op for each other. *)
let url_key (url : OpamUrl.t) =
  Digest.to_hex (Digest.string (OpamUrl.to_string url))

let sentinel_path src_dir = src_dir / ".oi-pin-ok"

let fetch ~fs ~cache ~refresh url =
  let root = Cache.pins_dir cache in
  let src_dir = root / "sources" / url_key url in
  let sentinel = sentinel_path src_dir in
  if Repo.cache_fresh ~refresh ~sentinel ~max_age:Repo.refresh_max_age then
    src_dir
  else begin
    Log.info (fun m ->
        m "Cloning URL project %s" (OpamUrl.to_string url));
    if Sys.file_exists src_dir then
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / src_dir);
    let dst = OpamFilename.Dir.of_string src_dir in
    OpamFilename.mkdir dst;
    let cache_dir =
      OpamRepositoryPath.download_cache OpamStateConfig.(!r.root_dir)
    in
    let result =
      OpamRepository.pull_tree (OpamUrl.to_string url) ~cache_dir dst []
        [ url ]
      |> OpamProcess.Job.run
    in
    match result with
    | OpamTypes.Result _ | OpamTypes.Up_to_date _ ->
        Eio.Path.save ~create:(`Or_truncate 0o644)
          Eio.Path.(fs / sentinel_path src_dir)
          "";
        src_dir
    | OpamTypes.Not_available (_, msg) ->
        Error.config_error "--with=%s: fetch failed: %s"
          (OpamUrl.to_string url) msg
  end

(* Build a synthetic [Project.pin] for a local package [name] declared
   by the cloned project. Version comes from the [*.opam] file's
   [version:] field when present; "dev" is the conventional fallback
   for in-development projects without a tagged release. *)
let synth_pin ~src_dir url name =
  let opam_path = src_dir / (name ^ ".opam") in
  let version =
    match
      OpamFile.OPAM.read_opt (OpamFile.make (OpamFilename.raw opam_path))
    with
    | Some opam ->
        OpamFile.OPAM.version_opt opam
        |> Stdlib.Option.map OpamPackage.Version.to_string
        |> Stdlib.Option.value ~default:"dev"
    | None -> "dev"
  in
  let pkg =
    OpamPackage.create
      (OpamPackage.Name.of_string name)
      (OpamPackage.Version.of_string version)
  in
  {
    Project.pkg;
    url;
    declared_in = Fmt.str "--with=%s" (OpamUrl.to_string url);
  }

(* Pre-dune projects (e.g. dbuenzli/fmt) ship a plain [opam] file at
   the repo root instead of [<name>.opam]. {!Project.load} only looks
   at [*.opam], so fall back to reading [opam] for its [name:] field
   and synthesise the single package manually. *)
let fallback_from_plain_opam ~fs ~src_dir url =
  let opam_path = src_dir / "opam" in
  match Eio.Path.kind ~follow:true Eio.Path.(fs / opam_path) with
  | `Regular_file -> (
      match
        OpamFile.OPAM.read_opt (OpamFile.make (OpamFilename.raw opam_path))
      with
      | None -> None
      | Some opam -> (
          match OpamFile.OPAM.name_opt opam with
          | None -> None
          | Some n ->
              let name = OpamPackage.Name.to_string n in
              (* [Pin.materialize] looks for [<name>.opam] or [opam]
                 in the src_dir; plain [opam] satisfies the second
                 candidate so no extra work is needed here. *)
              Some (synth_pin ~src_dir url name, name)))
  | _ -> None

let materialize_one ~fs ~sys:_ ~cache ~refresh url_s =
  let url = to_opam_url url_s in
  let src_dir = fetch ~fs ~cache ~refresh url in
  let project = Project.load ~fs src_dir in
  let synth_pins, synth_roots =
    match project.local_packages with
    | [] -> (
        match fallback_from_plain_opam ~fs ~src_dir url with
        | None -> ([], [])
        | Some (pin, name) -> ([ pin ], [ name ]))
    | names -> (List.map (synth_pin ~src_dir url) names, names)
  in
  {
    pins = synth_pins @ project.pins;
    roots = synth_roots;
    extra_repos = project.extra_repos;
    overlays = project.overlays;
  }

let merge a b =
  {
    pins = a.pins @ b.pins;
    roots = a.roots @ b.roots;
    extra_repos = a.extra_repos @ b.extra_repos;
    overlays = a.overlays @ b.overlays;
  }

let materialize ~fs ~sys ~cache ?(refresh = false) urls =
  List.fold_left
    (fun acc url -> merge acc (materialize_one ~fs ~sys ~cache ~refresh url))
    empty urls
