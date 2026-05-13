[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.project"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* -- Top-level: read *.opam files in a directory ------------------------- *)

type extra_repo = {
  name : string;
  url : string;
  local_packages_dir : string option;
      (* When [Some d], [d] is an existing on-disk packages/ directory and
         [Source.Repo.ensure_many] returns it without cloning. Used for
         reporepo-handle overlays that live in [<reporepo>/v2/<handle>/]. *)
}

type pin = { pkg : OpamPackage.t; url : OpamUrl.t; declared_in : string }

type t = {
  deps : string list;
  local_packages : string list;
  extra_repos : extra_repo list;
  pins : pin list;
  overlays : string list;
  packages_dir : string option;
}

(* -- x-repos parsing ---------------------------------------------------- *)

let string_of_value (v : OpamParserTypes.FullPos.value) : string option =
  match v.pelem with OpamParserTypes.FullPos.String s -> Some s | _ -> None

let classify_repo_token s =
  if String.length s > 0 && s.[0] = '@' then
    `Handle (String.sub s 1 (String.length s - 1))
  else `Url s

let rec classify_token_list acc = function
  | [] -> Some (List.rev acc)
  | v :: rest -> (
      match string_of_value v with
      | Some s -> classify_token_list (classify_repo_token s :: acc) rest
      | None -> None)

let parse_repos_value (v : OpamParserTypes.FullPos.value) :
    [ `Handle of string | `Url of string ] list option =
  match v.pelem with
  | OpamParserTypes.FullPos.String s -> Some [ classify_repo_token s ]
  | OpamParserTypes.FullPos.List { pelem = items; _ } ->
      classify_token_list [] items
  | _ -> None

let synthetic_repo_name url =
  let hash = Digest.string url |> Digest.to_hex in
  "extra-" ^ String.sub hash 0 10

(* -- Per-file loading ---------------------------------------------------- *)

type raw = {
  raw_deps : string list;
  raw_extra_repos : (string * string) list;
  raw_pins : pin list;
  raw_overlays : string list;
}

let read_opam_file ~filename path : OpamFile.OPAM.t option =
  try Some (OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path)))
  with exn ->
    Log.warn (fun m ->
        m "Could not parse %s: %s" filename (Printexc.to_string exn));
    None

let load_one ~filename (opam : OpamFile.OPAM.t) : raw =
  let raw_deps =
    OpamFormula.fold_left
      (fun acc (name, _) -> OpamPackage.Name.to_string name :: acc)
      []
      (OpamFile.OPAM.depends opam)
  in
  let split_repo_tokens toks =
    List.fold_left
      (fun (urls, handles) -> function
        | `Handle h -> (urls, h :: handles)
        | `Url u -> ((synthetic_repo_name u, u) :: urls, handles))
      ([], []) toks
    |> fun (urls, handles) -> (List.rev urls, List.rev handles)
  in
  let raw_extra_repos, raw_overlays =
    match
      OpamStd.String.Map.find_opt Keys.repos (OpamFile.OPAM.extensions opam)
    with
    | None -> ([], [])
    | Some v -> (
        match parse_repos_value v with
        | None ->
            Error.fail_config_error
              "%s: %s must be a string or a list of strings (each entry is a \
               [@HANDLE] reporepo handle or a repository URL)"
              filename Keys.repos
        | Some toks -> split_repo_tokens toks)
  in
  let raw_pins =
    OpamFile.OPAM.pin_depends opam
    |> List.map (fun (pkg, url) -> { pkg; url; declared_in = filename })
  in
  { raw_deps; raw_extra_repos; raw_pins; raw_overlays }

let dedup_with_conflict_check (type a) ~(key : a -> string)
    ?(equal : a -> a -> bool = ( == ))
    ?(on_conflict : (prev:a -> curr:a -> string -> unit) option)
    (items : a list) : a list =
  let seen : (string, a) Hashtbl.t = Hashtbl.create 8 in
  let ordered = ref [] in
  List.iter
    (fun entry ->
      let k = key entry in
      match Hashtbl.find_opt seen k with
      | None ->
          Hashtbl.add seen k entry;
          ordered := entry :: !ordered
      | Some prev when equal prev entry -> ()
      | Some prev -> (
          match on_conflict with Some f -> f ~prev ~curr:entry k | None -> ()))
    items;
  List.rev !ordered

let merge_extra_repos entries =
  dedup_with_conflict_check
    ~key:(fun (_, name, _) -> name)
    ~equal:(fun (_, _, u1) (_, _, u2) -> u1 = u2)
    ~on_conflict:(fun ~prev ~curr name ->
      let prev_file, _, prev_url = prev in
      let declared_in, _, url = curr in
      Error.fail_config_error
        "package %s and %s disagree on %s entry %s: %s vs %s" prev_file
        declared_in Keys.repos name prev_url url)
    entries
  |> List.map (fun (_, name, url) -> { name; url; local_packages_dir = None })

let merge_pins (entries : pin list) : pin list =
  dedup_with_conflict_check
    ~key:(fun pin -> OpamPackage.Name.to_string (OpamPackage.name pin.pkg))
    ~equal:(fun p1 p2 ->
      OpamPackage.equal p1.pkg p2.pkg
      && OpamUrl.to_string p1.url = OpamUrl.to_string p2.url)
    ~on_conflict:(fun ~prev ~curr name ->
      Error.fail_config_error
        "package %s and %s disagree on pin-depends entry %s: %s (%s) vs %s (%s)"
        prev.declared_in curr.declared_in name
        (OpamPackage.version_to_string prev.pkg)
        (OpamUrl.to_string prev.url)
        (OpamPackage.version_to_string curr.pkg)
        (OpamUrl.to_string curr.url))
    entries

let merge_overlays (handles : string list) : string list =
  dedup_with_conflict_check ~key:(fun s -> s) handles

let load ~fs dir =
  let files =
    Eio.Path.read_dir Eio.Path.(fs / dir)
    |> List.filter (fun f ->
        Filename.check_suffix f ".opam" && Filename.chop_suffix f ".opam" <> "")
    |> List.sort String.compare
  in
  let local_packages =
    List.map (fun f -> Filename.chop_suffix f ".opam") files
  in
  let local_set = Hashtbl.create 16 in
  List.iter (fun n -> Hashtbl.replace local_set n ()) local_packages;
  let per_file_raws =
    List.filter_map
      (fun filename ->
        let path = dir / filename in
        match read_opam_file ~filename path with
        | None -> None
        | Some opam -> Some (filename, load_one ~filename opam))
      files
  in
  let dep_set = Hashtbl.create 64 in
  List.iter
    (fun (_, raw) ->
      List.iter
        (fun name ->
          if name <> "ocaml" && not (Hashtbl.mem local_set name) then
            Hashtbl.replace dep_set name ())
        raw.raw_deps)
    per_file_raws;
  let deps =
    Hashtbl.fold (fun k () acc -> k :: acc) dep_set []
    |> List.sort String.compare
  in
  let repo_entries =
    List.concat_map
      (fun (filename, raw) ->
        List.map (fun (n, u) -> (filename, n, u)) raw.raw_extra_repos)
      per_file_raws
  in
  let extra_repos = merge_extra_repos repo_entries in
  let pin_entries =
    List.concat_map (fun (_, raw) -> raw.raw_pins) per_file_raws
  in
  let pins = merge_pins pin_entries in
  let overlays =
    List.concat_map (fun (_, raw) -> raw.raw_overlays) per_file_raws
    |> merge_overlays
  in
  let packages_dir =
    let repo_file = dir / "repo" in
    match Eio.Path.kind ~follow:true Eio.Path.(fs / repo_file) with
    | `Regular_file -> Some (dir / "packages")
    | _ -> None
    | exception _ -> None
  in
  { deps; local_packages; extra_repos; pins; overlays; packages_dir }

let empty =
  {
    deps = [];
    local_packages = [];
    extra_repos = [];
    pins = [];
    overlays = [];
    packages_dir = None;
  }

let pp ppf t =
  Fmt.pf ppf "@[<h>{ deps=%d local=%d overlays=%d pins=%d repos=%d }@]"
    (List.length t.deps)
    (List.length t.local_packages)
    (List.length t.overlays) (List.length t.pins)
    (List.length t.extra_repos)

(* -- Script dependency parser ------------------------------------------- *)

module Script = struct
  let log_src = Logs.Src.create "oi.project.script"

  module Log = (val Logs.src_log log_src : Logs.LOG)

  type dep = {
    name : OpamPackage.Name.t;
    findlib_name : string;
    constraint_ : OpamFormula.version_constraint option;
  }

  let split_at_relop s =
    let n = String.length s in
    let rec loop i =
      if i >= n then None
      else match s.[i] with '<' | '>' | '=' | '!' -> Some i | _ -> loop (i + 1)
    in
    match loop 0 with
    | None -> (s, "")
    | Some i -> (String.sub s 0 i, String.sub s i (n - i))

  let parse_dep s =
    let findlib_name, constraint_text = split_at_relop s in
    let opam_name =
      match String.index_opt findlib_name '.' with
      | None -> findlib_name
      | Some i -> String.sub findlib_name 0 i
    in
    let name, constraint_ =
      OpamFormula.atom_of_string (opam_name ^ constraint_text)
    in
    { name; findlib_name; constraint_ }

  let parse_cli_dep s =
    let base, constraint_text = split_at_relop s in
    let opam_spec =
      match (constraint_text, String.index_opt base '.') with
      | "", Some i ->
          let name = String.sub base 0 i in
          let ver = String.sub base (i + 1) (String.length base - i - 1) in
          if ver = "" then base else name ^ "=" ^ ver
      | _ -> base ^ constraint_text
    in
    let name, constraint_ = OpamFormula.atom_of_string opam_spec in
    { name; findlib_name = OpamPackage.Name.to_string name; constraint_ }

  let parse_deps_from_line line =
    let line = String.trim line in
    let prefix = "[@@@opam " in
    if
      String.length line > String.length prefix + 1
      && String.sub line 0 (String.length prefix) = prefix
      && line.[String.length line - 1] = ']'
    then
      let inner =
        String.sub line (String.length prefix)
          (String.length line - String.length prefix - 1)
      in
      let tokens =
        String.split_on_char ' ' inner
        |> List.map String.trim
        |> List.map (fun s ->
            if
              String.length s >= 2
              && s.[0] = '"'
              && s.[String.length s - 1] = '"'
            then String.sub s 1 (String.length s - 2)
            else s)
        |> List.filter (fun s -> s <> "")
      in
      List.map parse_dep tokens
    else []

  let parse_deps_from_file ~fs path =
    let line =
      Eio.Path.with_open_in Eio.Path.(fs / path) @@ fun flow ->
      let buf = Eio.Buf_read.of_flow ~max_size:8192 flow in
      try Eio.Buf_read.line buf
      with End_of_file | Eio.Buf_read.Buffer_limit_exceeded -> ""
    in
    parse_deps_from_line line

  let name_s d = OpamPackage.Name.to_string d.name

  let dedup deps =
    let seen = Hashtbl.create 16 in
    List.filter
      (fun d ->
        let key = name_s d ^ "#" ^ d.findlib_name in
        if Hashtbl.mem seen key then false
        else begin
          Hashtbl.add seen key ();
          true
        end)
      deps

  let script_hash path deps =
    let content = In_channel.with_open_bin path In_channel.input_all in
    let dep_str =
      List.map
        (fun d ->
          let c =
            match d.constraint_ with
            | Some (op, v) ->
                OpamFormula.string_of_relop op ^ OpamPackage.Version.to_string v
            | None -> ""
          in
          name_s d ^ c)
        deps
      |> String.concat ","
    in
    let hash = Digest.string (content ^ "\000" ^ dep_str) |> Digest.to_hex in
    String.sub hash 0 12

  let constraints deps =
    List.fold_left
      (fun acc d ->
        match d.constraint_ with
        | None -> acc
        | Some c -> OpamPackage.Name.Map.add d.name c acc)
      OpamPackage.Name.Map.empty deps

  let is_ppx name = String.length name >= 4 && String.sub name 0 4 = "ppx_"

  let generate_project ~script ~deps ~dir =
    let content = In_channel.with_open_bin script In_channel.input_all in
    Out_channel.with_open_bin (dir / "main.ml") (fun oc ->
        output_string oc content);
    let rev_libs, rev_pps =
      List.fold_left
        (fun (libs, pps) d ->
          let n = name_s d in
          if n = "ocaml" then (libs, pps)
          else if is_ppx n then (libs, d.findlib_name :: pps)
          else (d.findlib_name :: libs, pps))
        ([], []) deps
    in
    let libraries = List.rev rev_libs in
    let pps = List.rev rev_pps in
    Out_channel.with_open_text (dir / "dune-project") (fun oc ->
        output_string oc "(lang dune 3.0)\n");
    let dune_content =
      let buf = Buffer.create 256 in
      let pp = Fmt.with_buffer buf in
      Fmt.pf pp "(executable\n (name main)\n";
      if libraries <> [] then
        Fmt.pf pp " (libraries %s)\n" (String.concat " " libraries);
      if pps <> [] then
        Fmt.pf pp " (preprocess (pps %s))\n" (String.concat " " pps);
      Fmt.pf pp ")\n";
      Buffer.contents buf
    in
    Log.debug (fun m ->
        m "generated dune for %s (in %s):@.%s" script dir dune_content);
    Out_channel.with_open_text (dir / "dune") (fun oc ->
        output_string oc dune_content)
end

(* -- URL-supplied projects ---------------------------------------------- *)

module Url = struct
  let log_src = Logs.Src.create "oi.project.url"

  module Log = (val Logs.src_log log_src : Logs.LOG)

  (* Outer types brought into scope so the record below uses them
     directly. The [nonrec t] in the .mli matches the local [t] below. *)

  type t = {
    pins : pin list;
    roots : string list;
    extra_repos : extra_repo list;
    overlays : string list;
    packages_dir : string option;
  }

  let empty =
    {
      pins = [];
      roots = [];
      extra_repos = [];
      overlays = [];
      packages_dir = None;
    }

  type with_arg = Url of string | Dep of Script.dep

  let is_url s =
    List.exists
      (fun p -> String.starts_with ~prefix:p s)
      [ "http://"; "https://"; "git+"; "git://"; "git@"; "ssh://" ]

  let classify s = if is_url s then Url s else Dep (Script.parse_cli_dep s)

  let classify_all tokens =
    List.partition_map
      (fun s -> match classify s with Url u -> Left u | Dep d -> Right d)
      tokens

  let to_opam_url s =
    let s =
      match s with
      | s when String.starts_with ~prefix:"http://" s -> "git+" ^ s
      | s when String.starts_with ~prefix:"https://" s -> "git+" ^ s
      | s -> s
    in
    OpamUrl.parse ~handle_suffix:true s

  let url_key (url : OpamUrl.t) =
    Digest.to_hex (Digest.string (OpamUrl.to_string url))

  let sentinel_path src_dir = src_dir / ".oi-pin-ok"

  let fetch ?(reporter = Build_progress.null) ~fs ~cache ~refresh url =
    let root = Cache.pins_dir cache in
    let src_dir = root / "sources" / url_key url in
    let sentinel = sentinel_path src_dir in
    if Cache.fresh ~refresh ~sentinel ~max_age:Cache.refresh_max_age then
      src_dir
    else begin
      Log.info (fun m -> m "Cloning URL project %s" (OpamUrl.to_string url));
      Fmt.kstr
        (fun s -> reporter.Build_progress.event (Status s))
        "Cloning %s" (OpamUrl.to_string url);
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
          Error.fail_config_error "--with=%s: fetch failed: %s"
            (OpamUrl.to_string url) msg
    end

  let synth_pin ~src_dir url name : pin =
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
    { pkg; url; declared_in = Fmt.str "--with=%s" (OpamUrl.to_string url) }

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
                Some (synth_pin ~src_dir url name, name)))
    | _ -> None

  let rebase_local_pin ~src_dir (pin : pin) : pin =
    let url = pin.url in
    match url.OpamUrl.transport with
    | "file" ->
        let path = url.OpamUrl.path in
        if Sys.file_exists path then pin
        else
          (* The path was resolved against cwd by OpamUrl.parse; try it
             relative to the cloned project directory instead. *)
          let cwd = Sys.getcwd () in
          let rel =
            if String.starts_with ~prefix:(cwd ^ "/") path then
              String.sub path
                (String.length cwd + 1)
                (String.length path - String.length cwd - 1)
            else path
          in
          let rebased = src_dir / rel in
          if Sys.file_exists rebased then
            let url' = { url with OpamUrl.path = rebased } in
            { pin with url = url' }
          else pin
    | _ -> pin

  let materialize_one ?(reporter = Build_progress.null) ~fs ~sys:_ ~cache
      ~refresh url_s =
    let url = to_opam_url url_s in
    let src_dir = fetch ~reporter ~fs ~cache ~refresh url in
    let project = load ~fs src_dir in
    let synth_pins, synth_roots =
      match project.local_packages with
      | [] -> (
          match fallback_from_plain_opam ~fs ~src_dir url with
          | None -> ([], [])
          | Some (pin, name) -> ([ pin ], [ name ]))
      | names -> (List.map (synth_pin ~src_dir url) names, names)
    in
    let rebased_pins = List.map (rebase_local_pin ~src_dir) project.pins in
    {
      pins = synth_pins @ rebased_pins;
      roots = synth_roots;
      extra_repos = project.extra_repos;
      overlays = project.overlays;
      packages_dir = project.packages_dir;
    }

  let merge a b =
    {
      pins = a.pins @ b.pins;
      roots = a.roots @ b.roots;
      extra_repos = a.extra_repos @ b.extra_repos;
      overlays = a.overlays @ b.overlays;
      packages_dir =
        (match a.packages_dir with
        | Some _ -> a.packages_dir
        | None -> b.packages_dir);
    }

  let materialize ?(reporter = Build_progress.null) ~fs ~sys ~cache
      ?(refresh = false) urls =
    List.fold_left
      (fun acc url ->
        merge acc (materialize_one ~reporter ~fs ~sys ~cache ~refresh url))
      empty urls
end

(* -- dune-project reader/writer ----------------------------------------- *)

module Dune = struct
  module Sexp = Sexplib0.Sexp

  type t = { sexps : Sexp.t list; path : string }

  let load ~fs ~cwd =
    let path = cwd / "dune-project" in
    let content =
      try Eio.Path.load Eio.Path.(fs / path)
      with Eio.Exn.Io _ ->
        Error.fail_config_error "no dune-project at %s" path
    in
    match Parsexp.Many.parse_string content with
    | Ok sexps -> { sexps; path }
    | Error e ->
        Error.fail_config_error "failed to parse %s: %s" path
          (Parsexp.Parse_error.message e)

  let is_form name = function
    | Sexp.List (Atom h :: _) when h = name -> true
    | _ -> false

  let package_name_of_stanza = function
    | Sexp.List items ->
        List.find_map
          (function Sexp.List [ Atom "name"; Atom n ] -> Some n | _ -> None)
          items
    | _ -> None

  let generate_opam_files t =
    List.exists
      (fun s ->
        match s with
        | Sexp.List [ Atom "generate_opam_files" ] -> true
        | Sexp.List [ Atom "generate_opam_files"; Atom "true" ] -> true
        | _ -> false)
      t.sexps

  let package_names t =
    List.filter_map
      (fun s -> if is_form "package" s then package_name_of_stanza s else None)
      t.sexps

  let dep_sexp ~name ~constraint_ =
    match constraint_ with
    | None -> Sexp.Atom name
    | Some (op, ver) -> Sexp.List [ Atom name; Sexp.List [ Atom op; Atom ver ] ]

  let dep_matches ~name existing =
    match existing with
    | Sexp.Atom n -> n = name
    | Sexp.List (Atom n :: _) -> n = name
    | _ -> false

  let add_to_stanza ~new_dep ~name stanza =
    match stanza with
    | Sexp.List items ->
        let had_depends = ref false in
        let items' =
          List.map
            (function
              | Sexp.List (Atom "depends" :: existing) as orig ->
                  had_depends := true;
                  if List.exists (dep_matches ~name) existing then orig
                  else
                    Sexp.List ((Sexp.Atom "depends" :: existing) @ [ new_dep ])
              | other -> other)
            items
        in
        let items' =
          if !had_depends then items'
          else items' @ [ Sexp.List [ Atom "depends"; new_dep ] ]
        in
        Sexp.List items'
    | other -> other

  let add_dependency t ?package ~name ~constraint_ () =
    let new_dep = dep_sexp ~name ~constraint_ in
    let names = package_names t in
    let target_name =
      match (package, names) with
      | Some n, _ -> n
      | None, [ one ] -> one
      | None, [] ->
          Error.fail_config_error
            "no (package …) stanzas in dune-project; nothing to edit"
      | None, many ->
          Error.fail_config_error
            "multiple packages in dune-project (%s); re-run with -p PKG to \
             pick one"
            (String.concat ", " many)
    in
    let found = ref false in
    let sexps' =
      List.map
        (fun s ->
          if is_form "package" s && package_name_of_stanza s = Some target_name
          then begin
            found := true;
            add_to_stanza ~new_dep ~name s
          end
          else s)
        t.sexps
    in
    if not !found then
      Error.fail_config_error "no (package (name %s) …) stanza in %s"
        target_name t.path;
    { t with sexps = sexps' }

  let save ~fs t =
    let content =
      String.concat "\n\n" (List.map Sexp.to_string_hum t.sexps) ^ "\n"
    in
    Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / t.path) content
end

(* -- Dev-tool probes ---------------------------------------------------- *)

module Tool = struct
  type trigger = Ocamlformat_file | Dune_project_using of string
  type spec = { name : string; binary : string; trigger : trigger }

  (* Editor and dev tools whose installation depends on project state.
     Always-on tools (odoc, merlin, ocaml-lsp-server) used to live here
     too; they migrated to the active toolchain's [x-oi-toolchain-tools]
     reporepo field so the toolchain decides which dev tools its
     consumers get. The probes below only cover triggered tools — the
     triggers are project-state checks ([.ocamlformat] file present,
     [dune-project] uses a particular plugin) that the reporepo can't
     express. *)
  let specs =
    [
      { name = "mdx"; binary = "ocaml-mdx"; trigger = Dune_project_using "mdx" };
      {
        name = "ocamlformat";
        binary = "ocamlformat";
        trigger = Ocamlformat_file;
      };
    ]

  type result = {
    spec : spec;
    hit : bool;
    version : string option;
    detail : string;
  }

  let hit spec ?version detail = { spec; hit = true; version; detail }
  let miss spec detail = { spec; hit = false; version = None; detail }
  let read_file path = In_channel.with_open_text path In_channel.input_all

  let is_regular_file ~fs path =
    match Eio.Path.kind ~follow:true Eio.Path.(fs / path) with
    | `Regular_file -> true
    | _ -> false

  let ocamlformat_version_line line =
    let line = String.trim line in
    if line = "" || line.[0] = '#' then None
    else
      match String.index_opt line '=' with
      | None -> None
      | Some i ->
          let k = String.trim (String.sub line 0 i) in
          let v =
            String.trim (String.sub line (i + 1) (String.length line - i - 1))
          in
          if k = "version" && v <> "" then Some v else None

  let parse_ocamlformat_version path =
    String.split_on_char '\n' (read_file path)
    |> List.find_map ocamlformat_version_line

  let probe_ocamlformat ~fs dir spec =
    let path = dir / ".ocamlformat" in
    if not (is_regular_file ~fs path) then miss spec ".ocamlformat: missing"
    else
      match parse_ocamlformat_version path with
      | Some ver ->
          Fmt.kstr (hit spec ~version:ver) ".ocamlformat: version = %s" ver
      | None -> miss spec ".ocamlformat: no 'version = ...' line"

  let dune_project_using ~fs dir name =
    let path = dir / "dune-project" in
    if not (is_regular_file ~fs path) then false
    else
      match Parsexp.Many.parse_string (read_file path) with
      | Error _ -> false
      | Ok sexps ->
          List.exists
            (function
              | Sexplib0.Sexp.List (Atom "using" :: Atom plugin :: _)
                when plugin = name ->
                  true
              | _ -> false)
            sexps

  let probe_using ~fs dir spec plugin =
    if dune_project_using ~fs dir plugin then
      Fmt.kstr (hit spec) "dune-project: (using %s ...)" plugin
    else Fmt.kstr (miss spec) "dune-project: no (using %s ...)" plugin

  let probe_one ~fs dir spec =
    match spec.trigger with
    | Ocamlformat_file -> probe_ocamlformat ~fs dir spec
    | Dune_project_using plugin -> probe_using ~fs dir spec plugin

  let probe ~fs dir = List.map (probe_one ~fs dir) specs
  let hits = List.filter (fun r -> r.hit)
end
