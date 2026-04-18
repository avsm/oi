[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Reporepo: an opam-layout registry of overlay-repository pins. *)

let log_src = Logs.Src.create "oi.reporepo"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

let default_path =
  match Sys.getenv_opt "HOME" with
  | Some h -> h / "scratch" / "reporepo"
  | None -> "/tmp/reporepo"

let env_path () =
  match Sys.getenv_opt "OI_REPOREPO" with
  | Some v when v <> "" -> v
  | _ -> default_path

type entry = {
  handle : string;
  version : string;
  url : string;
  commit : string;
  depends : (string * string option) list;
  opam_path : string;
}

type root = { handle : string; version : string option }

(* -- File IO helpers ----------------------------------------------------- *)

let rec mkdir_p d =
  if d = "/" || d = "." || d = "" || Sys.file_exists d then ()
  else begin
    mkdir_p (Filename.dirname d);
    try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let write_file path content =
  mkdir_p (Filename.dirname path);
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () ->
      output_string oc content)

(* -- Loading ------------------------------------------------------------- *)

let split_url_commit s =
  match String.index_opt s '#' with
  | None -> (s, "")
  | Some i ->
      (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

let is_overlay_extension extensions =
  match OpamStd.String.Map.find_opt "x-oi-overlay" extensions with
  | None -> false
  | Some v -> (
      match v.OpamParserTypes.FullPos.pelem with
      | OpamParserTypes.FullPos.Bool true -> true
      | _ -> false)

(* Extract an exact [= "version"] pin from an opam [condition], if
   present. Anything more involved (filters, ranges) renders as [None]. *)
let pin_version_of_condition cond =
  OpamFormula.fold_left
    (fun acc foc ->
      match (acc, foc) with
      | Some _, _ -> acc
      | None, OpamTypes.Constraint (`Eq, filter) -> (
          match filter with
          | OpamTypes.FString v -> Some v
          | _ -> None)
      | None, _ -> None)
    None cond

let parse_depends_formula formula =
  let out = ref [] in
  OpamFormula.iter
    (fun (name, cond) ->
      out
      := (OpamPackage.Name.to_string name, pin_version_of_condition cond)
         :: !out)
    formula;
  List.rev !out

let parse_entry_file path : entry option =
  try
    let opam = OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path)) in
    if not (is_overlay_extension (OpamFile.OPAM.extensions opam)) then None
    else
      let handle =
        match OpamFile.OPAM.name_opt opam with
        | Some n -> OpamPackage.Name.to_string n
        | None -> Error.config_error "%s: missing name field" path
      in
      let version =
        match OpamFile.OPAM.version_opt opam with
        | Some v -> OpamPackage.Version.to_string v
        | None -> Error.config_error "%s: missing version field" path
      in
      let url =
        match OpamFile.OPAM.url opam with
        | Some u -> OpamUrl.to_string (OpamFile.URL.url u)
        | None -> Error.config_error "%s: missing url: src field" path
      in
      let url_bare, commit = split_url_commit url in
      let depends = parse_depends_formula (OpamFile.OPAM.depends opam) in
      Some
        {
          handle;
          version;
          url = url_bare;
          commit;
          depends;
          opam_path = path;
        }
  with
  | Error.E _ as e -> raise e
  | exn ->
      Log.warn (fun m ->
          m "Skipping %s: %s" path (Printexc.to_string exn));
      None

let load ~path =
  let packages_dir = path / "packages" in
  if not (Sys.file_exists packages_dir) then []
  else
    let handles = Sys.readdir packages_dir |> Array.to_list in
    List.sort String.compare handles
    |> List.concat_map (fun h ->
           let handle_dir = packages_dir / h in
           if not (Sys.is_directory handle_dir) then []
           else
             let versions = Sys.readdir handle_dir |> Array.to_list in
             List.sort String.compare versions
             |> List.filter_map (fun v ->
                    let opam_path = handle_dir / v / "opam" in
                    if Sys.file_exists opam_path then parse_entry_file opam_path
                    else None))

(* -- Version ordering --------------------------------------------------- *)

let version_compare a b =
  OpamPackage.Version.compare
    (OpamPackage.Version.of_string a)
    (OpamPackage.Version.of_string b)

let latest entries ~handle =
  entries
  |> List.filter (fun (e : entry) -> e.handle = handle)
  |> List.sort (fun (a : entry) (b : entry) ->
         version_compare b.version a.version)
  |> function x :: _ -> Some x | [] -> None

let find entries ~handle ~version =
  List.find_opt
    (fun (e : entry) -> e.handle = handle && e.version = version)
    entries

(* Topologically sort a set of resolved overlay entries so that
   dependencies appear before their dependents. The opam-0install
   solver doesn't guarantee this order; we sort ourselves because
   downstream callers rely on it for priority placement. *)
let topo_sort entries =
  let by_handle = Hashtbl.create 16 in
  List.iter
    (fun (e : entry) -> Hashtbl.replace by_handle e.handle e)
    entries;
  let visited = Hashtbl.create 16 in
  let out_rev = ref [] in
  let rec visit (e : entry) =
    if not (Hashtbl.mem visited e.handle) then begin
      Hashtbl.add visited e.handle ();
      List.iter
        (fun (h, _v) ->
          match Hashtbl.find_opt by_handle h with
          | Some dep -> visit dep
          | None -> ())
        e.depends;
      out_rev := e :: !out_rev
    end
  in
  List.iter visit entries;
  List.rev !out_rev

module Solver = Opam_0install.Solver.Make (Opam_0install.Dir_context)

(* Resolve overlay handles against the reporepo by invoking the
   opam-0install solver over [<reporepo>/packages/]. Each overlay's
   [depends:] field composes exactly like a regular opam package, so
   the solver handles exact pins, unpinned names, and version-range
   constraints uniformly, and flags conflicts (e.g. two roots that
   both want different pinned versions of the same base overlay). *)
let resolve entries ~roots =
  if roots = [] then []
  else
    let path = env_path () in
    let packages_dir = path / "packages" in
    if not (Sys.file_exists packages_dir) then
      Error.config_error "reporepo at %s has no packages/ tree" path;
    let constraints =
      List.fold_left
        (fun m (r : root) ->
          match r.version with
          | None -> m
          | Some v ->
              OpamPackage.Name.Map.add
                (OpamPackage.Name.of_string r.handle)
                (`Eq, OpamPackage.Version.of_string v)
                m)
        OpamPackage.Name.Map.empty roots
    in
    (* Overlay opam files don't use opam filter variables; return
       [None] uniformly. If a future overlay does consult variables
       the solver will raise and we'll revisit. *)
    let env _ = None in
    let ctx =
      Opam_0install.Dir_context.create ~constraints ~env packages_dir
    in
    let names =
      List.map
        (fun (r : root) -> OpamPackage.Name.of_string r.handle)
        roots
    in
    Log.debug (fun m ->
        m "0install solve over %s for roots: %s" packages_dir
          (String.concat ", "
             (List.map OpamPackage.Name.to_string names)));
    match Solver.solve ctx names with
    | Error diag ->
        Error.config_error "overlay resolution failed: %s"
          (Solver.diagnostics diag)
    | Ok sels ->
        let pkgs = Solver.packages_of_result sels in
        let resolved =
          List.filter_map
            (fun pkg ->
              find entries
                ~handle:(OpamPackage.Name.to_string (OpamPackage.name pkg))
                ~version:
                  (OpamPackage.Version.to_string (OpamPackage.version pkg)))
            pkgs
        in
        let sorted = topo_sort resolved in
        List.iter
          (fun (e : entry) ->
            let short =
              if String.length e.commit >= 7 then String.sub e.commit 0 7
              else e.commit
            in
            Log.debug (fun m ->
                m "  resolved %s.%s@%s via %s" e.handle e.version short
                  e.url))
          sorted;
        sorted

(* -- Materialising (cloning) ------------------------------------------- *)

let clone_dir_name (e : entry) = "overlay-" ^ e.handle ^ "-" ^ e.version

let materialize ~sys:_ ~data_dir ?(refresh = false) entries =
  List.map
    (fun (e : entry) ->
      let name = clone_dir_name e in
      let dir = data_dir / "repos" / name in
      (* Pin to the exact commit in the URL fragment so the clone is
         reproducible. The upstream URL may be any backend opam
         understands; the [#<sha>] suffix is respected by the git
         backend. *)
      let url =
        if e.commit = "" then e.url else e.url ^ "#" ^ e.commit
      in
      Repo.ensure_one ~refresh ~label:name ~url ~dir;
      dir / "packages")
    entries

(* -- Base overlay resolution ------------------------------------------- *)

(* Resolve the base universe from [relocatable] (which transitively
   depends on [default]). The solver wants higher-priority repos
   first in the list, so we reverse the deps-first topological order
   returned by [resolve]: [default, relocatable] becomes
   [relocatable, default]. *)
let base_entries () =
  let path = env_path () in
  if not (Sys.file_exists path) then []
  else
    let entries = load ~path in
    match latest entries ~handle:"relocatable" with
    | None -> []
    | Some _ ->
        resolve entries ~roots:[ { handle = "relocatable"; version = None } ]
        |> List.rev

let ensure_base ~data_dir ?(refresh = false) () =
  let path = env_path () in
  Log.debug (fun m -> m "ensure_base: reading reporepo %s" path);
  let entries = try load ~path with Error.E _ -> [] in
  match latest entries ~handle:"relocatable" with
  | None ->
      Error.config_error
        "reporepo at %s has no 'relocatable' overlay; run 'oi repo add \
         relocatable <URL>' to bootstrap the base repositories"
        path
  | Some _ ->
      let base =
        resolve entries ~roots:[ { handle = "relocatable"; version = None } ]
        |> List.rev
      in
      Log.debug (fun m ->
          m "base overlays (highest priority first): %s"
            (String.concat ", "
               (List.map
                  (fun (e : entry) -> e.handle ^ "." ^ e.version)
                  base)));
      List.map
        (fun (e : entry) ->
          let name = clone_dir_name e in
          let dir = data_dir / "repos" / name in
          let url =
            if e.commit = "" then e.url else e.url ^ "#" ^ e.commit
          in
          Repo.ensure_one ~refresh ~label:name ~url ~dir;
          dir / "packages")
        base

(* -- git ls-remote ------------------------------------------------------- *)

(* Resolve the HEAD of a remote git URL to a 40-char sha via
   [git ls-remote]. Tries HEAD first; if that's empty (remote's HEAD
   points at a branch that doesn't exist) falls back to the first of
   [main], [master], or any ref the remote advertises. *)
let parse_ls_remote_output out =
  String.split_on_char '\n' out
  |> List.filter_map (fun line ->
         let line = String.trim line in
         if line = "" then None
         else
           match String.split_on_char '\t' line with
           | sha :: ref_name :: _ when String.length sha = 40 ->
               Some (sha, ref_name)
           | _ -> None)

let try_ls_remote ~sys url args =
  try D10.Sysops.Cmd.run_out sys ("git" :: "ls-remote" :: url :: args)
  with exn ->
    Error.config_error "git ls-remote %s failed: %s" url (Printexc.to_string exn)

(* Resolve a git URL + optional ref to a 40-char commit sha.
   When [ref_] is [None] we try [HEAD], then fall back to
   [refs/heads/main] / [master] / any ref. When [ref_] is given we
   ask git for that ref directly and error if it's not found. *)
let ls_remote_sha ~sys ?ref_ url =
  match ref_ with
  | Some r -> (
      let out = try_ls_remote ~sys url [ r ] in
      match parse_ls_remote_output out with
      | (sha, _) :: _ -> sha
      | [] ->
          Error.config_error "git ls-remote %s %s: ref not found" url r)
  | None -> (
      let head = try_ls_remote ~sys url [ "HEAD" ] in
      match parse_ls_remote_output head with
      | (sha, _) :: _ -> sha
      | [] -> (
          let all = try_ls_remote ~sys url [] in
          let refs = parse_ls_remote_output all in
          let prefer name =
            List.find_map
              (fun (sha, r) -> if r = name then Some sha else None)
              refs
          in
          match
            Stdlib.Option.(
              prefer "refs/heads/main" |> fun x ->
              match x with Some _ -> x | None -> prefer "refs/heads/master")
          with
          | Some sha -> sha
          | None -> (
              match refs with
              | (sha, _) :: _ -> sha
              | [] ->
                  Error.config_error "git ls-remote %s returned no refs"
                    url)))

(* -- Today's version ----------------------------------------------------- *)

let today_yyyymmdd () =
  let tm = Unix.gmtime (Unix.time ()) in
  Fmt.str "%04d%02d%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

(* Next free [YYYYMMDD.N] for today among existing versions of [handle]. *)
let next_version entries ~handle =
  let today = today_yyyymmdd () in
  let prefix = today ^ "." in
  let max_seq =
    entries
    |> List.filter (fun (e : entry) -> e.handle = handle)
    |> List.filter_map (fun (e : entry) ->
           if String.starts_with ~prefix e.version then
             let suffix =
               String.sub e.version (String.length prefix)
                 (String.length e.version - String.length prefix)
             in
             int_of_string_opt suffix
           else None)
    |> List.fold_left max (-1)
  in
  Fmt.str "%s.%d" today (max_seq + 1)

(* -- opam file rendering ------------------------------------------------- *)

let escape_string s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let render_opam ~synopsis ~url ~commit ~depends ~display_name ~origin_url =
  let buf = Buffer.create 512 in
  Printf.bprintf buf "opam-version: \"2.0\"\n";
  Printf.bprintf buf "synopsis: %s\n" (escape_string synopsis);
  Printf.bprintf buf "url {\n  src: %s\n}\n"
    (escape_string (if commit = "" then url else url ^ "#" ^ commit));
  (match depends with
  | [] -> ()
  | ds ->
      Buffer.add_string buf "depends: [\n";
      List.iter
        (fun (h, v) ->
          match v with
          | Some ver ->
              Printf.bprintf buf "  %s { = %s }\n" (escape_string h)
                (escape_string ver)
          | None -> Printf.bprintf buf "  %s\n" (escape_string h))
        ds;
      Buffer.add_string buf "]\n");
  Printf.bprintf buf "x-oi-overlay: true\n";
  (match display_name with
  | Some s -> Printf.bprintf buf "x-oi-display-name: %s\n" (escape_string s)
  | None -> ());
  (match origin_url with
  | Some s -> Printf.bprintf buf "x-oi-origin-url: %s\n" (escape_string s)
  | None -> ());
  Buffer.contents buf

let write_entry ~path ~handle ~version content =
  let pkg_dir = path / "packages" / handle / (handle ^ "." ^ version) in
  let opam_path = pkg_dir / "opam" in
  write_file opam_path content;
  opam_path

let ensure_repo_marker ~path =
  let marker = path / "repo" in
  if not (Sys.file_exists marker) then
    write_file marker "opam-version: \"2.0\"\n"

(* -- add / bump / remove ------------------------------------------------ *)

let default_synopsis handle =
  "Overlay: " ^ handle ^ " — pinned opam repository"

(* Every non-base overlay carries [relocatable]/[default] deps pinned
   to whatever the reporepo currently has at its latest versions.
   That's how an overlay captures, via the [depends:] lockfile, which
   base repos and which commits it was composed against. *)
let is_base_handle h = h = "default" || h = "relocatable"

let auto_base_depends entries ~handle =
  if is_base_handle handle then []
  else
    let pin h =
      match latest entries ~handle:h with
      | Some e -> Some (h, Some e.version)
      | None -> None
    in
    List.filter_map Fun.id [ pin "relocatable"; pin "default" ]

let add ~sys ~path ~handle ~url ?ref_ ?depends ?synopsis ?display_name
    ?origin_url () =
  let entries = load ~path in
  if List.exists (fun (e : entry) -> e.handle = handle) entries then
    Error.config_error
      "overlay %s already exists in reporepo; use 'oi repo bump' to add a \
       new version"
      handle;
  let depends =
    match depends with
    | Some d -> d
    | None -> auto_base_depends entries ~handle
  in
  let commit = ls_remote_sha ~sys ?ref_ url in
  let version = today_yyyymmdd () ^ ".0" in
  let synopsis = Stdlib.Option.value synopsis ~default:(default_synopsis handle) in
  let content =
    render_opam ~synopsis ~url ~commit ~depends ~display_name ~origin_url
  in
  ensure_repo_marker ~path;
  let opam_path = write_entry ~path ~handle ~version content in
  { handle; version; url; commit; depends; opam_path }

let bump ~sys ~path ~handle ?url ?ref_ ?depends () =
  let entries = load ~path in
  let prev =
    match latest entries ~handle with
    | Some e -> e
    | None ->
        Error.config_error
          "overlay %s not in reporepo; use 'oi repo add' to create it" handle
  in
  let url = Stdlib.Option.value url ~default:prev.url in
  let commit = ls_remote_sha ~sys ?ref_ url in
  (* When [--depend] wasn't passed, refresh the auto-injected base
     pins against whatever the reporepo currently has. That way
     bumping a user overlay automatically re-locks it against the
     latest base versions, which is usually what you want. Explicit
     deps are carried through untouched. *)
  let depends =
    match depends with
    | Some d -> d
    | None ->
        if is_base_handle handle then prev.depends
        else
          let auto = auto_base_depends entries ~handle in
          if auto = [] then prev.depends else auto
  in
  let version = next_version entries ~handle in
  let content =
    render_opam ~synopsis:(default_synopsis handle) ~url ~commit ~depends
      ~display_name:None ~origin_url:None
  in
  let opam_path = write_entry ~path ~handle ~version content in
  { handle; version; url; commit; depends; opam_path }

let rec rmtree_path p =
  if Sys.file_exists p && Sys.is_directory p then begin
    Sys.readdir p |> Array.iter (fun e -> rmtree_path (p / e));
    try Unix.rmdir p with Unix.Unix_error _ -> ()
  end
  else try Unix.unlink p with Unix.Unix_error _ -> ()

let remove ~path ~handle ?version () =
  let entries = load ~path in
  let matches =
    List.filter
      (fun (e : entry) ->
        e.handle = handle
        && match version with None -> true | Some v -> e.version = v)
      entries
  in
  if matches = [] then
    Error.config_error "no such overlay %s%s in reporepo" handle
      (match version with None -> "" | Some v -> "." ^ v);
  List.iter
    (fun (e : entry) ->
      let pkg_dir = Filename.dirname e.opam_path in
      rmtree_path pkg_dir)
    matches;
  (* If no versions left, remove the handle directory too. *)
  let handle_dir = path / "packages" / handle in
  if
    Sys.file_exists handle_dir
    && Sys.readdir handle_dir |> Array.length = 0
  then try Unix.rmdir handle_dir with Unix.Unix_error _ -> ()
