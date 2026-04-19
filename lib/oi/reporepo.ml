[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Reporepo: an opam-layout registry of overlay-repository pins. *)

let log_src = Logs.Src.create "oi.reporepo"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

(* The reporepo lives under the XDG data hierarchy so it survives
   [oi clean --all] and sits in a predictable spot for the user to
   cd into, commit, and push from. *)
let default_path =
  let data_base =
    match Sys.getenv_opt "OI_DATA_DIR" with
    | Some v when v <> "" -> v
    | _ -> (
        match Sys.getenv_opt "XDG_DATA_HOME" with
        | Some v when v <> "" -> v / "oi"
        | _ -> (
            match Sys.getenv_opt "HOME" with
            | Some h -> h / ".local" / "share" / "oi"
            | None -> "/tmp/oi"))
  in
  data_base / "reporepo"

let env_path () =
  match Sys.getenv_opt "OI_REPOREPO" with
  | Some v when v <> "" -> v
  | _ -> default_path

let default_url = "https://tangled.org/anil.recoil.org/reporepo.git"

let env_url () =
  match Sys.getenv_opt "OI_REPOREPO_URL" with
  | Some v when v <> "" -> v
  | _ -> default_url

type entry = {
  handle : string;
  version : string;
  url : string;
  commit : string;
  ref_ : string option;
  depends : (string * string option) list;
  opam_path : string;
}

type root = { handle : string; version : string option }

(* -- File IO helpers ----------------------------------------------------- *)

let mkdir_p ~fs d =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / d)

let write_file ~fs path content =
  mkdir_p ~fs (Filename.dirname path);
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / path) content

(* Clone the reporepo from [url] into [path] when [path] isn't already
   a git working copy. Never pulls once the clone exists: the user is
   expected to [cd] in, edit, commit, and push — any auto-pull would
   risk stomping on local commits or unstaged edits. *)
let ensure_clone ~fs ~sys ~path ~url =
  let dot_git = path / ".git" in
  if Sys.file_exists dot_git then ()
  else if
    Sys.file_exists path
    && Sys.is_directory path
    && Array.length (Sys.readdir path) > 0
  then
    Error.config_error
      "reporepo path %s exists but is not a git clone; move or remove \
       it and retry"
      path
  else begin
    mkdir_p ~fs (Filename.dirname path);
    Log.info (fun m -> m "Cloning reporepo from %s to %s" url path);
    try
      Retry.with_attempts ~label:(Fmt.str "git clone %s" url) (fun () ->
          D10.Sysops.Cmd.run sys [ "git"; "clone"; url; path ])
    with _ ->
      Error.config_error "failed to clone reporepo from %s into %s" url
        path
  end

(* -- Sync (pull/commit/push) -------------------------------------------- *)

(* Internal git plumbing without progress (used for queries like
   [rev-parse]). *)
let git_at ~sys ~path args =
  D10.Sysops.Cmd.run sys ("git" :: "-C" :: path :: args)

let git_at_out ~sys ~path args =
  D10.Sysops.Cmd.run_out sys ("git" :: "-C" :: path :: args)

(* Git for user-facing operations (pull, push, commit). Streams stdout
   and stderr through to the parent terminal so the user sees the same
   output [git] would print on the command line — including the actual
   error message on failure. *)
let git_at_inherit ~sys ~path args =
  D10.Sysops.Cmd.run_inherit sys ("git" :: "-C" :: path :: args)

let assert_clone path =
  if not (Sys.file_exists (path / ".git")) then
    Error.config_error
      "reporepo at %s is not a git working copy — run an [oi repo] \
       subcommand first to bootstrap the clone"
      path

let set_push_url ~sys ~path url =
  assert_clone path;
  git_at ~sys ~path [ "remote"; "set-url"; "--push"; "origin"; url ]

type push_step =
  | Step_commit of { files : string list }
      (** Captured by the auto-commit; empty when the tree was already clean. *)
  | Step_pull of { commits : int }
      (** Number of upstream commits brought in by the rebase (0 = up to date). *)
  | Step_push of { commits : int }
      (** Number of local commits sent upstream (0 = nothing to push). *)

type push_outcome = push_step list

(* Count commits in [base..head] (linear distance). Returns 0 when
   either revision is unknown (e.g. no upstream tracking branch). *)
let commit_count ~sys ~path ~base ~head =
  if base = "" || head = "" || base = head then 0
  else
    try int_of_string (git_at_out ~sys ~path [ "rev-list"; "--count"; base ^ ".." ^ head ])
    with _ -> 0

let push ?(on_step_start = fun _ _ -> ()) ~sys ~path () =
  assert_clone path;
  (* 1. Auto-commit any local edits (typically from [oi repo bump] or
        manual edits) so they're a real commit before we rebase on
        upstream. Doing this first means any rebase conflicts manifest
        as commit-vs-commit conflicts (easier to drop into a shell to
        resolve) rather than dirty-tree-vs-stash conflicts. *)
  let porcelain = git_at_out ~sys ~path [ "status"; "--porcelain" ] in
  let dirty_paths =
    porcelain |> String.split_on_char '\n'
    |> List.filter_map (fun line ->
           let line = String.trim line in
           if line = "" || String.length line < 4 then None
           else Some (String.sub line 3 (String.length line - 3)))
  in
  on_step_start 1 "commit local changes";
  let commit_step =
    if dirty_paths = [] then Step_commit { files = [] }
    else begin
      git_at ~sys ~path [ "add"; "-A" ];
      let summary =
        if List.length dirty_paths = 1 then List.hd dirty_paths
        else Fmt.str "%d files" (List.length dirty_paths)
      in
      let msg =
        Fmt.str
          "oi repo push: local edits (%s)\n\nAuto-committed by `oi repo \
           push`. Files:\n%s"
          summary
          (String.concat "\n" (List.map (fun p -> "  " ^ p) dirty_paths))
      in
      git_at_inherit ~sys ~path [ "commit"; "-m"; msg ];
      Step_commit { files = dirty_paths }
    end
  in
  (* 2. Bring in upstream commits. [--rebase] keeps history linear and
        avoids merge bubbles in what is meant to read as a chronological
        log of pinned overlay versions. *)
  on_step_start 2 "pull --rebase from upstream";
  let head_before = git_at_out ~sys ~path [ "rev-parse"; "HEAD" ] in
  git_at_inherit ~sys ~path [ "pull"; "--rebase" ];
  let head_after = git_at_out ~sys ~path [ "rev-parse"; "HEAD" ] in
  let pull_step =
    Step_pull { commits = commit_count ~sys ~path ~base:head_before ~head:head_after }
  in
  (* 3. Push to whatever remote/branch the current branch tracks. The
        push URL may have been overridden via [set_push_url] earlier in
        this run, but that's a local-config concern; [git push] picks it
        up automatically. *)
  on_step_start 3 "push to origin";
  let push_step =
    let local = git_at_out ~sys ~path [ "rev-parse"; "@" ] in
    let remote =
      try git_at_out ~sys ~path [ "rev-parse"; "@{u}" ] with _ -> ""
    in
    let ahead = commit_count ~sys ~path ~base:remote ~head:local in
    if ahead = 0 then Step_push { commits = 0 }
    else begin
      git_at_inherit ~sys ~path [ "push" ];
      Step_push { commits = ahead }
    end
  in
  [ commit_step; pull_step; push_step ]

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

(* Read an [x-oi-*] string-valued extension from an opam file. Returns
   [None] when the field is absent or of the wrong shape. *)
let read_string_extension extensions name =
  match OpamStd.String.Map.find_opt name extensions with
  | None -> None
  | Some v -> (
      match v.OpamParserTypes.FullPos.pelem with
      | OpamParserTypes.FullPos.String s -> Some s
      | _ -> None)

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
      let ref_ =
        read_string_extension (OpamFile.OPAM.extensions opam) "x-oi-ref"
      in
      Some
        {
          handle;
          version;
          url = url_bare;
          commit;
          ref_;
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

let materialize ~fs ~sys:_ ~data_dir ?(refresh = false) entries =
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
      Repo.ensure_one ~fs ~refresh ~label:name ~url ~dir;
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

let ensure_base ~fs ~sys ~data_dir ?(refresh = false) () =
  let path = env_path () in
  ensure_clone ~fs ~sys ~path ~url:(env_url ());
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
          Repo.ensure_one ~fs ~refresh ~label:name ~url ~dir;
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
  try
    Retry.with_attempts ~label:(Fmt.str "git ls-remote %s" url) (fun () ->
        D10.Sysops.Cmd.run_out sys ("git" :: "ls-remote" :: url :: args))
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
          match prefer "refs/heads/main" with
          | Some sha -> sha
          | None -> (
              match prefer "refs/heads/master" with
              | Some sha -> sha
              | None -> (
                  match refs with
                  | (sha, _) :: _ -> sha
                  | [] ->
                      Error.config_error "git ls-remote %s returned no refs"
                        url))))

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

let render_opam ~synopsis ~url ~commit ~ref_ ~depends ~display_name
    ~origin_url =
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
  Stdlib.Option.iter
    (fun s -> Printf.bprintf buf "x-oi-ref: %s\n" (escape_string s))
    ref_;
  Stdlib.Option.iter
    (fun s -> Printf.bprintf buf "x-oi-display-name: %s\n" (escape_string s))
    display_name;
  Stdlib.Option.iter
    (fun s -> Printf.bprintf buf "x-oi-origin-url: %s\n" (escape_string s))
    origin_url;
  Buffer.contents buf

let write_entry ~fs ~path ~handle ~version content =
  let pkg_dir = path / "packages" / handle / (handle ^ "." ^ version) in
  let opam_path = pkg_dir / "opam" in
  write_file ~fs opam_path content;
  opam_path

let ensure_repo_marker ~fs ~path =
  let marker = path / "repo" in
  if not (Sys.file_exists marker) then
    write_file ~fs marker "opam-version: \"2.0\"\n"

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

let add ~fs ~sys ~path ~handle ~url ?ref_ ?depends ?synopsis ?display_name
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
    render_opam ~synopsis ~url ~commit ~ref_ ~depends ~display_name ~origin_url
  in
  ensure_repo_marker ~fs ~path;
  let opam_path = write_entry ~fs ~path ~handle ~version content in
  { handle; version; url; commit; ref_; depends; opam_path }

let bump ~fs ~sys ~path ~handle ?url ?ref_ ?depends () =
  let entries = load ~path in
  let prev =
    match latest entries ~handle with
    | Some e -> e
    | None ->
        Error.config_error
          "overlay %s not in reporepo; use 'oi repo add' to create it" handle
  in
  let url = Stdlib.Option.value url ~default:prev.url in
  (* When [--ref] isn't passed, reuse whatever the previous version
     tracked. This keeps a branch like [relocatable] pinned across
     bumps rather than silently reverting to HEAD. *)
  let ref_ = match ref_ with Some _ -> ref_ | None -> prev.ref_ in
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
  (* Skip the write when the resolved state matches [prev] exactly —
     same URL, commit, branch, and deps. Otherwise we'd accumulate
     identical [YYYYMMDD.N] entries on every scheduled bump. Compare
     deps order-insensitively since [auto_base_depends] returns them
     in lookup order, which may differ from the file order. *)
  let same_depends a b =
    let norm = List.sort compare in
    norm a = norm b
  in
  if
    url = prev.url && commit = prev.commit && ref_ = prev.ref_
    && same_depends depends prev.depends
  then `Unchanged prev
  else
    let version = next_version entries ~handle in
    let content =
      render_opam ~synopsis:(default_synopsis handle) ~url ~commit ~ref_
        ~depends ~display_name:None ~origin_url:None
    in
    let opam_path = write_entry ~fs ~path ~handle ~version content in
    `Bumped { handle; version; url; commit; ref_; depends; opam_path }

let remove ~fs ~path ~handle ?version () =
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
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / pkg_dir))
    matches;
  (* If no versions left, remove the handle directory too. *)
  let handle_dir = path / "packages" / handle in
  if
    Sys.file_exists handle_dir
    && Sys.readdir handle_dir |> Array.length = 0
  then Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / handle_dir)
