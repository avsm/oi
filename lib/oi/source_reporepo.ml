[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(* Reporepo (overlay-of-overlays metadata). Re-exported as [Source.Reporepo]. *)

let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.source.reporepo"

module Log = (val Logs.src_log log_src : Logs.LOG)

let default_path =
  let data_home =
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
  data_home / "reporepo"

let env_path () =
  match Sys.getenv_opt "OI_REPOREPO" with
  | Some v when v <> "" -> v
  | _ -> default_path

let default_url = "https://git.recoil.org/anil.recoil.org/reporepo"

let env_url () =
  match Sys.getenv_opt "OI_REPOREPO_URL" with
  | Some v when v <> "" -> v
  | _ -> default_url

(* -- v2 layout -------------------------------------------------------

   reporepo/
     v2/
       reporepo/                       — meta-entries describing each
         repo                            overlay (handle -> upstream pin).
         packages/<handle>/<handle>.<version>/opam
       <handle>/                       — fully-materialised overlay,
         repo                            content-addressed end-to-end:
         packages/<pkg>/<pkg>.<ver>/opam every url{} block carries a
                                         tarball checksum or sha-pinned
                                         git URL. opam files MAY carry
                                         [x-d10-archive: "<sha>"] pointing
                                         at a consolidated .tar.zst
                                         supplied by [oi repo bump].

   The [v2] prefix is a schema marker. v1 trees are no longer read;
   users with a legacy v1/ reporepo run [oi repo migrate] (or the
   one-shot bash script) to copy the latest version of every package
   into v2/. The shape is the same; only the highest version of each
   package is retained. *)

let schema_dir = "v2"
let v1_root ~path = path / schema_dir
let meta_root ~path = v1_root ~path / "reporepo"
let meta_packages_dir ~path = meta_root ~path / "packages"
let overlay_dir ~path ~handle = v1_root ~path / handle
let overlay_packages_dir ~path ~handle = overlay_dir ~path ~handle / "packages"

let iter_opam_files ~path ?(include_handles = []) ?(skip_handles = []) f =
  let v1 = v1_root ~path in
  if not (Sys.file_exists v1) then ()
  else
    let sorted_subdirs root =
      if not (Sys.file_exists root) then []
      else
        Sys.readdir root |> Array.to_list
        |> List.filter (fun n ->
            (not (String.starts_with ~prefix:"." n))
            && Sys.is_directory (root / n))
        |> List.sort String.compare
    in
    let handle_ok h =
      (* [v2/reporepo/] is the meta-overlay holding handle-registration
         entries, not an archive-bearing overlay. *)
      h <> "reporepo"
      && (include_handles = [] || List.mem h include_handles)
      && not (List.mem h skip_handles)
    in
    let strip_pkg_prefix pkg pkg_ver_dir =
      let prefix = pkg ^ "." in
      if String.starts_with ~prefix pkg_ver_dir then
        String.sub pkg_ver_dir (String.length prefix)
          (String.length pkg_ver_dir - String.length prefix)
      else pkg_ver_dir
    in
    sorted_subdirs v1 |> List.filter handle_ok
    |> List.iter (fun handle ->
        let pkgs_dir = overlay_packages_dir ~path ~handle in
        sorted_subdirs pkgs_dir
        |> List.iter (fun pkg ->
            let pkg_dir = pkgs_dir / pkg in
            sorted_subdirs pkg_dir
            |> List.filter (fun pv -> Sys.file_exists (pkg_dir / pv / "opam"))
            |> List.iter (fun pkg_ver_dir ->
                let opam_path = pkg_dir / pkg_ver_dir / "opam" in
                let version = strip_pkg_prefix pkg pkg_ver_dir in
                f ~handle ~pkg ~version ~opam_path)))

type entry = {
  handle : string;
  version : string;
  url : string;
  commit : string;
  ref_ : string option;
  toolchain : string option;
  toolchain_name : string option;
  toolchain_compiler : string option;
  relocatable : bool option;
  toolchain_roots : string list list;
  toolchain_tools : string list;
  default_toolchain : bool;
  depends : (string * string option) list;
  root_packages : string list list;
  opam_path : string;
}

type root = { handle : string; version : string option }

let mkdir_p ~fs d =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / d)

let write_file ~fs path content =
  mkdir_p ~fs (Filename.dirname path);
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / path) content

(* Default age threshold for auto-pulling the reporepo when [--refresh]
   wasn't explicitly passed. Overridable via [OI_REPOREPO_MAX_AGE]
   (positive float seconds; non-positive disables). *)
let default_auto_refresh_after_s = 3600.

let auto_refresh_after_s () =
  match Sys.getenv_opt "OI_REPOREPO_MAX_AGE" with
  | None | Some "" -> default_auto_refresh_after_s
  | Some v -> (
      match float_of_string_opt v with
      | Some f -> f
      | None -> default_auto_refresh_after_s)

(* Age of the most recent reporepo pull in seconds, or [None] if the
   path isn't a usable clone yet.

   [FETCH_HEAD] is touched by every [git fetch] / [git pull], so its
   mtime tracks "last upstream sync" precisely. Falling back to
   [.git/HEAD] covers a freshly-cloned repo (clone writes HEAD but not
   FETCH_HEAD), and on the way down we silently treat any [Unix.stat]
   failure as "unknown age" — the caller's existing-state path then
   runs unchanged. *)
let last_refresh_age_s ~now path =
  let try_stat p =
    try Some (Unix.stat p).st_mtime with Unix.Unix_error _ -> None
  in
  let mtime =
    match try_stat (path / ".git" / "FETCH_HEAD") with
    | Some m -> Some m
    | None -> try_stat (path / ".git" / "HEAD")
  in
  Option.map (fun m -> now -. m) mtime

let ensure_clone ?(reporter = Build_progress.null) ~fs ~sys ~refresh ~path ~url
    () =
  let dot_git = path / ".git" in
  if Sys.file_exists dot_git then
    begin if refresh then begin
      Log.info (fun m -> m "Refreshing reporepo at %s" path);
      reporter.Build_progress.event (Status "Refreshing reporepo");
      try
        Retry.with_attempts ~label:(Fmt.str "git pull reporepo at %s" path)
          (fun () ->
            D10.Sysops.Cmd.run sys [ "git"; "-C"; path; "pull"; "--ff-only" ])
      with Eio.Exn.Io _ | Failure _ ->
        Log.warn (fun m ->
            m
              "Failed to refresh reporepo at %s — continuing with existing \
               state. Resolve local changes and retry if this matters."
              path)
    end
    end
  else if
    Sys.file_exists path && Sys.is_directory path
    && Array.length (Sys.readdir path) > 0
  then
    Error.fail_config_error
      "reporepo path %s exists but is not a git clone; move or remove it and \
       retry"
      path
  else begin
    mkdir_p ~fs (Filename.dirname path);
    Log.info (fun m -> m "Cloning reporepo from %s to %s" url path);
    reporter.Build_progress.event (Status "Cloning reporepo");
    try
      Fmt.kstr
        (fun label ->
          Retry.with_attempts ~label (fun () ->
              D10.Sysops.Cmd.run sys [ "git"; "clone"; url; path ]))
        "git clone %s" url
    with Eio.Exn.Io _ | Failure _ ->
      Error.fail_config_error "failed to clone reporepo from %s into %s" url
        path
  end

let git_at ~sys ~path args =
  D10.Sysops.Cmd.run sys ("git" :: "-C" :: path :: args)

let git_at_out ~sys ~path args =
  D10.Sysops.Cmd.run_out sys ("git" :: "-C" :: path :: args)

let git_at_inherit ~sys ~path args =
  D10.Sysops.Cmd.run_inherit sys ("git" :: "-C" :: path :: args)

let assert_clone path =
  if not (Sys.file_exists (path / ".git")) then
    Error.fail_config_error
      "reporepo at %s is not a git working copy — run an [oi repo] subcommand \
       first to bootstrap the clone"
      path

let set_push_url ~sys ~path url =
  assert_clone path;
  git_at ~sys ~path [ "remote"; "set-url"; "--push"; "origin"; url ]

type push_step =
  | Step_commit of { files : string list }
  | Step_pull of { commits : int }
  | Step_push of { commits : int }

type push_outcome = push_step list

let commit_count ~sys ~path ~base ~head =
  if base = "" || head = "" || base = head then 0
  else
    try
      int_of_string
        (git_at_out ~sys ~path [ "rev-list"; "--count"; base ^ ".." ^ head ])
    with Failure _ | Eio.Exn.Io _ -> 0

let commit_dirty ~sys ~path ~msg () =
  assert_clone path;
  let porcelain = git_at_out ~sys ~path [ "status"; "--porcelain" ] in
  let dirty_paths =
    porcelain |> String.split_on_char '\n'
    |> List.filter_map (fun line ->
        if String.length line < 4 then None
        else Some (String.sub line 3 (String.length line - 3)))
  in
  if dirty_paths = [] then []
  else begin
    git_at ~sys ~path [ "add"; "-A" ];
    git_at_inherit ~sys ~path [ "commit"; "-m"; msg ];
    dirty_paths
  end

let push ?(on_step_start = fun _ _ -> ()) ~sys ~path () =
  on_step_start 1 "commit local changes";
  let commit_step =
    let porcelain = git_at_out ~sys ~path [ "status"; "--porcelain" ] in
    let dirty_paths =
      porcelain |> String.split_on_char '\n'
      |> List.filter_map (fun line ->
          if String.length line < 4 then None
          else Some (String.sub line 3 (String.length line - 3)))
    in
    if dirty_paths = [] then Step_commit { files = [] }
    else
      let summary =
        if List.length dirty_paths = 1 then List.hd dirty_paths
        else Fmt.str "%d files" (List.length dirty_paths)
      in
      let msg =
        Fmt.str
          "oi repo push: local edits (%s)\n\n\
           Auto-committed by `oi repo push`. Files:\n\
           %s"
          summary
          (String.concat "\n" (List.map (fun p -> "  " ^ p) dirty_paths))
      in
      let _ = commit_dirty ~sys ~path ~msg () in
      Step_commit { files = dirty_paths }
  in
  on_step_start 2 "pull --rebase from upstream";
  let head_before = git_at_out ~sys ~path [ "rev-parse"; "HEAD" ] in
  git_at_inherit ~sys ~path [ "pull"; "--rebase" ];
  let head_after = git_at_out ~sys ~path [ "rev-parse"; "HEAD" ] in
  let pull_step =
    Step_pull
      { commits = commit_count ~sys ~path ~base:head_before ~head:head_after }
  in
  on_step_start 3 "push to origin";
  let push_step =
    let local = git_at_out ~sys ~path [ "rev-parse"; "@" ] in
    let remote =
      try git_at_out ~sys ~path [ "rev-parse"; "@{u}" ]
      with Eio.Exn.Io _ -> ""
    in
    let ahead = commit_count ~sys ~path ~base:remote ~head:local in
    if ahead = 0 then Step_push { commits = 0 }
    else begin
      git_at_inherit ~sys ~path [ "push" ];
      Step_push { commits = ahead }
    end
  in
  [ commit_step; pull_step; push_step ]

let split_url_commit s =
  match String.index_opt s '#' with
  | None -> (s, "")
  | Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

let is_overlay_extension extensions =
  match OpamStd.String.Map.find_opt Keys.overlay extensions with
  | None -> false
  | Some v -> (
      match v.OpamParserTypes.FullPos.pelem with
      | OpamParserTypes.FullPos.Bool true -> true
      | _ -> false)

let read_string_extension extensions name =
  match OpamStd.String.Map.find_opt name extensions with
  | None -> None
  | Some v -> (
      match v.OpamParserTypes.FullPos.pelem with
      | OpamParserTypes.FullPos.String s -> Some s
      | _ -> None)

let read_bool_extension ~path extensions name =
  match OpamStd.String.Map.find_opt name extensions with
  | None -> None
  | Some v -> (
      match v.OpamParserTypes.FullPos.pelem with
      | OpamParserTypes.FullPos.Bool b -> Some b
      | _ -> Error.fail_config_error "%s: %s must be a boolean" path name)

let read_string_list_extension ~path extensions name =
  match OpamStd.String.Map.find_opt name extensions with
  | None -> []
  | Some v -> (
      match v.OpamParserTypes.FullPos.pelem with
      | OpamParserTypes.FullPos.String s -> [ s ]
      | OpamParserTypes.FullPos.List { pelem = items; _ } ->
          List.map
            (fun (it : OpamParserTypes.FullPos.value) ->
              match it.pelem with
              | OpamParserTypes.FullPos.String s -> s
              | _ ->
                  Error.fail_config_error "%s: %s items must be strings" path
                    name)
            items
      | _ ->
          Error.fail_config_error "%s: %s must be a string or a list of strings"
            path name)

let read_root_packages_extension ~path extensions name =
  let as_string_item (v : OpamParserTypes.FullPos.value) =
    match v.pelem with
    | OpamParserTypes.FullPos.String s -> s
    | _ ->
        Error.fail_config_error "%s: %s nested item must be a string" path name
  in
  match OpamStd.String.Map.find_opt name extensions with
  | None -> []
  | Some v -> (
      match v.OpamParserTypes.FullPos.pelem with
      | OpamParserTypes.FullPos.String s -> [ [ s ] ]
      | OpamParserTypes.FullPos.List { pelem = items; _ } ->
          List.map
            (fun (it : OpamParserTypes.FullPos.value) ->
              match it.pelem with
              | OpamParserTypes.FullPos.String s -> [ s ]
              | OpamParserTypes.FullPos.List { pelem = sub; _ } ->
                  List.map as_string_item sub
              | _ ->
                  Error.fail_config_error
                    "%s: %s item must be a string or a list of strings" path
                    name)
            items
      | _ ->
          Error.fail_config_error
            "%s: %s must be a string, a list of strings, or a list of lists"
            path name)

let pin_version_of_condition cond =
  OpamFormula.fold_left
    (fun acc foc ->
      match (acc, foc) with
      | Some _, _ -> acc
      | None, OpamTypes.Constraint (`Eq, filter) -> (
          match filter with OpamTypes.FString v -> Some v | _ -> None)
      | None, _ -> None)
    None cond

let parse_depends_formula formula =
  let out = ref [] in
  OpamFormula.iter
    (fun (name, cond) ->
      out :=
        (OpamPackage.Name.to_string name, pin_version_of_condition cond) :: !out)
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
        | None -> Error.fail_config_error "%s: missing name field" path
      in
      let version =
        match OpamFile.OPAM.version_opt opam with
        | Some v -> OpamPackage.Version.to_string v
        | None -> Error.fail_config_error "%s: missing version field" path
      in
      let extensions = OpamFile.OPAM.extensions opam in
      let url_bare, commit =
        match OpamFile.OPAM.url opam with
        | Some u -> split_url_commit (OpamUrl.to_string (OpamFile.URL.url u))
        | None -> ("", "")
      in
      let toolchain_name =
        read_string_extension extensions Keys.toolchain_name
      in
      if url_bare = "" && toolchain_name = None then
        Error.fail_config_error "%s: entry needs either a [url:] block or [%s]"
          path Keys.toolchain_name;
      let depends = parse_depends_formula (OpamFile.OPAM.depends opam) in
      let ref_ = read_string_extension extensions Keys.ref in
      let toolchain = read_string_extension extensions Keys.toolchain in
      let toolchain_compiler =
        read_string_extension extensions Keys.toolchain_compiler
      in
      if toolchain_name <> None && toolchain_compiler = None then
        Error.fail_config_error
          "%s: %s is set but %s is missing — every toolchain definition must \
           declare its compiler package (e.g. \"ocaml-base-compiler.5.4.1\")"
          path Keys.toolchain_name Keys.toolchain_compiler;
      let relocatable = read_bool_extension ~path extensions Keys.relocatable in
      let toolchain_roots =
        read_root_packages_extension ~path extensions Keys.toolchain_roots
      in
      let toolchain_tools =
        read_string_list_extension ~path extensions Keys.toolchain_tools
      in
      let default_toolchain =
        match read_bool_extension ~path extensions Keys.default_toolchain with
        | Some b -> b
        | None -> false
      in
      if default_toolchain && toolchain_name = None then
        Error.fail_config_error
          "%s: %s is only meaningful on toolchain definitions (entries with %s \
           set)"
          path Keys.default_toolchain Keys.toolchain_name;
      let root_packages =
        read_root_packages_extension ~path extensions Keys.root_packages
      in
      Some
        {
          handle;
          version;
          url = url_bare;
          commit;
          ref_;
          toolchain;
          toolchain_name;
          toolchain_compiler;
          relocatable;
          toolchain_roots;
          toolchain_tools;
          default_toolchain;
          depends;
          root_packages;
          opam_path = path;
        }
  with
  | Error.E _ as e -> raise e
  | exn ->
      Log.warn (fun m -> m "Skipping %s: %s" path (Printexc.to_string exn));
      None

let version_compare a b =
  OpamPackage.Version.compare
    (OpamPackage.Version.of_string a)
    (OpamPackage.Version.of_string b)

let load ~path =
  let packages_dir = meta_packages_dir ~path in
  if not (Sys.file_exists packages_dir) then []
  else
    let entries =
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
    in
    (* Default-flag check: ask "for each handle's LATEST version, is
       it flagged?". Older versions don't count — bumping a
       toolchain to clear the flag must not be defeated by the
       original-flagged entry still on disk. *)
    let latest_per_handle =
      entries
      |> List.fold_left
           (fun acc (e : entry) ->
             match List.assoc_opt e.handle acc with
             | None -> (e.handle, e) :: acc
             | Some prev when version_compare e.version prev.version > 0 ->
                 (e.handle, e) :: List.remove_assoc e.handle acc
             | Some _ -> acc)
           []
    in
    let defaults =
      latest_per_handle
      |> List.filter_map (fun (_, (e : entry)) ->
          if e.default_toolchain then Some e.handle else None)
      |> List.sort String.compare
    in
    (match defaults with
    | [] | [ _ ] -> ()
    | many ->
        Error.fail_config_error
          "reporepo at %s has %d toolchains marked as default (%s: true): %s. \
           Exactly one toolchain may be the default — clear the flag on the \
           others."
          path (List.length many) Keys.default_toolchain
          (String.concat ", " many));
    entries

let latest entries ~handle =
  entries
  |> List.filter (fun (e : entry) -> e.handle = handle)
  |> List.sort (fun (a : entry) (b : entry) ->
      version_compare b.version a.version)
  |> function
  | x :: _ -> Some x
  | [] -> None

let find entries ~handle ~version =
  List.find_opt
    (fun (e : entry) -> e.handle = handle && e.version = version)
    entries

let default_toolchain entries =
  let candidates =
    entries
    |> List.filter (fun (e : entry) -> e.toolchain_name <> None)
    |> List.fold_left
         (fun acc (e : entry) ->
           match List.assoc_opt e.handle acc with
           | None -> (e.handle, e) :: acc
           | Some prev when version_compare e.version prev.version > 0 ->
               (e.handle, e) :: List.remove_assoc e.handle acc
           | Some _ -> acc)
         []
  in
  candidates
  |> List.filter_map (fun (_, (e : entry)) ->
      if e.default_toolchain then Some e else None)
  |> function
  | [] -> None
  | e :: _ -> Some e

let topo_sort entries =
  let by_handle = Hashtbl.create 16 in
  List.iter (fun (e : entry) -> Hashtbl.replace by_handle e.handle e) entries;
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

module Inst = Opam_0install.Solver.Make (Opam_0install.Dir_context)

let resolve entries ~roots =
  if roots = [] then []
  else
    let path = env_path () in
    let packages_dir = meta_packages_dir ~path in
    if not (Sys.file_exists packages_dir) then
      Error.fail_config_error
        "reporepo at %s has no v2/reporepo/packages/ tree (run 'oi repo add' \
         or migrate from the old layout)"
        path;
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
    let env _ = None in
    let ctx = Opam_0install.Dir_context.create ~constraints ~env packages_dir in
    let names =
      List.map (fun (r : root) -> OpamPackage.Name.of_string r.handle) roots
    in
    Log.debug (fun m ->
        m "0install solve over %s for roots: %s" packages_dir
          (String.concat ", " (List.map OpamPackage.Name.to_string names)));
    match Inst.solve ctx names with
    | Error diag ->
        Error.fail_config_error "overlay resolution failed: %s"
          (Inst.diagnostics diag)
    | Ok sels ->
        let pkgs = Inst.packages_of_result sels in
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
                m "  resolved %s.%s@%s via %s" e.handle e.version short e.url))
          sorted;
        sorted

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

let assert_overlay_dir ~path ~handle =
  let dir = overlay_packages_dir ~path ~handle in
  if not (Sys.file_exists dir) then
    Error.fail_config_error
      "overlay %s not materialised in reporepo at %s (missing %s); run 'oi \
       repo bump %s' (or 'oi repo bump --all' to populate every overlay)"
      handle path dir handle;
  dir

let ensure_base ~fs ~sys ~data_dir:_ ?(refresh = false)
    ?(reporter = Build_progress.null) () =
  let path = env_path () in
  Fmt.kstr (fun s -> reporter.event (Status s)) "Reporepo at %s" path;
  (* Auto-pull when the existing clone hasn't seen [git fetch] for
     longer than [auto_refresh_after_s]. Skipped when [refresh] is
     already requested (avoid the double-pull), when the clone
     doesn't exist yet (the clone path itself counts as a fresh
     fetch), or when the threshold is non-positive. *)
  let effective_refresh =
    if refresh then true
    else
      let threshold = auto_refresh_after_s () in
      if threshold <= 0. then false
      else
        match last_refresh_age_s ~now:(Unix.time ()) path with
        | Some age when age > threshold ->
            Log.info (fun m ->
                m "Reporepo at %s is %.0fs old (>%.0fs); auto-refreshing" path
                  age threshold);
            true
        | _ -> false
  in
  ensure_clone ~reporter ~fs ~sys ~refresh:effective_refresh ~path
    ~url:(env_url ()) ();
  Log.debug (fun m -> m "ensure_base: reading reporepo %s" path);
  let entries = try load ~path with Error.E _ -> [] in
  match latest entries ~handle:"relocatable" with
  | None ->
      Error.fail_config_error
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
               (List.map (fun (e : entry) -> e.handle ^ "." ^ e.version) base)));
      List.filter_map
        (fun (e : entry) ->
          if e.url = "" then None
          else Some (assert_overlay_dir ~path ~handle:e.handle))
        base

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
        D10.Sysops.Cmd.run_out_quiet sys ("git" :: "ls-remote" :: url :: args))
  with exn ->
    Error.fail_config_error "git ls-remote %s failed: %s" url
      (Printexc.to_string exn)

let ls_remote_sha ~sys ?ref_ url =
  match ref_ with
  | Some r -> (
      let out = try_ls_remote ~sys url [ r ] in
      match parse_ls_remote_output out with
      | (sha, _) :: _ -> sha
      | [] -> Error.fail_config_error "git ls-remote %s %s: ref not found" url r
      )
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
                      Error.fail_config_error
                        "git ls-remote %s returned no refs" url))))

let today_yyyymmdd () =
  let tm = Unix.gmtime (Unix.time ()) in
  Fmt.str "%04d%02d%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

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

let render_root_groups buf field groups =
  match groups with
  | [] -> ()
  | groups ->
      let pp = Fmt.with_buffer buf in
      Fmt.pf pp "%s: [\n" field;
      List.iter
        (fun group ->
          match group with
          | [] -> ()
          | [ one ] -> Fmt.pf pp "  %s\n" (escape_string one)
          | multi ->
              Buffer.add_string buf "  [";
              List.iter (fun p -> Fmt.pf pp " %s" (escape_string p)) multi;
              Buffer.add_string buf " ]\n")
        groups;
      Buffer.add_string buf "]\n"

type toolchain_def = {
  td_name : string;
  td_compiler : string option;
  td_relocatable : bool option;
  td_roots : string list list;
  td_tools : string list;
  td_default : bool;
}

let render_opam ~synopsis ~url ~commit ~ref_ ~toolchain ?toolchain_def ~depends
    ~root_packages () =
  let buf = Buffer.create 512 in
  let pp = Fmt.with_buffer buf in
  Fmt.pf pp "opam-version: \"2.0\"\n";
  Fmt.pf pp "synopsis: %s\n" (escape_string synopsis);
  if url <> "" then
    Fmt.pf pp "url {\n  src: %s\n}\n"
      (escape_string (if commit = "" then url else url ^ "#" ^ commit));
  (match depends with
  | [] -> ()
  | ds ->
      Buffer.add_string buf "depends: [\n";
      List.iter
        (fun (h, v) ->
          match v with
          | Some ver ->
              Fmt.pf pp "  %s { = %s }\n" (escape_string h) (escape_string ver)
          | None -> Fmt.pf pp "  %s\n" (escape_string h))
        ds;
      Buffer.add_string buf "]\n");
  Fmt.pf pp "%s: true\n" Keys.overlay;
  Stdlib.Option.iter
    (fun s -> Fmt.pf pp "%s: %s\n" Keys.ref (escape_string s))
    ref_;
  Stdlib.Option.iter
    (fun s -> Fmt.pf pp "%s: %s\n" Keys.toolchain (escape_string s))
    toolchain;
  Stdlib.Option.iter
    (fun (td : toolchain_def) ->
      Fmt.pf pp "%s: %s\n" Keys.toolchain_name (escape_string td.td_name);
      Stdlib.Option.iter
        (fun s ->
          Fmt.pf pp "%s: %s\n" Keys.toolchain_compiler (escape_string s))
        td.td_compiler;
      Stdlib.Option.iter
        (fun b -> Fmt.pf pp "%s: %b\n" Keys.relocatable b)
        td.td_relocatable;
      if td.td_default then Fmt.pf pp "%s: true\n" Keys.default_toolchain;
      render_root_groups buf Keys.toolchain_roots td.td_roots;
      match td.td_tools with
      | [] -> ()
      | tools ->
          Fmt.pf pp "%s: [\n" Keys.toolchain_tools;
          List.iter (fun t -> Fmt.pf pp "  %s\n" (escape_string t)) tools;
          Buffer.add_string buf "]\n")
    toolchain_def;
  render_root_groups buf Keys.root_packages root_packages;
  Buffer.contents buf

let write_entry ~fs ~path ~handle ~version content =
  let pkg_dir = meta_packages_dir ~path / handle / (handle ^ "." ^ version) in
  let opam_path = pkg_dir / "opam" in
  write_file ~fs opam_path content;
  opam_path

let ensure_repo_marker ~fs ~path =
  let marker = meta_root ~path / "repo" in
  if not (Sys.file_exists marker) then
    write_file ~fs marker "opam-version: \"2.0\"\n"

let default_synopsis handle = "Overlay: " ^ handle ^ " — pinned opam repository"

let is_base_handle h = h = "default" || h = "relocatable"
let default_base_handles = [ "relocatable"; "default" ]

let auto_base_depends entries ~handle ~base_handles =
  if is_base_handle handle then []
  else
    List.filter_map
      (fun h ->
        if h = handle then
          None
          (* A handle never depends on itself. Filtering here keeps
             [base_handles_of_toolchain] callers safe even when the
             toolchain definition's own [depends:] lists the overlay
             that anchors its compiler (e.g. [toolchain-oxcaml]
             depends on [oxcaml]); bumping the [oxcaml] overlay must
             not generate an [oxcaml = ...] self-dep. *)
        else
          match latest entries ~handle:h with
          | Some e -> Some (h, Some e.version)
          | None -> None)
      base_handles

(* -- v1 overlay materialisation -------------------------------------- *)

(* Hardcoded — see design notes. Bumps are run interactively and the
   bottleneck is one [git ls-remote] per package; eight in flight is a
   comfortable concurrency without overwhelming a small reporepo's
   upstream hosts. *)
let max_concurrent_url_resolutions = 8

let is_sha_string s =
  String.length s = 40
  && String.for_all
       (function '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false)
       s

(* Classify [u] for v1 materialisation. Reporepo entries are
   git-or-tarball only; project pin-depends additionally accept
   [file://] paths (opam's [rsync] backend) so a bundle of locally
   extracted sources can pin-depends-back to itself. [`Local] is
   not produced for reporepo URLs — those still hard-error
   downstream — but [try_resolve_url] short-circuits it as a
   [Keep] since a local-dir pin doesn't need URL resolution. *)
let classify_url ~where (u : OpamUrl.t) : [ `Git | `Tarball | `Local ] =
  match u.OpamUrl.backend with
  | `git -> `Git
  | `http -> `Tarball
  | `rsync -> `Local
  | (`darcs | `hg) as b ->
      let name = match b with `darcs -> "darcs" | `hg -> "hg" in
      Error.fail_config_error
        "%s: %s URLs are not supported in v1 materialisation (URL: %s)" where
        name (OpamUrl.to_string u)

(* Resolve a single URL into a content-addressed form. The caller maps
   the polymorphic-variant outcome to whatever opam-file mutation it
   needs; the three cases are: keep as-is, replace the URL (sha pin),
   add a checksum to a tarball that lacked one, or report why we
   couldn't pin it. *)
let try_resolve_url ~fs ~sys ~where (u : OpamUrl.t) ~has_checksum :
    [ `Keep
    | `Replace_url of OpamUrl.t
    | `Add_checksum of OpamHash.t
    | `Failed of string ] =
  match classify_url ~where u with
  | `Local ->
      (* Local-directory pins (opam [rsync] backend, [file://]
         URLs) don't need resolution — the path on disk is already
         the source of truth. [Pin.fetch_pin] later calls
         [OpamRepository.pull_tree] which handles the rsync-copy
         itself. *)
      `Keep
  | `Git ->
      begin match u.hash with
      | Some sha when is_sha_string sha -> `Keep
      | ref_opt -> begin
          (* git(1) doesn't understand opam's [git+https://...]
             spelling — strip the [git+] backend prefix and feed it the
             plain [transport://path] form. The rewritten URL we write
             back to the opam file keeps the [git+] form. *)
          let url_for_query = { u with hash = None } in
          let url_str = OpamUrl.base_url url_for_query in
          try
            let sha = ls_remote_sha ~sys ?ref_:ref_opt url_str in
            `Replace_url { u with hash = Some sha }
          with exn ->
            `Failed
              (Fmt.str "git ls-remote %s%s failed: %s" url_str
                 (match ref_opt with None -> "" | Some r -> " " ^ r)
                 (Printexc.to_string exn))
        end
      end
  | `Tarball ->
      if has_checksum then `Keep
      else begin
        let url_str = OpamUrl.to_string u in
        let tmp = Filename.temp_file "oi-bump-tarball-" ".bin" in
        Fun.protect
          ~finally:(fun () -> try Sys.remove tmp with Sys_error _ -> ())
          (fun () ->
            if D10.Sysops.Http.fetch sys ~url:url_str ~dst:Eio.Path.(fs / tmp)
            then `Add_checksum (OpamHash.compute ~kind:`SHA256 tmp)
            else `Failed (Fmt.str "could not fetch tarball at %s" url_str))
      end

type pkg_outcome =
  | Pkg_kept
  | Pkg_rewrote of int (* number of url fields rewritten *)
  | Pkg_unavailable of string

type materialise_summary = {
  handle : string;
  total : int;
  kept : int;
  rewrote : int;
  unavailable : (string * string) list;
}

let set_extension key str opam =
  let v : OpamParserTypes.FullPos.value =
    { pelem = OpamParserTypes.FullPos.String str; pos = OpamTypesBase.pos_null }
  in
  let exts = OpamStd.String.Map.add key v (OpamFile.OPAM.extensions opam) in
  OpamFile.OPAM.with_extensions exts opam

(* Tag the rewritten file with the pre-rewrite URL so that incremental
   bumps can detect "upstream URL didn't change" without re-running
   ls-remote. *)
let stamp_source_url opam original_url =
  set_extension "x-oi-source-url" original_url opam

let mark_unavailable opam ~reason =
  opam
  |> OpamFile.OPAM.with_available (OpamTypes.FBool false)
  |> set_extension "x-oi-unavailable" reason

let process_main_url ~fs ~sys ~package opam =
  match OpamFile.OPAM.url opam with
  | None -> (opam, Pkg_kept, 0)
  | Some file_url -> begin
      let u = OpamFile.URL.url file_url in
      let has_checksum = OpamFile.URL.checksum file_url <> [] in
      let original = OpamUrl.to_string u in
      match try_resolve_url ~fs ~sys ~where:package u ~has_checksum with
      | `Keep -> (stamp_source_url opam original, Pkg_kept, 0)
      | `Replace_url u' ->
          let file_url' = OpamFile.URL.with_url u' file_url in
          let opam' = OpamFile.OPAM.with_url file_url' opam in
          (stamp_source_url opam' original, Pkg_rewrote 1, 1)
      | `Add_checksum h ->
          let file_url' = OpamFile.URL.with_checksum [ h ] file_url in
          let opam' = OpamFile.OPAM.with_url file_url' opam in
          (stamp_source_url opam' original, Pkg_rewrote 1, 1)
      | `Failed reason ->
          (* Lenient fallback: a transient ls-remote / tarball-fetch
             failure should not poison the package. Keep the original
             URL — opam will attempt the clone/download at build time
             and fail there if it really is broken. The package stays
             solvable, which preserves the pre-v1 behaviour for
             overlays whose upstream URLs are flaky. *)
          Log.warn (fun m -> m "%s: leaving URL unpinned (%s)" package reason);
          (stamp_source_url opam original, Pkg_kept, 0)
    end

let process_pin_depends ~fs ~sys ~package opam =
  match OpamFile.OPAM.pin_depends opam with
  | [] -> (opam, 0)
  | pins ->
      (* For pin-depends we only ever rewrite (when ls-remote
         resolves) or keep (when it doesn't, when a tarball has no
         checksum slot, or when there's nothing to add). The package
         stays solvable in every case — opam discovers the broken pin
         at fetch time, matching pre-v1 behaviour. *)
      let resolve_one (pkg, url) =
        let where =
          Fmt.str "%s pin-depends %s" package (OpamPackage.to_string pkg)
        in
        match try_resolve_url ~fs ~sys ~where url ~has_checksum:false with
        | `Keep -> ((pkg, url), 0)
        | `Replace_url u' -> ((pkg, u'), 1)
        | `Add_checksum _ ->
            Log.warn (fun m ->
                m "%s: tarball pin without checksum — leaving unpinned (%s)"
                  where (OpamUrl.to_string url));
            ((pkg, url), 0)
        | `Failed reason ->
            Log.warn (fun m -> m "%s: leaving pin unpinned (%s)" where reason);
            ((pkg, url), 0)
      in
      let rewritten, count =
        List.fold_right
          (fun pin (acc, n) ->
            let pin', delta = resolve_one pin in
            (pin' :: acc, n + delta))
          pins ([], 0)
      in
      (OpamFile.OPAM.with_pin_depends rewritten opam, count)

let process_one_opam ~fs ~sys ~src_opam_path ~dst_opam_path ~package =
  let read_src () =
    OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw src_opam_path))
  in
  let write_dst opam =
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
      Eio.Path.(fs / Filename.dirname dst_opam_path);
    OpamFile.OPAM.write (OpamFile.make (OpamFilename.raw dst_opam_path)) opam
  in
  let opam, outcome =
    try
      let opam = read_src () in
      let opam, main_outcome, main_count =
        process_main_url ~fs ~sys ~package opam
      in
      let opam, pin_count = process_pin_depends ~fs ~sys ~package opam in
      let total = main_count + pin_count in
      let outcome =
        match main_outcome with
        | Pkg_unavailable _ as r -> r
        | Pkg_kept when total = 0 -> Pkg_kept
        | _ -> Pkg_rewrote total
      in
      (opam, outcome)
    with exn ->
      (* Don't let a single bad opam file kill the whole bump — write
         a stub marked unavailable and keep going. *)
      Log.warn (fun m ->
          m "%s: rewrite failed (%s)" package (Printexc.to_string exn));
      let reason = Fmt.str "rewrite error: %s" (Printexc.to_string exn) in
      let stub =
        try mark_unavailable (read_src ()) ~reason
        with Failure _ | Sys_error _ -> OpamFile.OPAM.empty
      in
      (stub, Pkg_unavailable reason)
  in
  write_dst opam;
  outcome

let scratch_clone ~fs ~sys ~url ~commit =
  let scratch = Filename.temp_dir "oi-bump-clone-" "" in
  try
    Retry.with_attempts ~label:(Fmt.str "git clone %s" url) (fun () ->
        D10.Sysops.Cmd.run sys [ "git"; "clone"; "--quiet"; url; scratch ]);
    D10.Sysops.Cmd.run sys
      [ "git"; "-C"; scratch; "checkout"; "--quiet"; commit ];
    scratch
  with exn ->
    (try Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / scratch)
     with Eio.Exn.Io _ -> ());
    Error.fail_config_error "failed to clone %s at %s: %s" url commit
      (Printexc.to_string exn)

(* In an opam-repository layout, [packages/<name>/<name>.<version>/opam].
   The inner directory's basename is already [name.version] — don't
   re-concatenate it. *)
let walk_packages_dir packages_dir =
  if not (Sys.file_exists packages_dir) then []
  else
    Sys.readdir packages_dir |> Array.to_list
    |> List.filter (fun n ->
        Sys.is_directory (packages_dir / n)
        && not (String.starts_with ~prefix:"." n))
    |> List.sort String.compare
    |> List.concat_map (fun pkg ->
        let pkg_dir = packages_dir / pkg in
        Sys.readdir pkg_dir |> Array.to_list
        |> List.filter (fun pv ->
            Sys.is_directory (pkg_dir / pv)
            && Sys.file_exists (pkg_dir / pv / "opam"))
        |> List.sort String.compare
        |> List.map (fun pkg_ver_dir -> (pkg, pkg_ver_dir)))

(* Hardlink (or fall back to byte copy across filesystems) every regular
   file under [src] into [dst]. Used to mirror a package's [files/]
   directory from the upstream clone into the materialised v2/ tree. *)
let hardlink_or_copy_one ~fs s d =
  if Sys.file_exists s && not (Sys.is_directory s) then
    try Unix.link s d
    with Unix.Unix_error _ ->
      let bytes = Eio.Path.load Eio.Path.(fs / s) in
      Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / d) bytes

let copy_files_dir ~fs ~src ~dst =
  if Sys.file_exists src && Sys.is_directory src then begin
    mkdir_p ~fs dst;
    Sys.readdir src
    |> Array.iter (fun name ->
        hardlink_or_copy_one ~fs (src / name) (dst / name))
  end

(* Process every package in the scratch clone into the materialised tree:
   rewrites each [url{}] block to a content-addressed form, copies [files/]
   directories. Returns a per-version outcome list. *)
let process_clone_packages ~fs ~sys ~scratch_packages ~tmp_packages pkgs =
  Eio.Fiber.List.map ~max_fibers:max_concurrent_url_resolutions
    (fun (pkg, pkg_ver) ->
      let src = scratch_packages / pkg / pkg_ver / "opam" in
      let dst = tmp_packages / pkg / pkg_ver / "opam" in
      let outcome =
        process_one_opam ~fs ~sys ~src_opam_path:src ~dst_opam_path:dst
          ~package:pkg_ver
      in
      copy_files_dir ~fs
        ~src:(scratch_packages / pkg / pkg_ver / "files")
        ~dst:(tmp_packages / pkg / pkg_ver / "files");
      (pkg_ver, outcome))
    pkgs

let tally_outcomes outcomes =
  List.fold_left
    (fun (k, r, u) (pkg_ver, outcome) ->
      match outcome with
      | Pkg_kept -> (k + 1, r, u)
      | Pkg_rewrote _ -> (k, r + 1, u)
      | Pkg_unavailable reason -> (k, r, (pkg_ver, reason) :: u))
    (0, 0, []) outcomes

let materialise_handle ~fs ~sys ~path ~handle ~url ~commit =
  if commit = "" || url = "" then
    Error.fail_config_error
      "cannot materialise overlay %s: missing url or commit (definition-only \
       entries should not call this)"
      handle;
  let target_root = overlay_dir ~path ~handle in
  let tmp_root = target_root ^ ".tmp" in
  let tmp_packages = tmp_root / "packages" in
  Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / tmp_root);
  mkdir_p ~fs tmp_packages;
  write_file ~fs (tmp_root / "repo") "opam-version: \"2.0\"\n";
  let scratch = scratch_clone ~fs ~sys ~url ~commit in
  Fun.protect ~finally:(fun () ->
      try Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / scratch)
      with Eio.Exn.Io _ -> ())
  @@ fun () ->
  let scratch_packages = scratch / "packages" in
  let pkgs = walk_packages_dir scratch_packages in
  Log.info (fun m ->
      m "Materialising %s: %d package versions" handle (List.length pkgs));
  let outcomes =
    process_clone_packages ~fs ~sys ~scratch_packages ~tmp_packages pkgs
  in
  Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / target_root);
  Unix.rename tmp_root target_root;
  let kept, rewrote, unavailable = tally_outcomes outcomes in
  {
    handle;
    total = List.length outcomes;
    kept;
    rewrote;
    unavailable = List.rev unavailable;
  }

let add ~fs ~sys ~path ~handle ~url ?ref_ ?toolchain ?base_handles ?depends
    ?(root_packages = []) ?synopsis ?(force = false) () =
  let entries = load ~path in
  let handle_exists =
    List.exists (fun (e : entry) -> e.handle = handle) entries
  in
  if handle_exists && not force then
    Error.fail_config_error
      "overlay %s already exists in reporepo; use 'oi repo bump' to add a new \
       version, or pass --force to register a new entry with a different URL"
      handle;
  let base_handles =
    Stdlib.Option.value base_handles ~default:default_base_handles
  in
  let depends =
    match depends with
    | Some d -> d
    | None -> auto_base_depends entries ~handle ~base_handles
  in
  let commit = ls_remote_sha ~sys ?ref_ url in
  let version =
    if handle_exists then next_version entries ~handle
    else today_yyyymmdd () ^ ".0"
  in
  let synopsis =
    Stdlib.Option.value synopsis ~default:(default_synopsis handle)
  in
  let content =
    render_opam ~synopsis ~url ~commit ~ref_ ~toolchain ~depends ~root_packages
      ()
  in
  ensure_repo_marker ~fs ~path;
  let opam_path = write_entry ~fs ~path ~handle ~version content in
  {
    handle;
    version;
    url;
    commit;
    ref_;
    toolchain;
    toolchain_name = None;
    toolchain_compiler = None;
    relocatable = None;
    toolchain_roots = [];
    toolchain_tools = [];
    default_toolchain = false;
    depends;
    root_packages;
    opam_path;
  }

(* Compute the bumped [depends] list. Auto-derive from [base_handles] (or the
   defaults), then preserve any extras [prev] carried — bumped to their
   current latest version so a fresh bump never leaves a stale pin. *)
let compute_bump_depends ~entries ~handle ~base_handles ~prev_depends depends =
  let auto_derive_depends ~auto ~prev =
    let auto_handles = List.map fst auto in
    let extras_from_prev =
      List.filter_map
        (fun (h, _) ->
          if List.mem h auto_handles then None
          else
            match latest entries ~handle:h with
            | Some e -> Some (h, Some e.version)
            | None -> None)
        prev
    in
    auto @ extras_from_prev
  in
  match depends with
  | Some d -> d
  | None ->
      if is_base_handle handle then prev_depends
      else
        let auto =
          match base_handles with
          | Some bh -> auto_base_depends entries ~handle ~base_handles:bh
          | None ->
              auto_base_depends entries ~handle
                ~base_handles:default_base_handles
        in
        if auto = [] then prev_depends
        else auto_derive_depends ~auto ~prev:prev_depends

let bump_unchanged ~url ~commit ~ref_ ~toolchain ~default_toolchain ~depends
    ~root_packages (prev : entry) =
  let eq_as_set a b = List.sort compare a = List.sort compare b in
  url = prev.url && commit = prev.commit && ref_ = prev.ref_
  && toolchain = prev.toolchain
  && default_toolchain = prev.default_toolchain
  && eq_as_set depends prev.depends
  && eq_as_set root_packages prev.root_packages

let toolchain_def_of_prev ~default_toolchain (prev : entry) =
  match prev.toolchain_name with
  | None -> None
  | Some name ->
      Some
        {
          td_name = name;
          td_compiler = prev.toolchain_compiler;
          td_relocatable = prev.relocatable;
          td_roots = prev.toolchain_roots;
          td_tools = prev.toolchain_tools;
          td_default = default_toolchain;
        }

let bump ~fs ~sys ~path ~handle ?url ?ref_ ?toolchain ?base_handles ?depends
    ?root_packages ?default () =
  let entries = load ~path in
  let prev =
    match latest entries ~handle with
    | Some e -> e
    | None ->
        Error.fail_config_error
          "overlay %s not in reporepo; use 'oi repo add' to create it" handle
  in
  if default <> None && prev.toolchain_name = None then
    Error.fail_config_error
      "overlay %s is not a toolchain definition (no %s) — the --default flag \
       only applies to toolchain entries"
      handle Keys.toolchain_name;
  let default_toolchain =
    Stdlib.Option.value default ~default:prev.default_toolchain
  in
  let url = Stdlib.Option.value url ~default:prev.url in
  let ref_ = match ref_ with Some _ -> ref_ | None -> prev.ref_ in
  let toolchain =
    match toolchain with Some _ -> toolchain | None -> prev.toolchain
  in
  let commit = if url = "" then prev.commit else ls_remote_sha ~sys ?ref_ url in
  let depends =
    compute_bump_depends ~entries ~handle ~base_handles
      ~prev_depends:prev.depends depends
  in
  let root_packages =
    match root_packages with Some r -> r | None -> prev.root_packages
  in
  if
    bump_unchanged ~url ~commit ~ref_ ~toolchain ~default_toolchain ~depends
      ~root_packages prev
  then `Unchanged prev
  else
    let version = next_version entries ~handle in
    let toolchain_def = toolchain_def_of_prev ~default_toolchain prev in
    let content =
      render_opam ~synopsis:(default_synopsis handle) ~url ~commit ~ref_
        ~toolchain ?toolchain_def ~depends ~root_packages ()
    in
    let opam_path = write_entry ~fs ~path ~handle ~version content in
    `Bumped
      {
        handle;
        version;
        url;
        commit;
        ref_;
        toolchain;
        toolchain_name = prev.toolchain_name;
        toolchain_compiler = prev.toolchain_compiler;
        relocatable = prev.relocatable;
        toolchain_roots = prev.toolchain_roots;
        toolchain_tools = prev.toolchain_tools;
        default_toolchain;
        depends;
        root_packages;
        opam_path;
      }

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
    Error.fail_config_error "no such overlay %s%s in reporepo" handle
      (match version with None -> "" | Some v -> "." ^ v);
  List.iter
    (fun (e : entry) ->
      let pkg_dir = Filename.dirname e.opam_path in
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / pkg_dir))
    matches;
  let handle_dir = meta_packages_dir ~path / handle in
  if Sys.file_exists handle_dir && Sys.readdir handle_dir |> Array.length = 0
  then Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / handle_dir)
