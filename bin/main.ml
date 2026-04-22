[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

open Cmdliner

let ( / ) = Filename.concat
let app_name = "oi"
let log_src = Logs.Src.create "oi.cli"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* -- Common terms -------------------------------------------------------- *)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Progress.logs_reporter ())

let log_term =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let data_dir_term =
  let app_upper = String.uppercase_ascii app_name in
  let app_env = app_upper ^ "_DATA_DIR" in
  let xdg_var = "XDG_DATA_HOME" in
  let home = Sys.getenv "HOME" in
  let default_path = home / ".local" / "share" / app_name in
  let doc =
    Fmt.str
      "Override data directory. Can also be set with %s or %s. Default: %s"
      app_env xdg_var default_path
  in
  let arg =
    Arg.(value & opt string default_path & info ~docv:"DIR" ~doc [ "data-dir" ])
  in
  Term.(
    const (fun cmdline_val ->
        if cmdline_val <> default_path then cmdline_val
        else
          match Sys.getenv_opt app_env with
          | Some v when v <> "" -> v
          | _ -> (
              match Sys.getenv_opt xdg_var with
              | Some v when v <> "" -> v / app_name
              | _ -> default_path))
    $ arg)

let cache_dir_term = Xdge.Cmd.cache_term app_name

let refresh_term =
  Arg.(
    value & flag
    & info
        ~doc:
          "Force refresh of opam repos, pins, and URL clones. Caches older \
           than 24 hours refresh automatically."
        [ "refresh" ])

(* Common CLI flag: [--with-repo URL], repeatable. Available on every
   command that solves — extras are merged with project-declared
   [x-opam-repositories:] at the call site. *)
let with_repos_term =
  Arg.(
    value & opt_all string []
    & info ~docv:"URL"
        ~doc:
          "Additional opam repository to include when solving. Accepts a URL \
           or a reporepo handle. Repeatable."
        [ "with-repo" ])

(* Common CLI flag: [-j N] / [--jobs N]. Available on every command that
   builds — caps concurrent package builds within a stage to bound fd
   and process pressure. Unset here (= None) defers to
   [OI_BUILD_PARALLELISM] and then the executor's default. *)
let jobs_term =
  Arg.(
    value
    & opt (some int) None
    & info ~docv:"N"
        ~doc:"Maximum number of packages to build in parallel. Default 4."
        [ "j"; "jobs" ])

(* Common CLI flag: [--with PKG], repeatable. Available on every command
   that solves — adds extra packages (optionally with version
   constraints, e.g. "fmt>=0.9") to the solver's root set so they end up
   in the built prefix alongside whatever the command would otherwise
   solve. *)
let with_deps_term =
  Arg.(
    value & opt_all string []
    & info ~docv:"PKG"
        ~doc:
          "Additional dependency to include when solving. Accepts a package \
           name, an opam atom ($(b,fmt>=0.9), $(b,dune.3.20.0)), or a git URL. \
           URLs are cloned and every $(b,*.opam) at the root is pinned. \
           Repeatable."
        [ "with" ])

(* Convert a CLI [--with-repo URL] entry into a [Project.extra_repo] with a
   deterministic hashed name. We keep the CLI shape (a list of bare URLs)
   and invent a stable local clone directory name here at the boundary. *)
let cli_extra_repo_of_url url_s : Oi.Project.extra_repo =
  let hash = Digest.string url_s |> Digest.to_hex in
  let name = "extra-" ^ String.sub hash 0 10 in
  { name; url = url_s }

(* Reporepo path: honour [OI_REPOREPO] and otherwise fall back to
   [Oi.Reporepo.default_path]. Looked up fresh per call so tests can
   set the env per invocation. *)
let reporepo_path () =
  match Sys.getenv_opt "OI_REPOREPO" with
  | Some v when v <> "" -> v
  | _ -> Oi.Reporepo.default_path

(* Reporepo clone URL: [OI_REPOREPO_URL] wins, otherwise falls back
   to [Oi.Reporepo.default_url]. *)
let reporepo_url () =
  match Sys.getenv_opt "OI_REPOREPO_URL" with
  | Some v when v <> "" -> v
  | _ -> Oi.Reporepo.default_url

(* Format-style debug logger for overlay / reporepo plumbing. *)
let log_overlay fmt = Fmt.kstr (fun s -> Logs.debug (fun m -> m "%s" s)) fmt

(* A [--with-repo] token is a URL if it contains a scheme-like prefix
   or a path separator; otherwise it's treated as an overlay handle
   and looked up in the reporepo. *)
let is_url_like s =
  List.exists
    (fun p -> String.starts_with ~prefix:p s)
    [ "http://"; "https://"; "git+"; "git://"; "git@"; "file://"; "./"; "/" ]
  || String.contains s '/'

(* Classify every [--with=…] token in one pass: URLs get cloned into
   the pin cache and produce pins + solver roots; opam package specs
   come back as already-parsed {!Oi.Script.dep}. Returns
   [(pkg_deps, url_project)] so callers never need to re-parse or
   re-classify downstream. *)
let materialize_with_deps ~fs ~sys ~cache ?refresh with_deps =
  let urls, pkg_deps = Oi.Url_project.classify_all with_deps in
  let url_project = Oi.Url_project.materialize ~fs ~sys ~cache ?refresh urls in
  (pkg_deps, url_project)

(* Resolve a list of overlay handles to a flat list of extra-repo
   entries, including their transitive overlay deps. Later handles in
   the input list are given highest priority: they come first in the
   output so the solver's first-wins fold favours them. *)
let overlay_extras_of_handles ~fs ~sys handles =
  if handles = [] then []
  else begin
    let path = reporepo_path () in
    let url =
      match Sys.getenv_opt "OI_REPOREPO_URL" with
      | Some v when v <> "" -> v
      | _ -> Oi.Reporepo.default_url
    in
    Oi.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path ~url;
    log_overlay "resolving handles %s against reporepo %s"
      (String.concat ", " handles)
      path;
    let entries = Oi.Reporepo.load ~path in
    let roots =
      List.rev handles
      |> List.map (fun h : Oi.Reporepo.root -> { handle = h; version = None })
    in
    let resolved =
      Oi.Reporepo.resolve entries ~roots
      (* Resolve returns deps-first (topological). The opam solver's
         packages_dirs fold is first-wins on name collisions, so we
         reverse to get dependents-first: [samoht, relocatable, default]
         means samoht wins over relocatable wins over default. *)
      |> List.rev
    in
    log_overlay "overlay closure (highest priority first): %s"
      (String.concat ", "
         (List.map
            (fun (e : Oi.Reporepo.entry) ->
              Fmt.str "%s.%s@%s" e.handle e.version
                (String.sub e.commit 0 (min 7 (String.length e.commit))))
            resolved));
    List.map
      (fun (e : Oi.Reporepo.entry) ->
        let url = if e.commit = "" then e.url else e.url ^ "#" ^ e.commit in
        let name = "overlay-" ^ e.handle ^ "-" ^ e.version in
        { Oi.Project.name; url })
      resolved
  end

let cli_extra_repos ~fs ~sys tokens =
  let urls, handles = List.partition is_url_like tokens in
  overlay_extras_of_handles ~fs ~sys handles
  @ List.map cli_extra_repo_of_url urls

(* A ([@handle/pkg...]) shortcut parsed out of a TARGET or [--with]
   token, once the handle has been routed into [with_repos] and the
   package spec is ready for the solver. Carries the handle alongside
   the package name and any user-supplied constraint so we can later
   pin the package to whatever version the named overlay ships. *)
type handle_pin = {
  handle : string;
  pkg : OpamPackage.Name.t;
  user_constr : OpamFormula.version_constraint option;
}

(* Highest version of [pkg] found across [dirs] (each expected to be
   a [packages/] tree). [None] when the package is absent from all of
   them. The directory layout is standard opam: [packages/<pkg>/<pkg.ver>/opam]. *)
let latest_version_in_dirs ~pkg dirs =
  let prefix = pkg ^ "." in
  let versions =
    List.concat_map
      (fun d ->
        let subdir = d / pkg in
        if not (Sys.file_exists subdir) then []
        else
          Sys.readdir subdir |> Array.to_list
          |> List.filter_map (fun entry ->
              if String.starts_with ~prefix entry then
                Some
                  (String.sub entry (String.length prefix)
                     (String.length entry - String.length prefix))
              else None))
      dirs
    |> List.sort_uniq String.compare
  in
  match versions with
  | [] -> None
  | _ ->
      Some
        (List.fold_left
           (fun a v ->
             if
               OpamPackage.Version.compare
                 (OpamPackage.Version.of_string v)
                 (OpamPackage.Version.of_string a)
               > 0
             then v
             else a)
           (List.hd versions) (List.tl versions))

(* Kinds of "$(b,@handle)-prefixed target" a registry build accepts:
   a plain target, an overlay-scoped package, or "everything the
   overlay ships". [oi run] only accepts the first two; [oi registry
   build] additionally understands [@handle] alone as "all of it". *)
type build_target =
  | Plain_target of string
  | Overlay_pkg of string * string (* (handle, pkg_spec) *)
  | Overlay_all of string (* handle alone, expand to every overlay pkg *)

let is_handle_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
  | _ -> false

let parse_build_target s =
  if String.length s < 2 || s.[0] <> '@' then Plain_target s
  else
    let rest = String.sub s 1 (String.length s - 1) in
    match String.index_opt rest '/' with
    | None ->
        if String.for_all is_handle_char rest && rest <> "" then
          Overlay_all rest
        else Plain_target s
    | Some i ->
        let handle = String.sub rest 0 i in
        let pkg = String.sub rest (i + 1) (String.length rest - i - 1) in
        if String.for_all is_handle_char handle && handle <> "" && pkg <> ""
        then Overlay_pkg (handle, pkg)
        else Plain_target s

(* Detect an [@handle/pkg[constr]] prefix on a run [TARGET] or a
   [--with] token. Returns [(handle, stripped_spec)] or [None] if
   there's no [@] prefix. Raises [Error.config_error] when the
   handle is present but the package part is empty. *)
let split_handle_prefix s =
  if String.length s < 2 || s.[0] <> '@' then None
  else
    let rest = String.sub s 1 (String.length s - 1) in
    match String.index_opt rest '/' with
    | None ->
        if String.for_all is_handle_char rest && rest <> "" then
          Oi.Error.config_error
            "overlay handle %S given without a package (use '@%s/PKG')" rest
            rest
        else None
    | Some i ->
        let handle = String.sub rest 0 i in
        let pkg_spec = String.sub rest (i + 1) (String.length rest - i - 1) in
        if (not (String.for_all is_handle_char handle)) || handle = "" then None
        else if pkg_spec = "" then
          Oi.Error.config_error
            "overlay handle %S given without a package (use '@%s/PKG')" handle
            handle
        else begin
          log_overlay "detected handle shortcut: %s -> target=%s" handle
            pkg_spec;
          Some (handle, pkg_spec)
        end

(* Merge CLI [--with-repo] URLs and project-declared extras. Project
   extras win on a name collision (CLI URLs are synthesised names and
   cannot collide with a user-chosen name unless the user picked an
   [extra-xxxxxxxxxx] prefix, which would be surprising); we instead
   dedup by name, preferring the project entry. *)
let merge_extras ~cli ~project =
  let seen = Hashtbl.create 8 in
  let acc = ref [] in
  let push (e : Oi.Project.extra_repo) =
    if not (Hashtbl.mem seen e.name) then begin
      Hashtbl.add seen e.name ();
      acc := e :: !acc
    end
  in
  List.iter push project;
  List.iter push cli;
  List.rev !acc

let default_registry = "https://oi.ci.dev"

let registry_term =
  let doc =
    Fmt.str
      "Remote layer registry URL (default: %s). Layers are fetched as \
       <URL>/<os_key>/<hash>.tar.zst before building from source."
      default_registry
  in
  Arg.(
    value & opt string default_registry & info ~docv:"URL" ~doc [ "registry" ])

let remote_of_registry = function
  | "" -> None
  | url -> Some (`Http_remote url : D10.Layer.remote)

(* Build the [cache_urls] list that opam's [pull_tree]/[pull_file] probe
   before falling back to the upstream source URL. Always includes the
   local {!Source_mirror}; when a remote registry is configured it
   appends the registry's [sources/] subtree. opam tries entries in
   order and silently skips those that don't have the blob, so either
   or both is a no-op if they lack the source. *)
let cache_urls_of ~cache ~remote =
  let local = Oi.Source_mirror.url ~cache in
  match remote with
  | Some (`Http_remote r) -> [ local; Oi.Source_mirror.remote_url ~registry:r ]
  | None | Some _ -> [ local ]

(* After a successful [Execute.run], promote every source blob that now
   lives in opam's download-cache into the local {!Source_mirror} and
   record its metadata rows. Idempotent — [record] no-ops on blobs
   already present, and logs-and-continues on I/O errors so a mirror
   write failure never fails an otherwise-successful build. *)
let record_sources_to_mirror ~sys ~cache (exec_plan : Oi.Plan.t) =
  try
    List.iter
      (fun (group : Oi.Plan.group) ->
        List.iter
          (fun (p : Oi.Plan.package_plan) ->
            let package = OpamPackage.of_string p.pkg in
            let overlay =
              match (p.overlay_handle, p.overlay_version) with
              | Some h, Some v -> Some (h, v)
              | _ -> None
            in
            Stdlib.Option.iter
              (fun (src : Oi.Plan.source_info) ->
                let checksums = List.map OpamHash.of_string src.checksums in
                let url = OpamUrl.parse ~handle_suffix:true src.url in
                Oi.Source_mirror.record ~sys ~cache ~package ?overlay
                  ~kind:`Main ~url ~checksums ())
              p.source;
            List.iter
              (fun (name, (src : Oi.Plan.source_info)) ->
                let checksums = List.map OpamHash.of_string src.checksums in
                let url = OpamUrl.parse ~handle_suffix:true src.url in
                Oi.Source_mirror.record ~sys ~cache ~package ?overlay
                  ~kind:(`Extra name) ~url ~checksums ())
              p.extra_sources)
          group.packages)
      exec_plan.groups
  with Failure msg ->
    Log.warn (fun m -> m "source mirror write failed: %s" msg)

let remote_index_max_age = 3600.0 (* 1 hour *)

(* Join a registry base URL and a relative path with a single [/], regardless
   of whether the user supplied a trailing slash. [rel] is expected not to
   start with one. *)
let url_join registry rel =
  let n = String.length registry in
  let stripped =
    if n > 0 && registry.[n - 1] = '/' then String.sub registry 0 (n - 1)
    else registry
  in
  stripped ^ "/" ^ rel

(* Ensure the remote registry's index.db is cached locally. Downloads it if
   missing or older than [remote_index_max_age]. Returns the local path on
   success.

   The download is atomic: we curl to a [.tmp] sibling and only rename
   into place once the download finished cleanly. A Ctrl-C mid-transfer
   leaves only the half-written [.tmp] behind, which the next invocation
   overwrites. That protects us from the sqlite [CORRUPT] error you
   otherwise get when the previous run wrote a half-database at the
   live path. *)
let ensure_remote_index ~sys ~fs ~cache ~os_key ~registry =
  if registry = "" then None
  else
    let cache_root = Oi.Cache.root_s cache in
    let os_dir = cache_root / "layers" / os_key in
    let local_path = os_dir / "remote-index.db" in
    let tmp_path = local_path ^ ".tmp" in
    let fresh =
      try
        let st = Unix.stat local_path in
        Unix.gettimeofday () -. st.Unix.st_mtime < remote_index_max_age
      with Unix.Unix_error _ -> false
    in
    if fresh then Some local_path
    else begin
      let url = url_join registry (os_key / "index.db") in
      let dst = Eio.Path.(fs / tmp_path) in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / os_dir);
      (try Unix.unlink tmp_path with Unix.Unix_error _ -> ());
      Logs.app (fun m ->
          m "Fetching registry index from %s (this may take a moment)..." url);
      if D10.Sysops.Curl.fetch sys ~url ~dst then begin
        (try Unix.rename tmp_path local_path
         with Unix.Unix_error _ -> (
           try Unix.unlink tmp_path with Unix.Unix_error _ -> ()));
        Some local_path
      end
      else begin
        (try Unix.unlink tmp_path with Unix.Unix_error _ -> ());
        if Sys.file_exists local_path then begin
          Logs.warn (fun m ->
              m "Failed to fetch registry index, using stale cache");
          Some local_path
        end
        else begin
          Logs.warn (fun m ->
              m "Failed to fetch registry index from %s" registry);
          None
        end
      end
    end

(* Merge the remote index into the local index, creating the local index
   if it doesn't exist. If the remote sqlite file is corrupt — typically
   the aftermath of a Ctrl-C during the previous run's download — we
   unlink it so the next invocation re-fetches a clean copy instead of
   failing forever. *)
let merge_remote_into_local ~index_path ~remote_path =
  let db = D10.Index.open_ ~path:index_path in
  (try D10.Index.merge_remote db ~remote_path
   with Failure msg -> (
     Logs.warn (fun m ->
         m
           "Remote index merge failed (%s); removing %s so the next run \
            re-downloads it"
           msg remote_path);
     try Sys.remove remote_path with Sys_error _ -> ()));
  D10.Index.close db

(* Count [hash/] directories directly under [layers/<os_key>/]. Each
   corresponds to one stored layer. Returns 0 if the directory does
   not yet exist. *)
let count_on_disk_layers ~os_layer_dir =
  match Sys.readdir os_layer_dir with
  | exception Sys_error _ -> 0
  | entries ->
      Array.fold_left
        (fun n name ->
          if Sys.is_directory (os_layer_dir / name) then n + 1 else n)
        0 entries

(* Ensure the local index exists and is not stale. A stale index is
   the common cause of [oi which] missing a just-built layer: the
   index is built once when [oi which] / [oi run] first needs it, but
   subsequent builds store layers on disk without touching it. Cheap
   staleness check: compare [layers/<os_key>/] directory count with
   the row count in the index. A mismatch triggers a full rebuild.
   Call before any index query in oi run / oi which. *)
let ensure_local_index ~sys ~fs ~clock ~cache ~os_key =
  let layers_dir = Oi.Cache.root_s cache / "layers" / os_key in
  let index_path = layers_dir / "index.db" in
  let d10 : D10.Config.t =
    { sys; fs; clock; root = Oi.Cache.root cache; os_key }
  in
  let rebuild reason =
    Logs.info (fun m -> m "%s local index for %s" reason os_key);
    let db = D10.Index.open_ ~path:index_path in
    D10.Index.rebuild d10 db;
    D10.Index.close db
  in
  if not (Sys.file_exists index_path) then rebuild "Building"
  else begin
    let disk = count_on_disk_layers ~os_layer_dir:layers_dir in
    let db = D10.Index.open_ ~path:index_path in
    let indexed, _, _ = D10.Index.stats db ~os_key in
    D10.Index.close db;
    (* [disk] may dip below [indexed] when layers have been merged from
       a remote registry index but not yet downloaded, so only rebuild
       when the disk count exceeds what the index knows about. *)
    if disk > indexed then
      rebuild (Fmt.str "Refreshing (%d on-disk vs %d indexed)" disk indexed)
  end;
  index_path

(* -- Helpers ------------------------------------------------------------- *)

let init_opam_root ~fs ~data_dir =
  let opam_root = data_dir / "opam-root" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / opam_root);
  Oi.Opam_ctx.init_opam ~root:opam_root

(* Eio.Process.spawn uses execvp-style lookup, which resolves bare
   executable names against the *caller's* PATH — not the PATH inside
   [~env]. Resolve the first token against the target env's PATH here
   so the child actually finds binaries from the assembled prefix. *)
let path_from_env env =
  Array.find_map
    (fun s ->
      if String.starts_with ~prefix:"PATH=" s then
        Some (String.sub s 5 (String.length s - 5))
      else None)
    env

let is_executable p =
  try
    Unix.access p [ Unix.X_OK ];
    true
  with Unix.Unix_error _ -> false

let resolve_in_env ~env exe =
  if String.contains exe '/' then exe
  else
    match path_from_env env with
    | None -> exe
    | Some path ->
        String.split_on_char ':' path
        |> List.find_map (function
          | "" -> None
          | d ->
              let candidate = d / exe in
              if is_executable candidate then Some candidate else None)
        |> Stdlib.Option.value ~default:exe

(* Run a command and return its exit code (never raises on non-zero exit) *)
let run_exec proc_mgr ~env cmd =
  let cmd =
    match cmd with exe :: rest -> resolve_in_env ~env exe :: rest | [] -> cmd
  in
  Eio.Switch.run @@ fun sw ->
  let child = Eio.Process.spawn ~sw proc_mgr ~env cmd in
  match Eio.Process.await child with `Exited n -> n | `Signaled n -> 128 + n

(* Wrap command body to catch structured errors *)
let pp_one_exn fmt = function
  | Oi.Error.E e -> Oi.Error.pp fmt e
  | Failure msg -> Fmt.pf fmt "%a %s" Fmt.(styled `Red string) "error:" msg
  | e ->
      Fmt.pf fmt "%a %s"
        Fmt.(styled `Red string)
        "error:" (Printexc.to_string e)

(* Test whether an exception (possibly wrapped) is rooted in our
   signal-handler cancel path or opam's [Sys.Break]. Either should
   render as a clean "Interrupted." exit, not a scary traceback. *)
let rec is_interrupt = function
  | Oi.Signals.Interrupted | Sys.Break -> true
  | Eio.Cancel.Cancelled e -> is_interrupt e
  | Eio.Exn.Io _ -> false
  | _ -> false

let with_error_handling f =
  try f () with
  | exn when is_interrupt exn ->
      Fmt.epr "Interrupted.@.";
      exit 130
  | Eio.Exn.Multiple exns when List.exists (fun (e, _) -> is_interrupt e) exns
    ->
      Fmt.epr "Interrupted.@.";
      exit 130
  | (Oi.Error.E _ | Failure _) as exn ->
      Fmt.epr "%a@." pp_one_exn exn;
      exit 1
  | Eio.Exn.Multiple exns ->
      List.iter (fun (e, _bt) -> Fmt.epr "%a@." pp_one_exn e) exns;
      exit 1

(* Boilerplate wrapper: every top-level command body should be run
   inside a root [Eio.Switch] so that [Signals.install] has something
   concrete to cancel, and so that resources registered with the
   switch (subprocesses, daemons) unwind cleanly on Ctrl-C.

   Use as:
     [with_eio_root @@ fun env sw -> ...body using sw...]

   The returned closure is still expected to be called inside
   [with_error_handling]. *)
(* Forced to the POSIX backend rather than [Eio_main.run] so that
   builds under Linux don't pick up [eio_linux] / io_uring — we want
   the same syscall surface everywhere, and io_uring interacts poorly
   with some of the subprocess / signal paths we rely on. If you need
   a different backend, swap this for [Eio_main.run] and thread
   [eio_main] back into [bin/dune]. *)
let with_eio_root f =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Oi.Signals.install ~sw;
  f env sw

let get_packages_dirs ?(refresh = false) ~fs ~sys ~data_dir () =
  Oi.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ()

let make_d10 ~sys ~fs ~clock ~cache ~os_key : D10.Config.t =
  { sys; fs; clock; root = Oi.Cache.root cache; os_key }

(* Standard per-command bootstrap. Returns the fields most commands derive
   from the Eio environment, plus the configured cache. *)
let bootstrap env cache_dir =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let stdout = Eio.Stdenv.stdout env in
  let stderr = Eio.Stdenv.stderr env in
  let sys = D10.Sysops.create ~stdout ~stderr ~proc_mgr ~fs () in
  let platform = Osrel.detect ~proc_mgr ~fs in
  let os_key = D10.Os_key.(to_string (of_platform platform)) in
  let cache = Oi.Cache.create ~root:cache_dir fs in
  (proc_mgr, fs, clock, sys, platform, os_key, cache)

(* Does the Eio path exist? Follows symlinks. *)
let path_exists fs path =
  try
    ignore (Eio.Path.stat ~follow:true Eio.Path.(fs / path));
    true
  with Eio.Exn.Io _ -> false

(* Resolve the current working directory once, as a canonical absolute path.
   Returns the string form (for env vars and opam APIs that take strings)
   and an [Eio.Path.t] rooted at [fs] (for filesystem operations).
   Using [Unix.realpath] up front avoids the relative "." that
   [Eio.Stdenv.cwd] would yield via [Eio.Path.native_exn], which leaks
   into OCAMLFIND_CONF / OCAMLLIB and breaks dune. *)
let resolved_cwd fs =
  let s = Unix.realpath "." in
  (s, Eio.Path.(fs / s))

(* Detect a project-local [_oi/tools/] that {!install_tools} has written
   under [cwd]. Returns [Some path] only when [tools/bin/] is populated,
   so callers that prepend it to PATH don't add a dangling directory. *)
let tools_dir_for ~cwd =
  let tools = cwd / "_oi" / "tools" in
  match Sys.is_directory (tools / "bin") with
  | true -> Some tools
  | false | (exception Sys_error _) -> None

(* Parse a CLI target as either "name", "name.version", or an opam atom
   like "name>=1.0" / "name=1.0". Returns the bare name and an optional
   version constraint for the solver. *)
let parse_pkg_target s =
  match OpamPackage.of_string_opt s with
  | Some pkg -> (OpamPackage.name pkg, Some (`Eq, OpamPackage.version pkg))
  | None -> OpamFormula.atom_of_string s

(* -- Remote registry helpers ---------------------------------------------- *)

(* Cap on concurrent registry layer downloads. Each download spawns a
   curl subprocess (2 pipe fds + child) plus a tar extractor, so a
   50-package stage without a cap blows past macOS's default 256-fd
   rlim. Shares the same resolution as {!Oi.Execute.run}'s [?jobs]:
   explicit arg wins, then [OI_BUILD_PARALLELISM], then default. *)

(** Try fetching uncached [Source] layers from [remote]. Returns a new action
    plan with downloaded layers promoted to [Binary]. No-op when [remote] is
    [None] or every layer is already cached. *)
let fetch_parallelism ?jobs () =
  match jobs with
  | Some n when n > 0 -> n
  | _ -> (
      match Sys.getenv_opt "OI_BUILD_PARALLELISM" with
      | Some s -> (
          match int_of_string_opt s with Some n when n > 0 -> n | _ -> 4)
      | None -> min (Domain.recommended_domain_count ()) 4)

let fetch_remote_layers ?jobs ~remote ~d10 ~packages_dirs ~ctx ~pkgs build_plan
    =
  match remote with
  | None -> build_plan
  | Some r ->
      let source_hashes =
        List.filter_map
          (fun (node : Oi.Action.node) ->
            match node.method_ with
            | Oi.Action.Source -> Some node.layer_hash
            | Binary -> None)
          (Oi.Action.nodes build_plan)
      in
      if source_hashes = [] then build_plan
      else begin
        let index = D10.Layer.fetch_remote_index d10 ~remote:r in
        let available =
          List.filter (fun h -> Hashtbl.mem index h) source_hashes
        in
        if available = [] then begin
          Logs.info (fun m ->
              m "Registry has none of the %d needed layer(s)"
                (List.length source_hashes));
          build_plan
        end
        else begin
          Logs.info (fun m ->
              m "Fetching %d layer(s) from registry (%d needed)..."
                (List.length available)
                (List.length source_hashes));
          Eio.Fiber.List.iter
            ~max_fibers:(fetch_parallelism ?jobs ())
            (fun hash ->
              let sha256 =
                Option.map
                  (fun (e : D10.Layer.index_entry) -> e.sha256)
                  (Hashtbl.find_opt index hash)
              in
              if D10.Layer.pull_remote d10 ~remote:r ~hash ?sha256 () then
                Logs.info (fun m -> m "Fetched %s from registry" hash))
            available;
          Oi.Action.plan ctx ~d10 ~packages_dirs pkgs
        end
      end

(* -- Platform config ------------------------------------------------------ *)

let ocaml_version = "5.4.1"

let make_conf ~platform:(p : Osrel.t) : Oi.Opam_ctx.conf =
  {
    arch = Osrel.Arch.to_string p.arch;
    os = Osrel.OS.to_string p.os;
    os_distribution = Osrel.OS.kind_to_string p.os.kind;
    os_version = p.os.version;
    os_family = p.os.family;
    ocaml_version;
    jobs = p.jobs;
  }

(** Solve for [names], ensure all layers exist (building from source via the
    build prefix if needed), return the layer hashes in topo order. When
    [dry_run] is true, print the build plan and exit. *)
let solve_and_ensure_layers ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf
    ~os_key ?(dry_run = false) ?(extra_repos = []) ?(pins = [])
    ?(refresh = false) ?project_dir:_ ?remote ?jobs
    ?(constraints = OpamPackage.Name.Map.empty) names =
  let extra_pkg_dirs =
    Oi.Repo.ensure_extra ~fs ~data_dir ~refresh extra_repos
  in
  let pin_dir = Oi.Pin.materialize ~fs ~sys ~cache ~refresh pins in
  let packages_dirs =
    Stdlib.Option.to_list pin_dir
    @ extra_pkg_dirs
    @ get_packages_dirs ~fs ~sys ~data_dir ()
  in
  log_overlay "solver packages_dirs (first-wins, %d entries):%s"
    (List.length packages_dirs)
    (String.concat "" (List.map (fun d -> Fmt.str "\n  %s" d) packages_dirs));
  let cache_root = Oi.Cache.root_s cache in
  let build_prefix = cache_root / "build" / "prefix" in
  let ctx = Oi.Opam_ctx.create ~prefix:build_prefix ~packages_dirs ~conf in
  let pkgs =
    match Oi.Solve.solve ctx ~packages_dirs ~constraints names with
    | Ok pkgs -> pkgs
    | Error msg -> Oi.Error.no_solution msg
  in
  let d10 = make_d10 ~sys ~fs ~clock ~cache ~os_key in
  let build_plan = Oi.Action.plan ctx ~d10 ~packages_dirs pkgs in
  if dry_run then begin
    let remote_has =
      match remote with
      | Some r ->
          let idx = D10.Layer.fetch_remote_index d10 ~remote:r in
          fun h -> Hashtbl.mem idx h
      | None -> fun _ -> false
    in
    Fmt.pr "%a@." (Oi.Action.pp_tree ~remote_has) build_plan;
    exit 0
  end;
  let build_plan =
    fetch_remote_layers ?jobs ~remote ~d10 ~packages_dirs ~ctx ~pkgs build_plan
  in
  let hashes = Oi.Action.layer_hashes build_plan in
  (* Check if the requested packages' layers are cached *)
  let targets_cached =
    List.for_all
      (fun name ->
        match Oi.Action.layer_hash_for build_plan name with
        | Some h -> D10.Layer.succeeded d10 ~hash:h
        | None -> true)
      names
  in
  if targets_cached then begin
    Logs.info (fun m -> m "Layers cached, skipping build");
    hashes
  end
  else begin
    let exec_plan =
      Oi.Plan.create ctx ~packages_dirs ~cache_root ~os_key
        ~ocaml_version:conf.ocaml_version build_plan
    in
    let cache_urls = cache_urls_of ~cache ~remote in
    Oi.Execute.run ~cache_urls ~proc_mgr ~fs ?jobs
      ~clock:(clock :> D10.Config.clk)
      ~sys ~os_key exec_plan;
    (* Contribute any newly-fetched sources to the mirror so the next
       build — here or on another machine sharing the registry — can
       hit the mirror instead of upstream. *)
    record_sources_to_mirror ~sys ~cache exec_plan;
    (* Invalidate any stale prefix cache *)
    let prefix_hash = D10.Prefix.solve_hash hashes in
    let prefix_dir =
      Eio.Path.native_exn Eio.Path.(d10.root / "prefixes" / prefix_hash)
    in
    (try Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / prefix_dir)
     with _ -> ());
    hashes
  end

(** Assemble a prefix from all layer hashes, return the prefix path. *)
let assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes =
  let d10 = make_d10 ~sys ~fs ~clock ~cache ~os_key in
  D10.Prefix.assemble_cached d10 ~layer_hashes

(* -- run ----------------------------------------------------------------- *)

let run_script ~sys ~fs ~proc_mgr ~clock ~os_key ~prefix ~conf ~cache ~data_dir
    ?remote script_path cli_deps args =
  let file_deps = Oi.Script.parse_deps_from_file ~fs script_path in
  let all_deps = Oi.Script.dedup (file_deps @ cli_deps) in
  if all_deps = [] then
    Oi.Error.msg
      "No dependencies found. Add [@@@opam pkg1 pkg2] to the first line or use \
       --with=pkg";
  let script_hash = Oi.Script.script_hash script_path all_deps in
  let run_dir = Oi.Cache.run_dir cache ~hash:script_hash in
  let run_dir_s = Eio.Path.native_exn run_dir in
  let cached_bin = run_dir_s / "main.exe" in
  if path_exists fs cached_bin then
    exit
      (run_exec proc_mgr
         ~env:
           (Oi.Prefix.make_env ~prefix
              ~dune_cache_root:(Oi.Cache.dune_root cache) ())
         (cached_bin :: args))
  else begin
    let packages_dirs = get_packages_dirs ~fs ~sys ~data_dir () in
    let ocaml_name = OpamPackage.Name.of_string "ocaml" in
    let dep_names =
      List.filter_map
        (fun (d : Oi.Script.dep) ->
          if OpamPackage.Name.equal d.name ocaml_name then None else Some d.name)
        all_deps
    in
    let constraints = Oi.Script.constraints all_deps in
    if dep_names <> [] then begin
      let cache_root = Oi.Cache.root_s cache in
      let build_prefix = cache_root / "build" / "prefix" in
      let ctx = Oi.Opam_ctx.create ~prefix:build_prefix ~packages_dirs ~conf in
      let pkgs =
        match Oi.Solve.solve ctx ~packages_dirs ~constraints dep_names with
        | Ok pkgs -> pkgs
        | Error msg ->
            Fmt.epr "No solution: %s@." msg;
            exit 1
      in
      let d10 = make_d10 ~sys ~fs ~clock ~cache ~os_key in
      let plan = Oi.Action.plan ctx ~d10 ~packages_dirs pkgs in
      let exec_plan =
        Oi.Plan.create ctx ~packages_dirs ~cache_root ~os_key
          ~ocaml_version:conf.ocaml_version plan
      in
      let cache_urls = cache_urls_of ~cache ~remote in
      Oi.Execute.run ~cache_urls ~proc_mgr ~fs
        ~clock:(clock :> D10.Config.clk)
        ~sys ~os_key exec_plan;
      record_sources_to_mirror ~sys ~cache exec_plan
    end;
    let build_dir = run_dir_s in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / build_dir);
    Oi.Script.generate_project ~script:script_path ~deps:all_deps ~dir:build_dir;
    let build_env =
      Oi.Prefix.make_env ~prefix ~dune_cache_root:(Oi.Cache.dune_root cache) ()
    in
    Eio.Process.run proc_mgr ~env:build_env
      [ "/bin/sh"; "-c"; Fmt.str "cd %s && dune build main.exe 2>&1" build_dir ];
    let built = build_dir / "_build" / "default" / "main.exe" in
    if path_exists fs built then begin
      let content = Eio.Path.load Eio.Path.(fs / built) in
      Eio.Path.save ~create:(`Or_truncate 0o755)
        Eio.Path.(fs / cached_bin)
        content
    end;
    let exe = if path_exists fs cached_bin then cached_bin else built in
    exit (run_exec proc_mgr ~env:build_env (exe :: args))
  end

let run_cmd =
  let run () data_dir cache_dir refresh dry_run registry target with_deps
      with_repos jobs args =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    init_opam_root ~fs ~data_dir;
    ignore (get_packages_dirs ~fs ~sys ~data_dir ~refresh ());
    let conf = make_conf ~platform in
    let remote = remote_of_registry registry in
    let dune_cache_root = Oi.Cache.dune_root cache in
    (* [TARGET] accepts a "@handle/pkg[constraint]" shortcut that
       pulls the corresponding overlay (and its transitive overlays)
       into the solver set. The prefix is stripped; the remainder is
       treated as a package (not a binary), so [@samoht/irmin] solves
       and installs the [irmin] package from samoht's overlay rather
       than going through the binary-name index. The bare package
       name becomes the [TARGET] used for the final [bin/<name>]
       lookup; any version constraint is passed to the solver via
       [--with]. *)
    (* Pull [@handle/pkg] out of a spec string and collect it as a
       pin. Shared between the TARGET and the [--with] fold below so
       both inputs accept the same shortcut syntax. *)
    let extract_handle_pin spec =
      match split_handle_prefix spec with
      | None -> (None, spec)
      | Some (h, pkg_spec) ->
          let pkg, user_constr = OpamFormula.atom_of_string pkg_spec in
          (Some { handle = h; pkg; user_constr }, pkg_spec)
    in
    (* TARGET shortcut: the stripped package name becomes the bare
       TARGET used for the final [bin/<name>] lookup, the full spec
       is passed through [--with], and the handle is appended to
       [with_repos]. *)
    let target, with_repos, with_deps, target_pin =
      match extract_handle_pin target with
      | None, _ -> (target, with_repos, with_deps, None)
      | Some pin, pkg_spec ->
          ( OpamPackage.Name.to_string pin.pkg,
            with_repos @ [ pin.handle ],
            with_deps @ [ pkg_spec ],
            Some pin )
    in
    (* [--with] tokens accept the same shortcut syntax. The opam atom
       parser rejects "@avsm/owntracks-cli" directly because [@] and
       [/] aren't valid in package names, so we split before handing
       off. *)
    let with_repos, with_deps, with_pins =
      List.fold_left
        (fun (repos, deps, pins) w ->
          match extract_handle_pin w with
          | None, _ -> (repos, deps @ [ w ], pins)
          | Some pin, pkg_spec ->
              (repos @ [ pin.handle ], deps @ [ pkg_spec ], pins @ [ pin ]))
        (with_repos, [], []) with_deps
    in
    (* URL-projects in [--with=…]: clone each URL into the pin cache,
       scan its *.opam files, and merge the contribution as pins +
       solver roots + overlays + extra_repos. *)
    let extra_deps, url_project =
      materialize_with_deps ~fs ~sys ~cache ~refresh with_deps
    in
    let extra_constraints = Oi.Script.constraints extra_deps in
    (* Resolve the cwd once; reused for project-extras loading and the
       script-file existence check below. *)
    let cwd_s, cwd = resolved_cwd fs in
    (* Load project extras (if any *.opam in cwd). A missing/unreadable
       directory degrades to "no extras"; malformed metadata still raises
       [Error.E] so the user sees the problem. *)
    let project_extras, project_pins, project_overlays =
      match Oi.Project.load ~fs cwd_s with
      | exception Sys_error _ -> ([], [], [])
      | exception Eio.Exn.Io _ -> ([], [], [])
      | p -> (p.extra_repos, p.pins, p.overlays)
    in
    let project_extras = project_extras @ url_project.extra_repos in
    let project_pins = project_pins @ url_project.pins in
    let project_overlays = project_overlays @ url_project.overlays in
    (* Treat [x-reporepo:] handles as if they had been passed via
       [--with-repo]. Project-declared overlays go earlier in the
       list so CLI-supplied ones take priority (first-wins at repos
       level; later arguments stack atop). *)
    let with_repos = project_overlays @ with_repos in
    let cli_extras = cli_extra_repos ~fs ~sys with_repos in
    let all_extras = merge_extras ~cli:cli_extras ~project:project_extras in
    (* Pin each [@handle/pkg] (from TARGET or [--with]) to whatever
       version the overlay ships, so a dev-tagged version (e.g.
       [2.0.0~dev]) that would otherwise sort below a stable repo's
       version still wins when the user explicitly asked for it. The
       overlay is cloned upfront so we can scan its [packages/] tree;
       the subsequent solve reuses the same clone. *)
    let handle_pins = Stdlib.Option.to_list target_pin @ with_pins in
    let handle_constraints =
      if handle_pins = [] then OpamPackage.Name.Map.empty
      else
        let overlay_pkg_dirs =
          Oi.Repo.ensure_extra ~fs ~data_dir ~refresh cli_extras
        in
        List.fold_left
          (fun acc { handle; pkg; user_constr } ->
            match user_constr with
            | Some c -> OpamPackage.Name.Map.add pkg c acc
            | None -> (
                let pkg_s = OpamPackage.Name.to_string pkg in
                match latest_version_in_dirs ~pkg:pkg_s overlay_pkg_dirs with
                | None ->
                    Oi.Error.config_error
                      "overlay %s does not provide a package named %s" handle
                      pkg_s
                | Some v ->
                    log_overlay "pinning %s = %s from overlay %s" pkg_s v handle;
                    OpamPackage.Name.Map.add pkg
                      (`Eq, OpamPackage.Version.of_string v)
                      acc))
          OpamPackage.Name.Map.empty handle_pins
    in
    let extra_constraints =
      OpamPackage.Name.Map.union
        (fun a _ -> a)
        handle_constraints extra_constraints
    in
    let solve_assemble_run pkg_names =
      Logs.info (fun m ->
          m "Solving for packages: %s" (String.concat ", " pkg_names));
      let names = List.map OpamPackage.Name.of_string pkg_names in
      let layer_hashes =
        solve_and_ensure_layers ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf
          ~os_key ~dry_run ~extra_repos:all_extras ~pins:project_pins ~refresh
          ~project_dir:cwd_s ?remote ?jobs ~constraints:extra_constraints names
      in
      Logs.info (fun m -> m "Got %d layer hashes" (List.length layer_hashes));
      let prefix =
        assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes
      in
      Logs.info (fun m -> m "Assembled prefix at %s" prefix);

      let bin = prefix / "bin" / target in
      Logs.info (fun m -> m "Looking for binary: %s" bin);
      if path_exists fs bin then begin
        Logs.info (fun m -> m "Found binary, executing");
        exit
          (run_exec proc_mgr
             ~env:(Oi.Prefix.make_env ~prefix ~dune_cache_root ())
             (bin :: args))
      end
      else begin
        (* List what binaries are available in the prefix *)
        let bin_dir = prefix / "bin" in
        (try
           let bins = Eio.Path.read_dir Eio.Path.(fs / bin_dir) in
           Logs.info (fun m ->
               m "Available binaries in prefix: %s"
                 (String.concat ", " (List.sort String.compare bins)))
         with Eio.Exn.Io _ ->
           Logs.info (fun m -> m "No bin/ directory in prefix"));
        false
      end
    in
    (* HTTP(S) script URLs: fetch to a fresh tmp file keeping the [.ml]
       suffix, then treat the local copy as the script path for the
       rest of the run. Run caching keys on the script's content hash,
       so the nondeterministic path doesn't defeat the build cache. *)
    let target =
      let is_url s =
        String.starts_with ~prefix:"http://" s
        || String.starts_with ~prefix:"https://" s
      in
      if is_url target then begin
        let local = Filename.temp_file "oi-script-" ".ml" in
        Logs.info (fun m -> m "Fetching %s to %s" target local);
        if
          not (D10.Sysops.Curl.fetch sys ~url:target ~dst:Eio.Path.(fs / local))
        then Oi.Error.not_found target "failed to fetch %s" target;
        local
      end
      else target
    in
    (* Only .ml files are treated as scripts *)
    if Filename.check_suffix target ".ml" then begin
      if not (path_exists cwd target) then
        Oi.Error.not_found target "file not found: %s" target;
      (* For scripts, solve deps first to get a prefix with the compiler *)
      let all_script_deps =
        Oi.Script.parse_deps_from_file ~fs target @ extra_deps
      in
      let ocaml_name = OpamPackage.Name.of_string "ocaml" in
      let dep_opam_names =
        List.filter_map
          (fun (d : Oi.Script.dep) ->
            if OpamPackage.Name.equal d.name ocaml_name then None
            else Some d.name)
          all_script_deps
      in
      let constraints = Oi.Script.constraints all_script_deps in
      let layer_hashes =
        if dep_opam_names = [] then []
        else
          solve_and_ensure_layers ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir
            ~conf ~os_key ~dry_run ~extra_repos:all_extras ~pins:project_pins
            ~refresh ?remote ?jobs ~constraints dep_opam_names
      in
      if dry_run && dep_opam_names = [] then
        (* No deps to solve, but still in dry-run mode — just exit *)
        exit 0;
      let prefix =
        assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes
      in
      run_script ~sys ~fs ~proc_mgr ~clock ~os_key ~prefix ~conf ~cache
        ~data_dir ?remote target extra_deps args
    end
    else begin
      (* Include --with deps in every solve *)
      let ocaml_name = OpamPackage.Name.of_string "ocaml" in
      let extra_names =
        List.filter_map
          (fun (d : Oi.Script.dep) ->
            if OpamPackage.Name.equal d.name ocaml_name then None
            else Some (Oi.Script.name_s d))
          extra_deps
        @ url_project.roots
      in
      let solve_assemble_run_with pkg_names =
        solve_assemble_run (pkg_names @ extra_names)
      in
      (* Step 0: If --with deps are given, try solving for just those first.
         The target binary might come from a --with package. *)
      let from_with =
        if extra_names <> [] then solve_assemble_run extra_names else false
      in
      (* When a handle shortcut was used (e.g. [@samoht/irmin]), the user
         has explicitly named the source of truth for the target
         package. Never silently fall through to the layer-index
         lookup — that would quietly substitute a different package
         (irmin-cli, in the motivating case) for the one actually
         requested. If [solve_assemble_run] didn't produce a working
         [bin/target], surface a helpful error. *)
      if Stdlib.Option.is_some target_pin && not from_with then
        Oi.Error.not_found target
          "overlay-pinned package does not provide bin/%s. Check 'oi config' \
           or the overlay's opam file."
          target;
      if not from_with then begin
        (* Dash-split prefixes: "a-b-c" → ["a-b-c"; "a-b"; "a"] *)
        let dash_prefixes name =
          let parts = String.split_on_char '-' name in
          let rec aux acc prefix = function
            | [] -> List.rev acc
            | p :: rest ->
                let prefix = match prefix with "" -> p | s -> s ^ "-" ^ p in
                aux (prefix :: acc) prefix rest
          in
          List.rev (aux [] "" parts)
        in
        (* Step 1: Check layer index for which package provides this binary *)
        let index_path =
          ensure_local_index ~sys ~fs
            ~clock:(Eio.Stdenv.clock env :> D10.Config.clk)
            ~cache ~os_key
        in
        (* Merge remote registry index *)
        (match ensure_remote_index ~sys ~fs ~cache ~os_key ~registry with
        | Some remote_path -> merge_remote_into_local ~index_path ~remote_path
        | None -> Logs.info (fun m -> m "Remote index unavailable"));
        let index_exists = Sys.file_exists index_path in
        let from_index =
          if index_exists then begin
            let os_key =
              D10.Os_key.(to_string (of_platform (Osrel.detect ~proc_mgr ~fs)))
            in
            let db = D10.Index.open_ ~path:index_path in
            let results = D10.Index.find_binary db ~binary:target ~os_key in
            D10.Index.close db;
            match results with
            | (pkg_name, _pkg_ver, _hash) :: _ -> (
                Logs.info (fun m ->
                    m "Index: bin/%s provided by package %s" target pkg_name);
                try solve_assemble_run_with [ pkg_name ] with _ -> false)
            | [] -> false
          end
          else false
        in
        if not from_index then begin
          (* Step 2: Try target name and dash-split prefixes. Skip any
             prefix already in [extra_names] (Step 0 solved that already)
             and skip any prefix that doesn't exist as a package in any
             configured repo — a missing package name cannot possibly
             provide the binary, and attempting to solve for it wastes
             a full solver run. *)
          let pin_dir =
            Oi.Pin.materialize ~fs ~sys ~cache ~refresh project_pins
          in
          let packages_dirs =
            Stdlib.Option.to_list pin_dir
            @ Oi.Repo.ensure_extra ~fs ~data_dir ~refresh all_extras
            @ get_packages_dirs ~fs ~sys ~data_dir ~refresh ()
          in
          let package_exists name =
            List.exists (fun dir -> Sys.file_exists (dir / name)) packages_dirs
          in
          let prefixes =
            dash_prefixes target
            |> List.filter (fun p -> not (List.mem p extra_names))
            |> List.filter package_exists
          in
          if prefixes = [] then
            Oi.Error.not_found target "no package provides bin/%s" target
          else begin
            Logs.info (fun m ->
                m "Trying packages: %s" (String.concat ", " prefixes));
            let found =
              List.exists
                (fun name ->
                  try solve_assemble_run_with [ name ]
                  with Oi.Error.E _ -> false)
                prefixes
            in
            if not found then
              Oi.Error.not_found target "no package provides bin/%s" target
          end
        end
      end
    end
  in
  let target =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"TARGET" ~doc:"OCaml script (.ml) or binary name" [])
  in
  let dry_run =
    Arg.(
      value & flag
      & info ~doc:"Show what would be built without building" [ "n"; "dry-run" ])
  in
  let args =
    Arg.(
      value & pos_right 0 string []
      & info ~docv:"ARG" ~doc:"Arguments passed to the target" [])
  in
  let info =
    Cmd.info "run" ~doc:"Run an OCaml script or any opam-packaged binary"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Solve for $(b,TARGET)'s dependencies, install them into the local \
             cache, and run $(b,TARGET). Re-runs are instant when the dep set \
             is unchanged.";
          `P
            "$(b,TARGET) is a binary name, a $(b,.ml) script, or an http(s) \
             URL pointing at a $(b,.ml) script.";
          `S "BINARIES";
          `P
            "Pass a binary name. $(b,oi) finds the opam package that ships it. \
             Dash-split prefixes are also tried, so $(b,ocluster-admin) falls \
             back to $(b,ocluster). Packages listed in $(b,--with) are tried \
             first.";
          `Pre
            "  oi run utop\n\
            \  oi run ocamlformat -- --help\n\
            \  oi run --with=crockford roguedoi";
          `S "OVERLAYS";
          `P
            "Prefix a target or a $(b,--with) value with $(i,@handle/) to pin \
             the named package to whatever version that overlay ships. Stack \
             handles to compose overlays. Add $(b,x-reporepo: [\"handle\"]) in \
             an opam file to make it automatic.";
          `Pre
            "  oi run @avsm/owntracks\n\
            \  oi run @samoht/irmin\n\
            \  oi run --with=@avsm/crockford roguedoi";
          `S "GIT URLS";
          `P
            "$(b,--with=URL) clones the repository and pins every $(b,*.opam) \
             at its root as a solver root. Schemes: $(b,http(s)://), \
             $(b,git+), $(b,git@), $(b,git://), $(b,ssh://). Append $(b,#ref) \
             to pin a branch or commit.";
          `Pre
            "  oi run --with=https://github.com/owner/project.git target\n\
            \  oi run --with=git+https://example.org/foo.git#branch foo";
          `S "VERSIONS";
          `P
            "$(b,--with=pkg.version) and $(b,--with=pkg=version) pin a package \
             to a specific version. Relops $(b,>=), $(b,>), $(b,<=), $(b,<) \
             also work.";
          `Pre
            "  oi run --with=dune.3.20.0 -- dune --version\n\
            \  oi run --with=fmt>=0.9 my_script.ml";
          `S "SCRIPTS";
          `P "Scripts are $(b,.ml) files with dependencies on the first line:";
          `Pre "  [@@@opam fmt cmdliner lwt>=5.0]";
          `P
            "Each token is an opam package, optionally with a version \
             constraint. A dot names a findlib sub-library, e.g. \
             $(b,ppx_deriving.show). Packages starting with $(b,ppx_) are \
             wired in as PPX preprocessors.";
          `P
            "Builds are cached by script content plus dep list. Edit the \
             script to trigger a rebuild. URL targets are re-fetched on every \
             run but hit the cache when unchanged.";
          `Pre
            "  oi run my_script.ml\n\
            \  oi run my_script.ml --with=tls -- arg1 arg2\n\
            \  oi run https://gist.example.com/hello.ml";
          `S "DRY RUN";
          `P
            "$(b,-n) / $(b,--dry-run) prints the plan without executing. Each \
             package is tagged:";
          `I ("$(b,binary)", "Already cached locally.");
          `I ("$(b,remote)", "Available from the registry.");
          `I ("$(b,source)", "Would be built from source.");
          `I ("$(b,virtual)", "A stub like $(b,conf-pkg-config).");
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ dry_run $ registry_term $ target $ with_deps_term $ with_repos_term
      $ jobs_term $ args)

(* -- plan ---------------------------------------------------------------- *)

let plan_cmd =
  let run () data_dir cache_dir refresh targets with_repos with_deps =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    init_opam_root ~fs ~data_dir;
    ignore (get_packages_dirs ~fs ~sys ~data_dir ~refresh ());
    let conf = make_conf ~platform in
    let cwd_s, _ = resolved_cwd fs in
    let extra_deps, url_project =
      materialize_with_deps ~fs ~sys ~cache ~refresh with_deps
    in
    let project_extras, project_pins, project_overlays =
      match Oi.Project.load ~fs cwd_s with
      | exception Sys_error _ -> ([], [], [])
      | exception Eio.Exn.Io _ -> ([], [], [])
      | p -> (p.extra_repos, p.pins, p.overlays)
    in
    let project_extras = project_extras @ url_project.extra_repos in
    let project_pins = project_pins @ url_project.pins in
    let project_overlays = project_overlays @ url_project.overlays in
    let with_repos = project_overlays @ with_repos in
    let all_extras =
      merge_extras
        ~cli:(cli_extra_repos ~fs ~sys with_repos)
        ~project:project_extras
    in
    let extra_pkg_dirs =
      Oi.Repo.ensure_extra ~fs ~data_dir ~refresh all_extras
    in
    let pin_dir = Oi.Pin.materialize ~fs ~sys ~cache ~refresh project_pins in
    let packages_dirs =
      Stdlib.Option.to_list pin_dir
      @ extra_pkg_dirs
      @ get_packages_dirs ~fs ~sys ~data_dir ()
    in
    let extra_constraints = Oi.Script.constraints extra_deps in
    let extra_names =
      List.filter_map
        (fun (d : Oi.Script.dep) ->
          if OpamPackage.Name.to_string d.name = "ocaml" then None
          else Some d.name)
        extra_deps
    in
    let url_names = List.map OpamPackage.Name.of_string url_project.roots in
    let names =
      List.map OpamPackage.Name.of_string targets @ extra_names @ url_names
    in
    let build_prefix = Oi.Cache.root_s cache / "build" / "prefix" in
    let ctx = Oi.Opam_ctx.create ~prefix:build_prefix ~packages_dirs ~conf in
    let pkgs =
      match
        Oi.Solve.solve ctx ~packages_dirs ~constraints:extra_constraints names
      with
      | Ok pkgs -> pkgs
      | Error msg -> Oi.Error.no_solution msg
    in
    let d10 =
      make_d10 ~sys ~fs ~clock:(clock :> D10.Config.clk) ~cache ~os_key
    in
    let action_plan = Oi.Action.plan ctx ~d10 ~packages_dirs pkgs in
    let plan =
      Oi.Plan.create ctx ~packages_dirs ~cache_root:(Oi.Cache.root_s cache)
        ~os_key ~ocaml_version:conf.ocaml_version action_plan
    in
    Fmt.pr "%a@." Oi.Plan.pp plan
  in
  let targets =
    Arg.(
      non_empty & pos_all string []
      & info ~docv:"PKG" ~doc:"Package(s) to plan" [])
  in
  let info =
    Cmd.info "plan" ~doc:"Show the build plan for these packages"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Resolve the dependency tree for $(b,PKG) and print every package \
             in it. Does not download or build anything.";
          `P "Each node is tagged:";
          `I ("$(b,source)", "Would be built from source.");
          `I ("$(b,binary)", "Already cached locally.");
          `I ("$(b,remote)", "Available from the registry.");
          `I ("$(b,virtual)", "A stub like $(b,conf-pkg-config).");
          `P
            "$(b,--with) and $(b,--with-repo) accept the same forms as $(b,oi \
             run).";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ targets $ with_repos_term $ with_deps_term)

(* -- env ----------------------------------------------------------------- *)

let env_cmd =
  let run () data_dir cache_dir refresh with_repos with_deps jobs =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    let dune_cache_root = Oi.Cache.dune_root cache in
    (* Detect _oi/ project directory. A pre-existing _oi/prefix is
       reused as-is UNLESS the user passes --with-repo or --with, which
       demands a fresh solve to honour the additions. *)
    let cwd_s, cwd = resolved_cwd fs in
    let oi_prefix = cwd_s / "_oi" / "prefix" in
    let want_extras = with_repos <> [] || with_deps <> [] in
    let prefix =
      if (not want_extras) && path_exists cwd "_oi/prefix" then oi_prefix
      else begin
        (* Fall back to a minimal compiler-only prefix, optionally
           extended with CLI extras + with-deps. *)
        init_opam_root ~fs ~data_dir;
        ignore (get_packages_dirs ~fs ~sys ~data_dir ~refresh ());
        let conf = make_conf ~platform in
        let extra_cli, url_project =
          materialize_with_deps ~fs ~sys ~cache ~refresh with_deps
        in
        let extras =
          merge_extras
            ~cli:(cli_extra_repos ~fs ~sys (with_repos @ url_project.overlays))
            ~project:url_project.extra_repos
        in
        let extra_constraints = Oi.Script.constraints extra_cli in
        let extra_names =
          List.filter_map
            (fun (d : Oi.Script.dep) ->
              if OpamPackage.Name.to_string d.name = "ocaml" then None
              else Some d.name)
            extra_cli
          @ List.map OpamPackage.Name.of_string url_project.roots
        in
        let layer_hashes =
          solve_and_ensure_layers ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir
            ~conf ~os_key ~refresh ~extra_repos:extras ~pins:url_project.pins
            ?jobs ~constraints:extra_constraints
            (OpamPackage.Name.of_string "ocaml" :: extra_names)
        in
        assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes
      end
    in
    let vars = Oi.Prefix.env_vars ~prefix ~dune_cache_root in
    let current_path =
      try Sys.getenv "PATH" with Not_found -> "/usr/bin:/bin"
    in
    let tools = tools_dir_for ~cwd:cwd_s in
    let path_prefix =
      match tools with
      | None -> prefix / "bin"
      | Some t -> (t / "bin") ^ ":" ^ (prefix / "bin")
    in
    List.iter
      (fun (k, v) ->
        let v = if k = "PATH" then path_prefix ^ ":" ^ current_path else v in
        Fmt.pr "export %s=\"%s\"@." k v)
      vars
  in
  let info =
    Cmd.info "env" ~doc:"Print shell exports for the project environment"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Print $(b,export) statements for $(b,PATH), $(b,OCAMLLIB), and \
             the other variables OCaml tools look at. Typical usage:";
          `Pre "  eval \"\\$(oi env)\"";
          `P
            "Same environment $(b,oi sync) writes into $(b,.envrc), without \
             the $(b,direnv) wrapper. Sync runs implicitly if $(b,_oi/prefix/) \
             is missing or stale.";
          `P
            "$(b,--with) and $(b,--with-repo) force a fresh solve. They accept \
             the same forms as $(b,oi run).";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ with_repos_term $ with_deps_term $ jobs_term)

(* -- init ---------------------------------------------------------------- *)

(* -- which --------------------------------------------------------------- *)

let which_cmd =
  let run () cache_dir registry long pattern =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, clock, sys, _platform, os_key, cache =
      bootstrap env cache_dir
    in
    let clk = (clock :> D10.Config.clk) in
    let index_path = ensure_local_index ~sys ~fs ~clock:clk ~cache ~os_key in
    (* Merge remote index *)
    (match ensure_remote_index ~sys ~fs ~cache ~os_key ~registry with
    | Some remote_path -> merge_remote_into_local ~index_path ~remote_path
    | None -> ());
    let db = D10.Index.open_ ~path:index_path in
    let results = D10.Index.search_binary db ~pattern ~os_key in
    if results = [] then begin
      Fmt.pr "No binaries matching %s@." pattern;
      D10.Index.close db
    end
    else begin
      (* Determine which hashes are available locally *)
      let d10 : D10.Config.t =
        { sys; fs; clock = clk; root = Oi.Cache.root cache; os_key }
      in
      let short h = String.sub h 0 (min 12 (String.length h)) in
      List.iter
        (fun (binary, pkg_name, pkg_ver, hash, overlay) ->
          let source =
            if D10.Layer.succeeded d10 ~hash then
              Fmt.str "%a" Fmt.(styled `Green string) "local"
            else Fmt.str "%a" Fmt.(styled `Cyan string) "remote"
          in
          let overlay_s =
            match overlay with
            | None -> "-"
            | Some (h, v) -> Fmt.str "%s.%s" h v
          in
          Fmt.pr "%-20s %-24s %-20s %-12s (%s)@." binary
            (Fmt.str "%s.%s" pkg_name pkg_ver)
            overlay_s (short hash) source;
          if long then begin
            let deps = D10.Index.deps db ~hash in
            if deps = [] then
              Fmt.pr "  %a@." Fmt.(styled `Faint string) "(no deps)"
            else
              List.iter
                (fun (dep_name, dep_ver, dep_hash) ->
                  Fmt.pr "  %a %s.%s@."
                    Fmt.(styled `Faint string)
                    (short dep_hash) dep_name dep_ver)
                deps
          end)
        results;
      D10.Index.close db
    end
  in
  let pattern =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"PATTERN"
          ~doc:
            "Binary name to search for. Use * as wildcard (e.g. ocaml* or \
             *format*)."
          [])
  in
  let long =
    Arg.(
      value & flag
      & info
          ~doc:
            "Show the direct dependencies (name, version, short hash) under \
             each matching layer. Useful when the same package appears more \
             than once: duplicates are distinct layer hashes with different \
             dep closures, and this flag reveals which dependencies differ."
          [ "l"; "long" ])
  in
  let info =
    Cmd.info "which" ~doc:"Find which opam package ships a given binary"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Search the local cache and remote registry for packages providing \
             $(b,PATTERN). Wildcards with $(b,*).";
          `P
            "Each row: binary name, package.version, overlay tag, short layer \
             hash, source (local or remote).";
          `P
            "A package can appear more than once: different dep closures \
             produce different layer hashes. $(b,-l) / $(b,--long) prints each \
             layer's direct deps so you can see what differs.";
          `Pre
            "  oi which dune\n\
            \  oi which -l jsont\n\
            \  oi which 'ocaml*'\n\
            \  oi which '*fmt*'";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ cache_dir_term $ registry_term $ long $ pattern)

(* -- tool installation --------------------------------------------------- *)

let short_hash h = String.sub h 0 (min 12 (String.length h))

let warn_tool spec fmt =
  Fmt.kstr
    (fun s ->
      Fmt.epr "%a tool %s: %s@."
        Fmt.(styled `Yellow string)
        "WARN" (spec : Oi.Tool.spec).name s)
    fmt

(* Return the layer hash whose [layer.json] declares package name
   [want_name] (any version). Tools get assembled from only their own
   leaf layer — the transitive deps (ocaml, dune, ocamlfind…) are
   already present in the shared d10 cache but are not needed at
   runtime by a native-compiled tool binary, so we leave them out of
   [_oi/tools/] to keep it small and focused. *)
let leaf_hash_for ~fs ~cache ~os_key ~want_name hashes =
  let layers_dir = Oi.Cache.root_s cache / "layers" / os_key in
  let leaf hash =
    match
      D10.Layer.load_meta Eio.Path.(fs / layers_dir / hash / "layer.json")
    with
    | Some m -> (
        match OpamPackage.of_string_opt m.package with
        | Some p
          when OpamPackage.Name.to_string (OpamPackage.name p) = want_name ->
            Some hash
        | _ -> None)
    | None -> None
  in
  List.find_map leaf hashes

(* Solve and install every probed dev tool into [cwd/_oi/tools/]. Each
   tool is its own independent solve so its dep closure never leaks
   into the main project's OCAMLLIB / OCAMLPATH. A tool that fails to
   solve (e.g. pinned to an older ocaml) warns and is skipped; other
   tools still install. Returns the assembled path if at least one
   tool made it in, or [None] if nothing to install. *)
let install_tools ?(quiet = false) ?refresh ?jobs ~proc_mgr ~fs ~clock ~sys
    ~cache ~data_dir ~conf ~os_key ~extra_repos ~pins ?remote ~cwd () =
  let say fmt =
    if quiet then Fmt.kstr (fun s -> Logs.info (fun m -> m "%s" s)) fmt
    else Fmt.kstr (fun s -> Fmt.pr "%s@." s) fmt
  in
  let install_one (r : Oi.Tool.result) =
    let spec = r.spec in
    let name = OpamPackage.Name.of_string spec.name in
    let constraints =
      match r.version with
      | None -> OpamPackage.Name.Map.empty
      | Some v ->
          OpamPackage.Name.Map.singleton name
            (`Eq, OpamPackage.Version.of_string v)
    in
    try
      let hashes =
        solve_and_ensure_layers ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf
          ~os_key ~extra_repos ~pins ?refresh ?remote ?jobs ~constraints
          [ name ]
      in
      match leaf_hash_for ~fs ~cache ~os_key ~want_name:spec.name hashes with
      | None ->
          warn_tool spec "layer for leaf package not found";
          None
      | Some h ->
          say "Tool %s: %d dep(s) built, leaf layer %s" spec.name
            (List.length hashes - 1)
            (short_hash h);
          Some h
    with
    | Oi.Error.E e ->
        warn_tool spec "%a" Oi.Error.pp e;
        None
    | exn ->
        warn_tool spec "%s" (Printexc.to_string exn);
        None
  in
  match Oi.Tool.(hits (probe ~fs cwd)) with
  | [] ->
      say "No dev tools to install";
      None
  | hits -> (
      let leaves = List.filter_map install_one hits in
      match leaves with
      | [] -> None
      | _ ->
          let tools_dir = cwd / "_oi" / "tools" in
          Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / tools_dir);
          Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / tools_dir);
          let d10 = make_d10 ~sys ~fs ~clock ~cache ~os_key in
          let unique = List.sort_uniq String.compare leaves in
          D10.Prefix.assemble d10 ~layer_hashes:unique
            ~dst:Eio.Path.(fs / tools_dir);
          say "Tools assembled at %s (%d tool(s), %d leaf layer(s))" tools_dir
            (List.length leaves) (List.length unique);
          Some tools_dir)

(* -- sync ---------------------------------------------------------------- *)

(* Run a full sync in [cwd]: solve the deps declared in *.opam files,
   build/fetch layers, assemble [cwd]/_oi/prefix, and (re)write .envrc.
   Returns the path to the assembled prefix. When [quiet] is true,
   narration goes to Logs.info (hidden at default verbosity); otherwise
   it prints to stdout. *)
let do_sync ?(quiet = false) ?(refresh = false) ?(with_repos = [])
    ?(with_deps = []) ?jobs ~proc_mgr ~fs ~clock ~sys ~platform ~os_key ~cache
    ~data_dir ~registry ~cwd () =
  let say fmt =
    if quiet then Fmt.kstr (fun s -> Logs.info (fun m -> m "%s" s)) fmt
    else Fmt.kstr (fun s -> Fmt.pr "%s@." s) fmt
  in
  init_opam_root ~fs ~data_dir;
  ignore (get_packages_dirs ~fs ~sys ~data_dir ~refresh ());
  let project = Oi.Project.load ~fs cwd in
  let extra_cli, url_project =
    materialize_with_deps ~fs ~sys ~cache ~refresh with_deps
  in
  let deps = project.deps in
  if deps = [] && extra_cli = [] && url_project.roots = [] then
    Oi.Error.config_error "No .opam files found in %s." cwd;
  say "Dependencies from opam files: %s" (String.concat ", " deps);
  if url_project.roots <> [] then
    say "URL-supplied packages: %s" (String.concat ", " url_project.roots);
  if project.overlays <> [] || url_project.overlays <> [] then
    say "Project overlays (from x-reporepo): %s"
      (String.concat ", " (project.overlays @ url_project.overlays));
  let with_repos = project.overlays @ url_project.overlays @ with_repos in
  let all_extras =
    merge_extras
      ~cli:(cli_extra_repos ~fs ~sys with_repos)
      ~project:(project.extra_repos @ url_project.extra_repos)
  in
  if all_extras <> [] then
    say "Extra repositories: %s"
      (String.concat ", "
         (List.map
            (fun (e : Oi.Project.extra_repo) -> Fmt.str "%s (%s)" e.name e.url)
            all_extras));
  let conf = make_conf ~platform in
  let remote = remote_of_registry registry in
  let extra_constraints = Oi.Script.constraints extra_cli in
  let extra_names =
    List.filter_map
      (fun (d : Oi.Script.dep) ->
        if OpamPackage.Name.to_string d.name = "ocaml" then None
        else Some d.name)
      extra_cli
  in
  let url_names = List.map OpamPackage.Name.of_string url_project.roots in
  let names =
    List.map OpamPackage.Name.of_string deps @ extra_names @ url_names
  in
  let layer_hashes =
    solve_and_ensure_layers ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf
      ~os_key ~extra_repos:all_extras
      ~pins:(project.pins @ url_project.pins)
      ~refresh ~project_dir:cwd ~constraints:extra_constraints ?remote ?jobs
      names
  in
  let oi_dir = cwd / "_oi" in
  let prefix = oi_dir / "prefix" in
  Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / prefix);
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / oi_dir);
  let d10 = make_d10 ~sys ~fs ~clock ~cache ~os_key in
  D10.Prefix.assemble d10 ~layer_hashes ~dst:Eio.Path.(fs / prefix);
  let tools =
    install_tools ~quiet ?refresh:(Some refresh) ?jobs ~proc_mgr ~fs ~clock ~sys
      ~cache ~data_dir ~conf ~os_key ~extra_repos:all_extras ~pins:project.pins
      ?remote ~cwd ()
  in
  let envrc_path = Eio.Path.(fs / cwd / ".envrc") in
  let dune_cache_root = Oi.Cache.dune_root cache in
  let envrc = Oi.Prefix.envrc_content ~prefix ?tools ~dune_cache_root () in
  (try Eio.Path.unlink envrc_path with Eio.Exn.Io _ -> ());
  Eio.Path.save ~create:(`Exclusive 0o644) envrc_path envrc;
  say "Wrote .envrc (run 'direnv allow' to activate)";
  say "Prefix assembled at %s (%d packages)" prefix (List.length layer_hashes);
  prefix

(* True if [cwd]/_oi/prefix is missing, or any *.opam in [cwd] has been
   modified more recently than the prefix directory. *)
let needs_sync ~cwd ~prefix =
  match Unix.stat prefix with
  | exception Unix.Unix_error _ -> true
  | st ->
      let prefix_mtime = st.Unix.st_mtime in
      let opam_files =
        try
          Sys.readdir cwd |> Array.to_list
          |> List.filter (fun f ->
              Filename.check_suffix f ".opam"
              && Filename.chop_suffix f ".opam" <> "")
        with Sys_error _ -> []
      in
      List.exists
        (fun f ->
          try (Unix.stat (cwd / f)).Unix.st_mtime > prefix_mtime
          with Unix.Unix_error _ -> false)
        opam_files

let sync_cmd =
  let run () data_dir cache_dir refresh registry with_repos with_deps jobs =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    let cwd, _ = resolved_cwd fs in
    ignore
      (do_sync ~refresh ~with_repos ~with_deps ?jobs ~proc_mgr ~fs ~clock ~sys
         ~platform ~os_key ~cache ~data_dir ~registry ~cwd ())
  in
  let info =
    Cmd.info "sync" ~doc:"Install project dependencies into _oi/prefix/"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Solve for every dep declared in the directory's $(b,*.opam) files \
             and install into $(b,_oi/prefix/). Write $(b,.envrc) so $(b,PATH) \
             and OCaml environment variables point at it.";
          `P
            "Activate the environment by running $(b,direnv allow) or sourcing \
             $(b,.envrc) manually.";
          `P
            "Rerun after editing $(b,*.opam) files or pulling new pins. $(b,oi \
             exec) auto-syncs, so $(b,oi sync) is only needed for deliberate \
             changes.";
          `S "DEV TOOLS";
          `P "$(b,oi sync) also installs dev tools onto $(b,PATH):";
          `I ("$(b,odoc)", "Documentation generator.");
          `I ("$(b,merlin)", "Editor integration.");
          `I ("$(b,ocaml-lsp-server)", "LSP server for editors.");
          `I ("$(b,mdx)", "Installed when $(b,dune-project) uses it.");
          `I
            ( "$(b,ocamlformat)",
              "Installed at the version $(b,.ocamlformat) declares." );
          `P "Run $(b,oi config) to see the current probe table.";
          `S "OPTIONS";
          `P
            "$(b,--with=PKG) adds extra deps to the solve. See $(b,oi run) for \
             accepted forms.";
          `P
            "$(b,--with-repo=URL|HANDLE) layers an extra opam repository onto \
             the solve.";
          `P "$(b,-j N) caps parallel builds. Default 4.";
          `P
            "$(b,--refresh) forces re-pulls of opam repos, pins, and URL \
             clones.";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ registry_term $ with_repos_term $ with_deps_term $ jobs_term)

(* -- add ----------------------------------------------------------------- *)

(* "op version" split from an opam [version_constraint], as two raw
   strings suitable for {!Oi.Dune_project.add_dependency}. *)
let constr_to_op_ver (op, ver) =
  (OpamFormula.string_of_relop op, OpamPackage.Version.to_string ver)

let add_cmd =
  let run () data_dir cache_dir refresh registry with_repos package pkg_spec =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    let cwd, _ = resolved_cwd fs in
    (* Fail fast before the sync's 10-second repo refresh. *)
    let dp = Oi.Dune_project.load ~fs ~cwd in
    if not (Oi.Dune_project.generate_opam_files dp) then
      Oi.Error.config_error
        "dune-project does not have (generate_opam_files): oi add only \
         supports projects where dune owns the *.opam files";
    (match (package, Oi.Dune_project.package_names dp) with
    | Some p, names when not (List.mem p names) ->
        Oi.Error.config_error
          "no (package (name %s) …) stanza in dune-project (declared: %s)" p
          (if names = [] then "none" else String.concat ", " names)
    | Some _, _ | _, [ _ ] | _, [] -> ()
    | None, many ->
        Oi.Error.config_error
          "multiple packages in dune-project (%s); re-run with -p PKG to pick \
           one"
          (String.concat ", " many));
    let dep = Oi.Script.parse_cli_dep pkg_spec in
    let dep_name = OpamPackage.Name.to_string dep.name in
    let op_ver = Stdlib.Option.map constr_to_op_ver dep.constraint_ in
    let render_constraint = function
      | None -> ""
      | Some (op, ver) -> Fmt.str " %s %s" op ver
    in
    (* Phase 1: prove the solve succeeds with the new dep included. If
       solve fails, [do_sync] raises before we touch any project files. *)
    Fmt.pr "Solving %s%s into the project...@." dep_name
      (render_constraint op_ver);
    ignore
      (do_sync ~refresh ~with_repos ~with_deps:[ pkg_spec ] ~proc_mgr ~fs ~clock
         ~sys ~platform ~os_key ~cache ~data_dir ~registry ~cwd ());
    (* Phase 2: edit dune-project. Reload in case something touched it
       during the sync (shouldn't, but cheap to be defensive). *)
    let dp = Oi.Dune_project.load ~fs ~cwd in
    let dp' =
      Oi.Dune_project.add_dependency dp ?package ~name:dep_name
        ~constraint_:op_ver ()
    in
    Oi.Dune_project.save ~fs dp';
    Fmt.pr "Updated dune-project: added %s%s@." dep_name
      (render_constraint op_ver);
    (* Phase 3: regenerate *.opam via dune build inside the assembled
       prefix — dune itself comes from [_oi/prefix/bin]. *)
    let prefix = cwd / "_oi" / "prefix" in
    let tools = tools_dir_for ~cwd in
    let env =
      Oi.Prefix.make_env ~prefix ?tools
        ~dune_cache_root:(Oi.Cache.dune_root cache) ()
    in
    Fmt.pr "Running dune build to regenerate *.opam...@.";
    ( Eio.Switch.run @@ fun sw ->
      let child =
        Eio.Process.spawn ~sw proc_mgr ~env
          ~cwd:Eio.Path.(fs / cwd)
          [ prefix / "bin" / "dune"; "build" ]
      in
      match Eio.Process.await child with
      | `Exited 0 -> ()
      | `Exited n ->
          Oi.Error.msg
            "dune build exited with code %d; dune-project was updated but \
             *.opam regeneration failed"
            n
      | `Signaled n -> Oi.Error.msg "dune build killed by signal %d" n );
    (* Phase 4: re-sync so the prefix reflects the committed *.opam. *)
    Fmt.pr "Re-syncing to pick up regenerated *.opam...@.";
    ignore
      (do_sync ~quiet:true ~refresh:false ~with_repos ~with_deps:[] ~proc_mgr
         ~fs ~clock ~sys ~platform ~os_key ~cache ~data_dir ~registry ~cwd ());
    Fmt.pr "Done.@."
  in
  let pkg_spec =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"PKG"
          ~doc:
            "Opam package to add, optionally with a version constraint (e.g. \
             $(b,fmt), $(b,fmt.0.9.5), or $(b,fmt>=0.9))."
          [])
  in
  let package =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"NAME"
          ~doc:
            "Which (package …) stanza in dune-project to edit. Required when \
             the file declares more than one package."
          [ "p"; "package" ])
  in
  let info =
    Cmd.info "add" ~doc:"Add a new dependency to the current project"
      ~man:
        [
          `S "DESCRIPTION";
          `P
            "$(b,oi add PKG) pulls a new opam package into your project. First \
             it checks that $(b,PKG) can actually be resolved alongside \
             everything else (and does a sync, so the new package is installed \
             too). Then it edits $(b,dune-project) to record $(b,PKG) as a \
             dependency, runs $(b,dune build) to regenerate the $(b,*.opam) \
             files, and finally re-syncs so the installed toolchain matches \
             the committed source.";
          `P
            "If the pre-flight resolve fails, nothing is written to disk. You \
             can use this to probe whether a dependency would be compatible \
             without committing to the change.";
          `P
            "Your project's $(b,dune-project) needs \
             $(b,\\(generate_opam_files\\)) enabled for this to work, since \
             the command treats the $(b,dune-project) as the source of truth \
             and lets dune regenerate the opam files.";
          `P
            "When the $(b,dune-project) declares multiple packages, use $(b,-p \
             NAME) to pick which one the new dependency is added to.";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ registry_term $ with_repos_term $ package $ pkg_spec)

(* -- exec ---------------------------------------------------------------- *)

let exec_cmd =
  let run () data_dir cache_dir refresh registry with_repos with_deps jobs cmd
      args =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    let cwd, _ = resolved_cwd fs in
    let prefix = cwd / "_oi" / "prefix" in
    (* Any --with-repo / --with flag forces a re-sync even if the prefix is
       fresh, so the extras and extra packages make it into the build. *)
    let forced = with_repos <> [] || with_deps <> [] in
    if forced || needs_sync ~cwd ~prefix then begin
      Logs.info (fun m -> m "Syncing %s before exec" cwd);
      ignore
        (do_sync ~quiet:true ~refresh ~with_repos ~with_deps ?jobs ~proc_mgr ~fs
           ~clock ~sys ~platform ~os_key ~cache ~data_dir ~registry ~cwd ())
    end;
    let tools = tools_dir_for ~cwd in
    let env_arr =
      Oi.Prefix.make_env ~prefix ?tools
        ~dune_cache_root:(Oi.Cache.dune_root cache) ()
    in
    exit (run_exec proc_mgr ~env:env_arr (cmd :: args))
  in
  let cmd =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"CMD" ~doc:"Command to execute" [])
  in
  let args =
    Arg.(
      value & pos_right 0 string []
      & info ~docv:"ARG" ~doc:"Arguments passed to CMD" [])
  in
  let info =
    Cmd.info "exec" ~doc:"Run a command in the project environment"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Run $(b,CMD) with the project's toolchain on $(b,PATH) and OCaml \
             environment variables set.";
          `P
            "Auto-syncs when $(b,_oi/prefix/) is missing or older than any \
             $(b,*.opam) file. Passing $(b,--with) or $(b,--with-repo) forces \
             a re-sync.";
          `P
            "Dev tools ($(b,ocamlformat), $(b,ocamllsp), $(b,odoc), \
             $(b,ocamlmerlin), $(b,ocaml-mdx)) are on $(b,PATH) too.";
          `Pre
            "  oi exec dune build\n\
            \  oi exec -- ocamlformat --check .\n\
            \  oi exec utop";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ registry_term $ with_repos_term $ with_deps_term $ jobs_term $ cmd
      $ args)

(* -- config -------------------------------------------------------------- *)

let config_cmd =
  let run () cache_dir data_dir =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, _clock, sys, _platform, os_key, _cache =
      bootstrap env cache_dir
    in
    Fmt.pr "@[<v>%a@," Fmt.(styled `Bold string) "Platform";
    Fmt.pr "  os-key:     %s@," os_key;
    Fmt.pr "  ocaml:      %s (relocatable)@," ocaml_version;
    Fmt.pr "@,%a@," Fmt.(styled `Bold string) "Directories";
    Fmt.pr "  data:       %s@," data_dir;
    Fmt.pr "  cache:      %s@," cache_dir;
    Fmt.pr "@,%a@," Fmt.(styled `Bold string) "Registry";
    Fmt.pr "  url:        %s@," default_registry;
    Fmt.pr "  index TTL:  %gs@," remote_index_max_age;
    Fmt.pr "@,%a@," Fmt.(styled `Bold string) "Base overlays (from reporepo)";
    let base = Oi.Reporepo.base_entries () in
    if base = [] then
      Fmt.pr
        "  %a no 'relocatable' overlay in reporepo %s — run 'oi repo add'@,"
        Fmt.(styled `Yellow string)
        "(none)"
        (match Sys.getenv_opt "OI_REPOREPO" with
        | Some v when v <> "" -> v
        | _ -> Oi.Reporepo.default_path)
    else
      List.iter
        (fun (e : Oi.Reporepo.entry) ->
          let name = "overlay-" ^ e.handle ^ "-" ^ e.version in
          let dir = Oi.Repo.repo_dir ~data_dir name in
          let status =
            if Sys.file_exists (dir / ".git") then
              let hash =
                try D10.Sysops.Git.head_short sys ~dir:Eio.Path.(fs / dir)
                with _ -> "?"
              in
              Fmt.str "%a (%s)" Fmt.(styled `Green string) "cloned" hash
            else Fmt.str "%a" Fmt.(styled `Yellow string) "not cloned"
          in
          Fmt.pr "  %a.%s  %s  %s@,"
            Fmt.(styled `Bold string)
            e.handle e.version status e.url)
        base;
    Fmt.pr "@]@.";
    let cwd_s, _ = resolved_cwd fs in
    let proj =
      match Oi.Project.load ~fs cwd_s with
      | exception Sys_error _ -> None
      | exception Eio.Exn.Io _ -> None
      | p -> Some p
    in
    match proj with
    | None -> ()
    | Some p ->
        if p.extra_repos <> [] then begin
          Fmt.pr "@.Project extra repositories:@.";
          List.iter
            (fun (r : Oi.Project.extra_repo) ->
              Fmt.pr "  %-20s %s@." r.name r.url)
            p.extra_repos
        end;
        if p.pins <> [] then begin
          Fmt.pr "@.Project pin-depends:@.";
          List.iter
            (fun (pin : Oi.Project.pin) ->
              Fmt.pr "  %-20s %s@."
                (OpamPackage.to_string pin.pkg)
                (OpamUrl.to_string pin.url))
            p.pins
        end;
        if p.overlays <> [] then begin
          Fmt.pr "@.Project overlays (x-reporepo):@.";
          List.iter (fun h -> Fmt.pr "  %s@." h) p.overlays
        end;
        (* Dev tools: run the probe registry against cwd and print one
           row per tool. Shown in every project (even one with no
           hits), so it's obvious when merlin / odoc would end up in
           [_oi/tools/] after the next sync. *)
        let tool_results = Oi.Tool.probe ~fs cwd_s in
        Fmt.pr "@.Dev tools:@.";
        List.iter
          (fun (r : Oi.Tool.result) ->
            let mark =
              if r.hit then Fmt.str "%a" Fmt.(styled `Green string) "hit"
              else Fmt.str "%a" Fmt.(styled `Faint string) "miss"
            in
            Fmt.pr "  %-18s %-4s %s@." r.spec.name mark r.detail)
          tool_results
  in
  let info =
    Cmd.info "config" ~doc:"Show oi's view of this machine and project"
      ~man:
        [
          `S Manpage.s_description;
          `P "Summarise platform, caches, repositories, and project state.";
          `I
            ( "$(b,Platform)",
              "OS, architecture, distro. Check here first when a solve picks \
               unexpected packages." );
          `I
            ( "$(b,Directories)",
              "Cache and data directories. Honours $(b,OI_CACHE_DIR) and \
               $(b,OI_DATA_DIR); falls back to XDG." );
          `I
            ( "$(b,Repositories)",
              "Opam repo clones with last-updated timestamps." );
          `I
            ( "$(b,Project extra repositories / pins / overlays)",
              "Shown when the current directory's $(b,*.opam) files declare \
               $(b,x-opam-repositories:), $(b,pin-depends:), or \
               $(b,x-reporepo:)." );
          `I
            ( "$(b,Dev tools)",
              "Which tools the next $(b,oi sync) will install. $(b,hit) means \
               yes, $(b,miss) means no." );
        ]
  in
  Cmd.v info Term.(const run $ log_term $ cache_dir_term $ data_dir_term)

(* -- clean --------------------------------------------------------------- *)

(* dir_size and pp_size are now in Oi.Cache *)

let clean_cmd =
  let run () cache_dir data_dir all toolchains sources binaries dune_cache repos
      dry_run =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, _clock, sys, _platform, _os_key, cache =
      bootstrap env cache_dir
    in
    let clean_any =
      all || toolchains || sources || binaries || dune_cache || repos
    in
    if not clean_any then begin
      Fmt.pr "@[<v>%a@,@," Fmt.(styled `Bold string) "Cleanable items:";
      let items = Oi.Cache.cleanable_items cache ~data_dir in
      List.iter
        (fun (item : Oi.Cache.item) ->
          let path_s = Eio.Path.native_exn item.path in
          if Sys.file_exists path_s then
            Fmt.pr "  --%-20s %a  %s@," item.label Oi.Cache.pp_size
              (Oi.Cache.size ~sys item.path)
              item.description
          else
            Fmt.pr "  --%-20s %a  %s@," item.label
              Fmt.(styled `Faint string)
              "(empty)" item.description)
        items;
      Fmt.pr "@,Use --all to clean everything, or select specific items.@]@."
    end
    else begin
      let items = Oi.Cache.cleanable_items cache ~data_dir in
      let find_item label =
        List.find_opt (fun (i : Oi.Cache.item) -> i.label = label) items
      in
      let rm label =
        match find_item label with
        | None -> ()
        | Some item ->
            let path_s = Eio.Path.native_exn item.path in
            if Sys.file_exists path_s then begin
              let sz = Oi.Cache.size ~sys item.path in
              if dry_run then
                Fmt.pr "Would remove %s (%a) %s@." label Oi.Cache.pp_size sz
                  path_s
              else begin
                Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / path_s);
                Fmt.pr "Removed %s (%a)@." label Oi.Cache.pp_size sz
              end
            end
      in
      if all || toolchains then rm "toolchains";
      if all || sources then rm "sources";
      if all || binaries then rm "layers";
      if all || binaries then rm "runs";
      if all || dune_cache then rm "dune";
      if all || repos then rm "repos";
      Fmt.pr "Done.@."
    end
  in
  let all =
    Arg.(
      value & flag
      & info ~doc:"Remove everything (caches, builds, config, repos)" [ "all" ])
  in
  let toolchains =
    Arg.(
      value & flag
      & info ~doc:"Remove cached toolchain tarballs" [ "toolchains" ])
  in
  let sources =
    Arg.(value & flag & info ~doc:"Remove cached source tarballs" [ "sources" ])
  in
  let binaries =
    Arg.(
      value & flag
      & info ~doc:"Remove binary layer cache and script builds" [ "layers" ])
  in
  let dune_cache =
    Arg.(value & flag & info ~doc:"Remove dune shared build cache" [ "dune" ])
  in
  let repos =
    Arg.(value & flag & info ~doc:"Remove cloned repositories" [ "repos" ])
  in
  let dry_run =
    Arg.(
      value & flag
      & info ~doc:"Show what would be removed without deleting"
          [ "dry-run"; "n" ])
  in
  let info =
    Cmd.info "clean" ~doc:"Free up disk space by deleting cached data"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "With no flags, this command only $(i,lists) what it could clean \
             up, along with how much disk each category is using. It doesn't \
             remove anything unless you ask. Flags are additive; combine as \
             many as you like.";
          `I
            ( "$(b,--toolchains)",
              "Remove the OCaml compilers $(b,oi) downloaded. A later $(b,oi \
               run) will re-download them." );
          `I
            ( "$(b,--sources)",
              "Remove cached source tarballs and pinned source clones. A later \
               build will refetch sources from upstream." );
          `I
            ( "$(b,--binaries)",
              "Remove the pre-built packages that make subsequent runs fast. \
               Everything will be rebuilt from source on the next $(b,oi run)."
            );
          `I
            ( "$(b,--dune-cache)",
              "Remove the shared build cache that $(b,dune) uses across \
               projects. Purely a performance hit on the next build." );
          `I
            ( "$(b,--repos)",
              "Remove the clones of the opam package index and any extra \
               repositories. The next solve will refetch them." );
          `I
            ( "$(b,--all)",
              "Delete every category above, plus the assembled prefix caches \
               and script build dirs. Effectively a 'reset' that undoes \
               everything $(b,oi) has ever cached." );
          `P
            "Use $(b,-n) or $(b,--dry-run) to see what would be deleted before \
             committing. Recommended before $(b,--all).";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ cache_dir_term $ data_dir_term $ all $ toolchains
      $ sources $ binaries $ dune_cache $ repos $ dry_run)

(* -- registry show ------------------------------------------------------- *)

let registry_show_cmd =
  let run () cache_dir _data_dir target =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, _clock, sys, _platform, os_key, _cache =
      bootstrap env cache_dir
    in
    let layers_dir = cache_dir / "layers" / os_key in
    match target with
    | None ->
        (* Show overview of all layers *)
        Fmt.pr "@[<v>%a %s@,@," Fmt.(styled `Bold string) "Layer cache" os_key;
        if not (Sys.file_exists layers_dir) then Fmt.pr "  (empty)@,"
        else begin
          let entries =
            Sys.readdir layers_dir |> Array.to_list |> List.sort String.compare
          in
          let total_size = ref 0L in
          List.iter
            (fun hash ->
              let info =
                D10.Layer.load_meta
                  Eio.Path.(fs / layers_dir / hash / "layer.json")
              in
              match info with
              | Some i ->
                  let status =
                    if i.exit_status = 0 then
                      Fmt.str "%a" Fmt.(styled `Green string) "ok"
                    else
                      Fmt.str "%a (exit %d)"
                        Fmt.(styled `Red string)
                        "fail" i.exit_status
                  in
                  let fs_dir = layers_dir / hash / "fs" in
                  let sz = Oi.Cache.size ~sys Eio.Path.(fs / fs_dir) in
                  total_size := Int64.add !total_size sz;
                  Fmt.pr "  %a  %s  %a  %s@,"
                    Fmt.(styled `Faint string)
                    (String.sub hash 0 (min 12 (String.length hash)))
                    status Oi.Cache.pp_size sz i.package
              | None ->
                  Fmt.pr "  %a  %a@,"
                    Fmt.(styled `Faint string)
                    (String.sub hash 0 (min 12 (String.length hash)))
                    Fmt.(styled `Yellow string)
                    "(no metadata)")
            entries;
          Fmt.pr "@,%a %d layers, %a total@,"
            Fmt.(styled `Bold string)
            "Summary:" (List.length entries) Oi.Cache.pp_size !total_size
        end;
        Fmt.pr "@]@."
    | Some pkg_name ->
        (* Show details for a specific package *)
        Fmt.pr "@[<v>%a %s@,@," Fmt.(styled `Bold string) "Package" pkg_name;
        (* Find matching layers *)
        let found = ref false in
        if Sys.file_exists layers_dir then begin
          let entries = Sys.readdir layers_dir |> Array.to_list in
          List.iter
            (fun hash ->
              let info =
                D10.Layer.load_meta
                  Eio.Path.(fs / layers_dir / hash / "layer.json")
              in
              match info with
              | Some i
                when String.length i.package >= String.length pkg_name
                     && String.sub i.package 0 (String.length pkg_name)
                        = pkg_name ->
                  found := true;
                  Fmt.pr "  %a %s@," Fmt.(styled `Bold string) "Layer" hash;
                  Fmt.pr "  package:     %s@," i.package;
                  Fmt.pr "  status:      %s@,"
                    (if i.exit_status = 0 then "ok"
                     else Fmt.str "failed (exit %d)" i.exit_status);
                  Fmt.pr "  created:     %s@,"
                    (let t = Unix.gmtime i.created in
                     Fmt.str "%04d-%02d-%02d %02d:%02d:%02d UTC"
                       (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour
                       t.tm_min t.tm_sec);
                  Fmt.pr "  deps:        %s@,"
                    (if i.deps = [] then "(none)" else String.concat ", " i.deps);
                  Fmt.pr "  parent hash: %s@,"
                    (if i.hashes = [] then "(none)"
                     else
                       String.concat ", "
                         (List.map
                            (fun h -> String.sub h 0 (min 12 (String.length h)))
                            i.hashes));
                  let fs_dir = layers_dir / hash / "fs" in
                  if Sys.file_exists fs_dir then begin
                    let sz = Oi.Cache.size ~sys Eio.Path.(fs / fs_dir) in
                    Fmt.pr "  size:        %a@," Oi.Cache.pp_size sz;
                    (* List files in fs/ *)
                    let files = ref [] in
                    let rec scan dir =
                      if Sys.file_exists dir && Sys.is_directory dir then
                        Array.iter
                          (fun name ->
                            let path = dir / name in
                            if Sys.is_directory path then scan path
                            else
                              let rel =
                                String.sub path
                                  (String.length fs_dir + 1)
                                  (String.length path - String.length fs_dir - 1)
                              in
                              files := rel :: !files)
                          (Sys.readdir dir)
                    in
                    scan fs_dir;
                    let files = List.sort String.compare !files in
                    Fmt.pr "  files:       %d@," (List.length files);
                    if List.length files <= 20 then
                      List.iter (fun f -> Fmt.pr "    %s@," f) files
                    else begin
                      List.iteri
                        (fun i f -> if i < 10 then Fmt.pr "    %s@," f)
                        files;
                      Fmt.pr "    ... (%d more)@," (List.length files - 10)
                    end
                  end;
                  Fmt.pr "@,"
              | _ -> ())
            entries
        end;
        if not !found then Fmt.pr "  No layers found for %s@," pkg_name;
        Fmt.pr "@]@."
  in
  let target =
    Arg.(
      value
      & pos 0 (some string) None
      & info ~docv:"PACKAGE" ~doc:"Package name to inspect (omit for overview)"
          [])
  in
  let info =
    Cmd.info "show"
      ~doc:"Summarise what's in the local cache of pre-built packages"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "With no argument, prints how many packages are cached locally, \
             how much disk they take, and which ones failed to build (if any). \
             Useful as a quick sanity check on the state of the cache.";
          `P
            "Pass a $(b,PACKAGE) name to drill into that specific entry. \
             You'll see its exact cached version, the build hash, the list of \
             direct dependencies that went into it, and the files it \
             installed.";
        ]
  in
  Cmd.v info
    Term.(const run $ log_term $ cache_dir_term $ data_dir_term $ target)

(* -- registry index ------------------------------------------------------ *)

let registry_index_cmd =
  let run () cache_dir =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, clock, sys, _platform, _os_key, _cache =
      bootstrap env cache_dir
    in
    let layers_root = cache_dir / "layers" in
    let total_layers = ref 0 in
    let total_bins = ref 0 in
    let total_files = ref 0 in
    if Sys.file_exists layers_root then
      Array.iter
        (fun entry ->
          let dir = layers_root / entry in
          if Sys.is_directory dir && entry.[0] <> '.' then begin
            let index_path = dir / "index.db" in
            let db = D10.Index.open_ ~path:index_path in
            D10.Index.rebuild
              {
                D10.Config.sys;
                fs;
                clock :> D10.Config.clk;
                root = Eio.Path.(fs / cache_dir);
                os_key = entry;
              }
              db;
            let nl, nb, nf = D10.Index.stats db ~os_key:entry in
            D10.Index.close db;
            Fmt.pr "  %s: %d layers, %d binaries, %d files@." entry nl nb nf;
            total_layers := !total_layers + nl;
            total_bins := !total_bins + nb;
            total_files := !total_files + nf
          end)
        (Sys.readdir layers_root);
    Fmt.pr "Total: %d layers, %d binaries, %d files@." !total_layers !total_bins
      !total_files
  in
  let info =
    Cmd.info "index" ~doc:"Rebuild the fast-lookup index over the local cache"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi) keeps a small SQLite database alongside the cache that \
             maps binaries to the packages that ship them. It's what powers \
             $(b,oi which) and the binary-mode fallback in $(b,oi run). This \
             command rebuilds that database from scratch by walking every \
             cached package.";
          `P
            "You normally don't need to run this. Reach for it if $(b,oi \
             which) starts missing a binary you know is cached, or after a \
             manual edit of the cache directory.";
        ]
  in
  Cmd.v info Term.(const run $ log_term $ cache_dir_term)

(* -- registry ------------------------------------------------------------ *)

(* Remove a sqlite scratch file together with its WAL/SHM siblings.
   sqlite in WAL journal_mode leaves [-wal] and [-shm] files next to
   the main [.db] on close, and plain [Sys.remove] on just the [.db]
   leaves orphans behind — visible in the published sources/ tree. *)
let remove_sqlite_scratch path =
  List.iter
    (fun p -> try Sys.remove p with Sys_error _ -> ())
    [ path; path ^ "-wal"; path ^ "-shm"; path ^ "-journal" ]

(* Collapse any WAL/SHM sidecars next to [path] into the main database.
   Runs [PRAGMA journal_mode=DELETE], which checkpoints outstanding WAL
   pages into the main file and removes the [-wal]/[-shm] files. Used
   at the tail of [registry export] so the published index.db files
   are self-contained — rsync'ing the sources/ tree doesn't need to
   copy or create WAL siblings on the remote. *)
let finalize_sqlite_for_publish path =
  if Sys.file_exists path then begin
    (try
       let db = Sqlite3.db_open path in
       Fun.protect
         ~finally:(fun () -> ignore (Sqlite3.db_close db))
         (fun () -> ignore (Sqlite3.exec db "PRAGMA journal_mode=DELETE"))
     with _ -> ());
    (* sqlite's WAL→DELETE transition truncates the [-wal] but may
       leave the zero-byte [-shm] sidecar behind. At this point both
       are orphans — the main db owns no WAL state — so unlink any
       leftovers directly. *)
    List.iter
      (fun suffix -> try Sys.remove (path ^ suffix) with Sys_error _ -> ())
      [ "-wal"; "-shm"; "-journal" ]
  end

(* Fetch [registry]/<rel> to [dst] via curl. Returns true on success,
   false otherwise (404, network error, empty response). The caller
   decides how to react (typically: skip the remote merge). *)
let fetch_remote_to ~sys ~fs ~registry ~rel ~dst =
  if registry = "" then false
  else begin
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
      Eio.Path.(fs / Filename.dirname dst);
    D10.Sysops.Curl.fetch sys ~url:(url_join registry rel)
      ~dst:Eio.Path.(fs / dst)
  end

(* Body of [oi registry export]; kept as its own function so other
   callers (tests, future commands) can drive it without going
   through cmdliner. *)
let do_registry_export ~fs ~clock ~sys ~os_key ~cache ~registry ~output =
  let d10 = make_d10 ~sys ~fs ~clock ~cache ~os_key in
  let dst = Eio.Path.(fs / output) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dst;
  let count = D10.Layer.export_all d10 ~dst in
  Fmt.pr "Exported %d layer(s) to %s@." count output;
  (* Rebuild the index.db only for this container's os_key. Sibling os_key
     subdirs may exist alongside ours when [dst] is a shared volume (e.g.
     docker-compose bind mount) — leave their indices alone. *)
  if Sys.file_exists (output / os_key) then begin
    let index_path = output / os_key / "index.db" in
    (try Sys.remove index_path with Sys_error _ -> ());
    let db = D10.Index.open_ ~path:index_path in
    D10.Index.rebuild d10 db;
    (* If a remote registry is configured, fetch its current
       <os_key>/index.db into a scratch file and merge those rows
       in. This keeps rows for layers that live on the remote but
       haven't been rebuilt locally this run — important for rsync:
       without it, the published index would shrink to just what the
       caller happens to have cached. *)
    if registry <> "" then begin
      let scratch = output / os_key / ".remote-index.db" in
      if
        fetch_remote_to ~sys ~fs ~registry ~rel:(os_key / "index.db")
          ~dst:scratch
      then begin
        (try D10.Index.merge_remote db ~remote_path:scratch
         with Failure msg ->
           Logs.warn (fun m -> m "Failed to merge remote layer index: %s" msg));
        remove_sqlite_scratch scratch
      end
      else
        Logs.info (fun m ->
            m "No remote layer index at %s/%s/index.db (skipping merge)"
              registry os_key)
    end;
    let nl, nb, _ = D10.Index.stats db ~os_key in
    D10.Index.close db;
    finalize_sqlite_for_publish index_path;
    Fmt.pr "  %s: %d layers, %d binaries@." os_key nl nb
  end;
  (* Sources are OS-independent — publish them once at the registry
     top level (sources/), not per os_key. A sibling [oi registry
     export] from a different arch/distro will merge into the same
     tree: blobs are content-addressed so collisions are correctness-
     preserving. *)
  let n_sources = Oi.Source_mirror.export ~cache ~dst in
  if registry <> "" then begin
    let scratch = output / "sources" / ".remote-index.db" in
    if fetch_remote_to ~sys ~fs ~registry ~rel:"sources/index.db" ~dst:scratch
    then begin
      let index_path = output / "sources" / "index.db" in
      (try Oi.Source_mirror.merge_remote ~fs ~index_path ~remote_path:scratch
       with Failure msg ->
         Logs.warn (fun m -> m "Failed to merge remote sources index: %s" msg));
      remove_sqlite_scratch scratch
    end
    else
      Logs.info (fun m ->
          m "No remote sources index at %s/sources/index.db (skipping merge)"
            registry)
  end;
  finalize_sqlite_for_publish (output / "sources" / "index.db");
  if n_sources > 0 then
    Fmt.pr "  sources: %d blob(s) at %s/sources/@." n_sources output

let registry_export_cmd =
  let run () cache_dir registry output =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, clock, sys, _platform, os_key, cache =
      bootstrap env cache_dir
    in
    do_registry_export ~fs ~clock ~sys ~os_key ~cache ~registry ~output
  in
  let output =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"DIR" ~doc:"Output directory for the registry" [])
  in
  let info =
    Cmd.info "export"
      ~doc:"Publish the local cache to a directory for HTTP serving or rsync"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Copies every locally-cached package into $(b,DIR) in the layout \
             an $(b,oi) client expects when it points at a remote registry. \
             The result is a tree of compressed archives plus a sqlite \
             $(b,index.db) and a sha256 $(b,OINDEX.txt) per os_key, ready to \
             be served over HTTP (static file hosting is fine) or $(b,rsync)'d \
             to another machine.";
          `P
            "The index carries each layer's overlay handle/version, so clients \
             that want to scope to a specific overlay can query the index \
             directly — there's no separate per-overlay tree to fetch from. \
             Use $(b,oi which) to inspect what overlays the published index \
             covers.";
          `P
            "If $(b,--registry URL) is given, $(b,oi) first downloads the \
             registry's existing index and merges those rows into the one \
             being published. This matters when you $(b,rsync) back to a \
             shared registry: without the merge, your export would overwrite \
             entries contributed by other machines.";
          `P
            "Source tarballs are published once at the top of $(b,DIR) (under \
             $(b,sources/)), not per OS, because source code is the same \
             regardless of which distro will consume it.";
        ]
  in
  Cmd.v info
    Term.(const run $ log_term $ cache_dir_term $ registry_term $ output)

(* Render the per-target summary emitted at the end of [oi registry build].
   One row per target, in the order the user asked for them. Columns are
   truncated/padded so the table stays readable even with long overlay
   package names. Group-level counts are repeated across all targets that
   shared a group — two targets under the same solver solution get the same
   "packages / built / cached" figures, which matches how the build itself
   treated them. *)
let print_build_summary ~targets ~target_handle ~solve_failures ~target_group
    ~group_results =
  let module R = struct
    type t =
      | Skipped of string  (** solver failure *)
      | Ok of int * int * int
      | Failed of int * int * int * string
  end in
  let result_for t =
    match Hashtbl.find_opt solve_failures t with
    | Some msg -> R.Skipped msg
    | None -> (
        match Hashtbl.find_opt target_group t with
        | None -> R.Skipped "unknown"
        | Some gi -> (
            match Hashtbl.find_opt group_results gi with
            | None -> R.Skipped "group not built"
            | Some (`Ok (p, b, c)) -> R.Ok (p, b, c)
            | Some (`Fail (p, b, c, msg)) -> R.Failed (p, b, c, msg)))
  in
  let handle_for t =
    match Hashtbl.find_opt target_handle t with
    | Some h -> "@" ^ h
    | None -> ""
  in
  let rows = List.map (fun t -> (t, handle_for t, result_for t)) targets in
  let n_ok, n_failed, n_skipped =
    List.fold_left
      (fun (o, f, s) (_, _, r) ->
        match r with
        | R.Ok _ -> (o + 1, f, s)
        | R.Failed _ -> (o, f + 1, s)
        | R.Skipped _ -> (o, f, s + 1))
      (0, 0, 0) rows
  in
  let status_col r =
    match r with
    | R.Ok _ -> Fmt.str "%a" Fmt.(styled `Green string) "ok"
    | R.Failed _ -> Fmt.str "%a" Fmt.(styled `Red string) "fail"
    | R.Skipped _ -> Fmt.str "%a" Fmt.(styled `Yellow string) "skip"
  in
  (* Shorten multi-line solver diagnostics to the first line so the
     table stays readable. Full detail is still logged per-target
     below. *)
  let first_line s =
    match String.index_opt s '\n' with None -> s | Some i -> String.sub s 0 i
  in
  let detail_col r =
    match r with
    | R.Ok (p, b, c) -> Fmt.str "%d pkg (%d built, %d cached)" p b c
    | R.Failed (p, b, c, _) ->
        Fmt.str "%d pkg (%d built, %d cached) — build failed" p b c
    | R.Skipped msg -> Fmt.str "skipped (%s)" (first_line msg)
  in
  let target_width =
    List.fold_left (fun w (t, _, _) -> max w (String.length t)) 12 rows
  in
  let handle_width =
    List.fold_left (fun w (_, h, _) -> max w (String.length h)) 0 rows
  in
  let styled_handle h =
    if h = "" then String.make handle_width ' '
    else
      (* [Fmt.str] with styling inflates the visible length with ANSI
         codes; pad first, then colour. *)
      let padded = Fmt.str "%-*s" handle_width h in
      Fmt.str "%a" Fmt.(styled `Cyan string) padded
  in
  Fmt.pr "@.";
  List.iter
    (fun (target, handle, r) ->
      if handle_width = 0 then
        Fmt.pr "  %-6s %-*s  %s@." (status_col r) target_width target
          (detail_col r)
      else
        Fmt.pr "  %-6s %s  %-*s  %s@." (status_col r) (styled_handle handle)
          target_width target (detail_col r))
    rows;
  Fmt.pr "@.%d ok, %d failed, %d skipped@." n_ok n_failed n_skipped;
  (* Dump per-target build-failure output at debug level so `-v` still
     shows the reason, without dumping a compiler transcript by
     default. *)
  List.iter
    (fun (target, _handle, r) ->
      match r with
      | R.Failed (_, _, _, msg) -> Log.info (fun m -> m "%s: %s" target msg)
      | R.Skipped msg when String.contains msg '\n' ->
          Log.info (fun m -> m "%s: %s" target msg)
      | _ -> ())
    rows

let registry_build_cmd =
  let run () data_dir cache_dir refresh dry_run all only skip registry
      with_repos with_deps jobs targets =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    init_opam_root ~fs ~data_dir;
    ignore (get_packages_dirs ~fs ~sys ~data_dir ~refresh ());
    let conf = make_conf ~platform in
    let remote = remote_of_registry registry in
    (* When [--all] is set, walk every overlay in the reporepo and
       derive targets from each one:
       - skip [default] (ocaml/opam-repository) — its ~10k packages
         are never what [--all] should mean;
       - if the overlay has [x-root-packages], emit one [@handle/pkg]
         per entry;
       - otherwise fall back to [@handle], which expands to every
         package the overlay's clone ships.
       [--only] restricts to named handles; [--skip] excludes them.
       [default] can still be included by explicitly listing it via
       [--only default]. *)
    let reporepo_target_groups =
      if not all then []
      else begin
        let path = reporepo_path () in
        Oi.Reporepo.ensure_clone ~fs ~sys ~refresh ~path
          ~url:(reporepo_url ());
        let entries = Oi.Reporepo.load ~path in
        let only_set =
          if only = [] then None else Some (List.sort_uniq compare only)
        in
        let skip_set = List.sort_uniq compare skip in
        let handles =
          List.map (fun (e : Oi.Reporepo.entry) -> e.handle) entries
          |> List.sort_uniq String.compare
        in
        List.concat_map
          (fun h ->
            let default_skipped =
              h = "default"
              && (match only_set with None -> true | Some s -> not (List.mem h s))
            in
            if default_skipped then begin
              Log.info (fun m ->
                  m "--all: skipping %s (pass --only default to include)" h);
              []
            end
            else
              let included =
                (match only_set with None -> true | Some s -> List.mem h s)
                && not (List.mem h skip_set)
              in
              if not included then []
              else
                match Oi.Reporepo.latest entries ~handle:h with
                | None -> []
                | Some e ->
                    if e.root_packages = [] then begin
                      Log.info (fun m ->
                          m
                            "--all: overlay %s has no x-root-packages, \
                             expanding to every package in the overlay"
                            h);
                      [ [ "@" ^ h ] ]
                    end
                    else
                      List.map
                        (fun group ->
                          List.map (fun p -> "@" ^ h ^ "/" ^ p) group)
                        e.root_packages)
          handles
      end
    in
    (* Each CLI-supplied target is its own (singleton) solve group,
       preserving the previous behaviour where [oi registry build a b]
       solved [a] and [b] independently. Reporepo groups may be
       multi-element (compiler variants etc.). *)
    let target_groups =
      List.map (fun t -> [ t ]) targets @ reporepo_target_groups
    in
    let targets = List.concat target_groups in
    if targets = [] then begin
      if all then
        Oi.Error.config_error
          "--all expanded to nothing in %s (all overlays filtered by \
           --skip/--only, or the reporepo only contains 'default')"
          (reporepo_path ())
      else
        Oi.Error.config_error
          "no targets to build (pass PKG arguments or --all)"
    end;
    (* Classify each input into a plain target or an overlay form.
       Overlay forms collect handles to thread through [with_repos]
       so the later [cli_extra_repos] run clones them up front. The
       "build everything in this overlay" form is expanded once the
       clones exist. *)
    let parsed = List.map parse_build_target targets in
    let with_repos =
      let handles =
        List.filter_map
          (function
            | Plain_target _ -> None
            | Overlay_pkg (h, _) | Overlay_all h -> Some h)
          parsed
        |> List.sort_uniq String.compare
      in
      with_repos @ handles
    in
    let extra_cli, url_project =
      materialize_with_deps ~fs ~sys ~cache ~refresh with_deps
    in
    let with_repos = with_repos @ url_project.overlays in
    let cli_extras_records =
      merge_extras
        ~cli:(cli_extra_repos ~fs ~sys with_repos)
        ~project:url_project.extra_repos
    in
    let extra_pkg_dirs =
      Oi.Repo.ensure_extra ~fs ~data_dir ~refresh cli_extras_records
    in
    (* URL-project pins materialize into a synthetic packages/ tree
       the solver consumes ahead of everything else, so the URL's
       dev-version of each local package wins over any stable version
       from the opam-repository. *)
    let pin_dir =
      Oi.Pin.materialize ~fs ~sys ~cache ~refresh url_project.pins
    in
    (* Expand [@handle] into every package the overlay's clone
       provides. List just the top-level names under the overlay's
       [packages/] dir — the solver will pick specific versions. *)
    let overlay_packages handle =
      let entries = Oi.Reporepo.load ~path:(reporepo_path ()) in
      match Oi.Reporepo.latest entries ~handle with
      | None -> Oi.Error.config_error "no overlay %s in reporepo" handle
      | Some e ->
          let dir =
            data_dir / "repos"
            / ("overlay-" ^ e.handle ^ "-" ^ e.version)
            / "packages"
          in
          if not (Sys.file_exists dir) then
            Oi.Error.config_error "overlay %s clone has no packages/ tree at %s"
              handle dir;
          Sys.readdir dir |> Array.to_list
          |> List.filter (fun n -> Sys.is_directory (dir / n))
          |> List.sort String.compare
    in
    (* Remember which handle each target came from so the summary
       table can render a column for it. Targets from plain PKG
       arguments have no handle. If two overlays contribute the same
       bare package name the later one wins for display purposes; the
       solver still sees them as a single target. *)
    let target_handle : (string, string) Hashtbl.t = Hashtbl.create 16 in
    let targets =
      List.concat_map
        (function
          | Plain_target t -> [ t ]
          | Overlay_pkg (h, pkg_spec) ->
              Hashtbl.replace target_handle pkg_spec h;
              [ pkg_spec ]
          | Overlay_all h ->
              let ps = overlay_packages h in
              List.iter (fun p -> Hashtbl.replace target_handle p h) ps;
              Log.info (fun m ->
                  m "Overlay %s: %d package(s) to build" h (List.length ps));
              ps)
        parsed
    in
    (* [--dry-run --all] prints the expanded target list (with handles
       and the latest version each overlay ships) and stops before
       solving. Useful to audit what [--all] would attempt without
       paying the solver's cost. Non-[--all] dry-runs keep the existing
       per-group build-plan tree output downstream. *)
    if dry_run && all then begin
      let n = List.length targets in
      Fmt.pr "@.%a@." Fmt.(styled `Bold string)
        (Fmt.str "--all would build %d target%s:" n (if n = 1 then "" else "s"));
      (* Resolve each handle once to its cloned packages/ dir so we can
         look up the latest version of each package it contributes.
         Falls back gracefully if the overlay isn't cloned yet. *)
      let handle_dir = Hashtbl.create 8 in
      let entries = Oi.Reporepo.load ~path:(reporepo_path ()) in
      let dir_for_handle h =
        match Hashtbl.find_opt handle_dir h with
        | Some v -> v
        | None ->
            let v =
              match Oi.Reporepo.latest entries ~handle:h with
              | None -> None
              | Some e ->
                  let d =
                    data_dir / "repos"
                    / ("overlay-" ^ e.handle ^ "-" ^ e.version)
                    / "packages"
                  in
                  if Sys.file_exists d then Some d else None
            in
            Hashtbl.replace handle_dir h v;
            v
      in
      let bare_name t =
        (* Strip version / relop suffixes so [latest_version_in_dirs]
           can find the package entry. *)
        let stop = [ '='; '<'; '>'; '.'; '{' ] in
        let len = String.length t in
        let rec find i =
          if i >= len then len
          else if List.mem t.[i] stop then i
          else find (i + 1)
        in
        String.sub t 0 (find 0)
      in
      let version_for t =
        match Hashtbl.find_opt target_handle t with
        | None -> None
        | Some h -> (
            match dir_for_handle h with
            | None -> None
            | Some d -> latest_version_in_dirs ~pkg:(bare_name t) [ d ])
      in
      let handle_w =
        List.fold_left
          (fun w t ->
            match Hashtbl.find_opt target_handle t with
            | Some h -> max w (String.length h + 1)
            | None -> w)
          0 targets
      in
      let target_w =
        List.fold_left (fun w t -> max w (String.length t)) 0 targets
      in
      List.iter
        (fun t ->
          let handle_cell =
            match Hashtbl.find_opt target_handle t with
            | Some h ->
                Fmt.str "%a" Fmt.(styled `Cyan string) ("@" ^ h)
            | None -> ""
          in
          let version_cell =
            match version_for t with
            | Some v -> Fmt.str "%a" Fmt.(styled `Faint string) v
            | None -> ""
          in
          (* [handle_w] is used raw as the visible width, even though
             [handle_cell] has ANSI codes — we pad the plain-text
             handle, then colourise. *)
          let handle_plain =
            match Hashtbl.find_opt target_handle t with
            | Some h -> "@" ^ h
            | None -> ""
          in
          let handle_pad = String.make (handle_w - String.length handle_plain) ' ' in
          Fmt.pr "  %s%s  %-*s  %s@." handle_cell handle_pad target_w t
            version_cell)
        targets;
      Fmt.pr "@.";
      exit 0
    end;
    if targets = [] && url_project.roots = [] then
      Oi.Error.config_error "no targets to build";
    let packages_dirs =
      Stdlib.Option.to_list pin_dir
      @ extra_pkg_dirs
      @ get_packages_dirs ~fs ~sys ~data_dir ()
    in
    let cache_root = Oi.Cache.root_s cache in
    let build_prefix = cache_root / "build" / "prefix" in
    let ctx = Oi.Opam_ctx.create ~prefix:build_prefix ~packages_dirs ~conf in
    let d10 = make_d10 ~sys ~fs ~clock ~cache ~os_key in
    (* [--with] adds extra packages to every target's root set plus any
       version constraints they carry. *)
    let base_constraints = Oi.Script.constraints extra_cli in
    let extra_names =
      List.filter_map
        (fun (d : Oi.Script.dep) ->
          if OpamPackage.Name.to_string d.name = "ocaml" then None
          else Some d.name)
        extra_cli
      @ List.map OpamPackage.Name.of_string url_project.roots
    in
    let target_groups =
      target_groups @ List.map (fun r -> [ r ]) url_project.roots
    in
    let targets = List.concat target_groups in
    (* Per-target result tracking; the final summary walks [targets] in
       order and looks each name up here. A target either fails to
       solve (status stored directly), or lands in some group. Groups
       are keyed by index; their build result (ok / failed, with the
       package counts) is written into [group_results] when the group
       finishes. *)
    let solve_failures : (string, string) Hashtbl.t = Hashtbl.create 16 in
    let target_group : (string, int) Hashtbl.t = Hashtbl.create 16 in
    let group_results :
        ( int,
          [ `Ok of int * int * int | `Fail of int * int * int * string ] )
        Hashtbl.t =
      Hashtbl.create 16
    in
    (* 1. Solve each solve-group. A group is a list of target strings
       fed to the solver as a single root set, so variant-forming groups
       (e.g. [["ocaml-option-flambda"; "ocaml-option-static"; "ocaml"]])
       solve to a combined plan and cache as a unit. Singleton groups
       are indistinguishable from the previous per-target solves. *)
    let solutions =
      let n_groups = List.length target_groups in
      let group_label group = String.concat " " group in
      let solve_group group =
        let items = List.map parse_pkg_target group in
        let names = List.map fst items in
        let constraints =
          List.fold_left
            (fun acc (name, c) ->
              match c with
              | None -> acc
              | Some c -> OpamPackage.Name.Map.add name c acc)
            base_constraints items
        in
        match
          Oi.Solve.solve ctx ~packages_dirs ~constraints
            (names @ extra_names)
        with
        | Ok pkgs ->
            Log.info (fun m ->
                m "Solved %s: %d packages" (group_label group)
                  (List.length pkgs));
            Some (group, pkgs)
        | Error msg ->
            List.iter (fun t -> Hashtbl.replace solve_failures t msg) group;
            Log.debug (fun m ->
                m "solve failed: %s: %s" (group_label group) msg);
            None
      in
      if n_groups <= 1 then List.filter_map solve_group target_groups
      else
        let config = Progress.Config.v ~persistent:false () in
        let bar =
          let open Progress.Line in
          pair ~sep:(const " ")
            (list [ spinner (); brackets (count_to n_groups) ])
            (rpad 40 string)
        in
        let acc = ref [] in
        Progress.with_reporter ~config bar (fun report ->
            List.iter
              (fun group ->
                let label = group_label group in
                report (0, Fmt.str "solve %s" label);
                (match solve_group group with
                | Some s -> acc := s :: !acc
                | None -> ());
                report (1, Fmt.str "solve %s" label))
              target_groups);
        List.rev !acc
    in
    if solutions = [] then Oi.Error.msg "no packages solved successfully";
    (* 2. Each solve group becomes its own build group. *)
    let groups = List.map (fun sol -> ([ sol ], Hashtbl.create 0)) solutions in
    let n_groups = List.length groups in
    Log.info (fun m ->
        m "%d solve group(s) → %d build group(s)" (List.length solutions)
          n_groups);
    (* 3. Build each group *)
    List.iteri
      (fun gi (group_solutions, _) ->
        let group_targets_list = List.concat_map fst group_solutions in
        let group_targets = String.concat ", " group_targets_list in
        List.iter
          (fun t -> Hashtbl.replace target_group t gi)
          group_targets_list;
        let seen = Hashtbl.create 256 in
        let merged_pkgs =
          List.concat_map
            (fun (_, pkgs) ->
              List.filter
                (fun pkg ->
                  let key = OpamPackage.to_string pkg in
                  if Hashtbl.mem seen key then false
                  else begin
                    Hashtbl.replace seen key true;
                    true
                  end)
                pkgs)
            group_solutions
        in
        let group_ctx =
          Oi.Opam_ctx.create ~prefix:build_prefix ~packages_dirs ~conf
        in
        let sorted_pkgs =
          Oi.Solve.topo_sort ~packages_dirs group_ctx merged_pkgs
        in
        if n_groups > 1 then
          Log.info (fun m ->
              m "Group %d/%d [%s]: %d packages" (gi + 1) n_groups group_targets
                (List.length sorted_pkgs))
        else
          Log.info (fun m -> m "%d unique packages" (List.length sorted_pkgs));
        let build_plan =
          Oi.Action.plan group_ctx ~d10 ~packages_dirs sorted_pkgs
        in
        let count_by f =
          List.length (List.filter f (Oi.Action.nodes build_plan))
        in
        let n_build =
          count_by (fun (n : Oi.Action.node) -> n.method_ = Source)
        in
        let n_cached =
          count_by (fun (n : Oi.Action.node) -> n.method_ = Binary)
        in
        let n_pkgs = n_build + n_cached in
        if dry_run then begin
          let remote_has =
            match remote with
            | Some r ->
                let idx = D10.Layer.fetch_remote_index d10 ~remote:r in
                fun h -> Hashtbl.mem idx h
            | None -> fun _ -> false
          in
          Fmt.pr "%a@." (Oi.Action.pp_tree ~remote_has) build_plan
        end
        else begin
          Log.info (fun m -> m "%d to build, %d cached" n_build n_cached);
          let exec_plan =
            Oi.Plan.create group_ctx ~packages_dirs ~cache_root ~os_key
              ~ocaml_version:conf.ocaml_version build_plan
          in
          let build_outcome : [ `Ok | `Fail of string ] =
            if n_build = 0 then `Ok
            else begin
              let build_plan =
                fetch_remote_layers ?jobs ~remote ~d10 ~packages_dirs
                  ~ctx:group_ctx ~pkgs:sorted_pkgs build_plan
              in
              try
                let exec_plan =
                  Oi.Plan.create group_ctx ~packages_dirs ~cache_root ~os_key
                    ~ocaml_version:conf.ocaml_version build_plan
                in
                let cache_urls = cache_urls_of ~cache ~remote in
                Oi.Execute.run ~cache_urls ?jobs ~proc_mgr ~fs
                  ~clock:(clock :> D10.Config.clk)
                  ~sys ~os_key exec_plan;
                `Ok
              with
              | Oi.Error.E e -> `Fail (Fmt.str "%a" Oi.Error.pp e)
              | Failure msg -> `Fail msg
            end
          in
          Hashtbl.replace group_results gi
            (match build_outcome with
            | `Ok -> `Ok (n_pkgs, n_build, n_cached)
            | `Fail msg -> `Fail (n_pkgs, n_build, n_cached, msg));
          (* Write-side mirror: run unconditionally so binary-cached
             layers whose tarballs still live in opam's download-cache
             from an earlier run also get promoted. [record] is a
             silent no-op for anything not in the cache. *)
          record_sources_to_mirror ~sys ~cache exec_plan
        end)
      groups;
    if not dry_run then
      print_build_summary ~targets ~target_handle ~solve_failures ~target_group
        ~group_results
  in
  let targets =
    Arg.(
      value & pos_all string []
      & info ~docv:"PKG" ~doc:"Opam packages to build layers for" [])
  in
  let all =
    Arg.(
      value & flag
      & info
          ~doc:
            "Walk every overlay in the reporepo and derive targets: if an \
             overlay declares $(b,x-root-packages), build each as \
             $(b,@HANDLE/PKG); otherwise build every package the overlay \
             ships (the $(b,@HANDLE) shortcut). The $(b,default) overlay \
             (ocaml/opam-repository) is excluded because building its ~10k \
             packages is never what you want. Combine with $(b,--only) / \
             $(b,--skip) to refine the handle set (pass $(b,--only default) \
             if you really want to build everything in opam-repository). \
             Pair with $(b,--dry-run) to preview the target list without \
             solving. Positional $(b,PKG) arguments are still honoured and \
             are built in addition to the reporepo-derived list."
          [ "all" ])
  in
  let only =
    Arg.(
      value & opt_all string []
      & info ~docv:"HANDLE"
          ~doc:
            "Restrict $(b,--all) to these handles. Repeat for multiple. \
             Without $(b,--all) this flag has no effect."
          [ "only" ])
  in
  let skip =
    Arg.(
      value & opt_all string []
      & info ~docv:"HANDLE"
          ~doc:
            "Exclude these handles from $(b,--all). Repeat for multiple. \
             Without $(b,--all) this flag has no effect."
          [ "skip" ])
  in
  let dry_run =
    Arg.(
      value & flag
      & info ~doc:"Show the merged build plan without building"
          [ "n"; "dry-run" ])
  in
  let info =
    Cmd.info "build"
      ~doc:"Build packages into the local cache for later publication"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Solve and build every $(b,PKG) into the local cache and source \
             mirror. Pass many packages at once: $(b,oi) groups compatible \
             solutions so one invocation is cheaper than a loop.";
          `P "Intended for priming a cache before $(b,oi registry export).";
          `S "PKG FORMS";
          `I ("$(b,PKG)", "Opam package name.");
          `I
            ( "$(b,@HANDLE/PKG)",
              "Build $(b,PKG) pinned to the version $(b,HANDLE)'s overlay \
               ships." );
          `I ("$(b,@HANDLE)", "Build every package in $(b,HANDLE)'s overlay.");
          `S "OPTIONS";
          `P
            "$(b,--all) enumerates every overlay in the reporepo and builds \
             its $(b,x-root-packages) list as $(b,@HANDLE/PKG). Restrict with \
             $(b,--only=HANDLE) or exclude with $(b,--skip=HANDLE). This is \
             the standard way to prime a registry: the reporepo is the single \
             source of truth for which packages to pre-build.";
          `P
            "$(b,--with) adds extra packages or URL-supplied projects as build \
             targets.";
          `P "$(b,-j N) caps parallel builds and fetches. Default 4.";
          `P "$(b,-n) / $(b,--dry-run) prints the plan without building.";
          `S Manpage.s_examples;
          `Pre
            "  # Build the reporepo's declared root packages for every \
             overlay\n\
            \  oi registry build --all\n\n\
            \  # Same, but only for the 'avsm' overlay\n\
            \  oi registry build --all --only=avsm\n\n\
            \  # Build a one-off package alongside the full reporepo set\n\
            \  oi registry build --all ocaml-lsp-server";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ dry_run $ all $ only $ skip $ registry_term $ with_repos_term
      $ with_deps_term $ jobs_term $ targets)

(* -- depexts ------------------------------------------------------------- *)

let depexts_cmd =
  let run () data_dir cache_dir refresh with_repos with_deps by_package
      os_override =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, _clock, sys, platform, _os_key, cache =
      bootstrap env cache_dir
    in
    let cwd_s, _ = resolved_cwd fs in
    init_opam_root ~fs ~data_dir;
    ignore (get_packages_dirs ~fs ~sys ~data_dir ~refresh ());
    let proj = Oi.Project.load ~fs cwd_s in
    let extra_cli, url_project =
      materialize_with_deps ~fs ~sys ~cache ~refresh with_deps
    in
    if proj.deps = [] && extra_cli = [] && url_project.roots = [] then
      Oi.Error.config_error
        "No dependencies declared in %s (need at least one *.opam with \
         non-local depends, or use --with)"
        cwd_s;
    let with_repos = proj.overlays @ url_project.overlays @ with_repos in
    let pin_dir =
      Oi.Pin.materialize ~fs ~sys ~cache ~refresh (proj.pins @ url_project.pins)
    in
    let all_extras =
      merge_extras
        ~cli:(cli_extra_repos ~fs ~sys with_repos)
        ~project:(proj.extra_repos @ url_project.extra_repos)
    in
    let extras = Oi.Repo.ensure_extra ~fs ~data_dir ~refresh all_extras in
    let packages_dirs =
      Stdlib.Option.to_list pin_dir
      @ extras
      @ get_packages_dirs ~fs ~sys ~data_dir ()
    in
    let conf =
      let c = make_conf ~platform in
      match os_override with None -> c | Some os -> { c with os }
    in
    let build_prefix = Oi.Cache.root_s cache / "build" / "prefix" in
    let ctx = Oi.Opam_ctx.create ~prefix:build_prefix ~packages_dirs ~conf in
    let extra_constraints = Oi.Script.constraints extra_cli in
    let extra_names =
      List.filter_map
        (fun (d : Oi.Script.dep) ->
          if OpamPackage.Name.to_string d.name = "ocaml" then None
          else Some d.name)
        extra_cli
      @ List.map OpamPackage.Name.of_string url_project.roots
    in
    let names = List.map OpamPackage.Name.of_string proj.deps @ extra_names in
    let solved =
      match
        Oi.Solve.solve ctx ~packages_dirs ~constraints:extra_constraints names
      with
      | Ok pkgs -> pkgs
      | Error msg -> Oi.Error.no_solution msg
    in
    let entries = Oi.Depexts.compute ctx ~packages_dirs solved in
    if by_package then
      List.iter
        (fun { Oi.Depexts.pkg; sys_pkgs } ->
          OpamSysPkg.Set.iter
            (fun s ->
              Fmt.pr "%s\t%s@."
                (OpamPackage.to_string pkg)
                (OpamSysPkg.to_string s))
            sys_pkgs)
        entries
    else begin
      let all =
        List.fold_left
          (fun acc e -> OpamSysPkg.Set.union acc e.Oi.Depexts.sys_pkgs)
          OpamSysPkg.Set.empty entries
      in
      OpamSysPkg.Set.iter (fun s -> Fmt.pr "%s@." (OpamSysPkg.to_string s)) all
    end
  in
  let by_package =
    Arg.(value & flag & info ~doc:"Group depexts by package" [ "by-package" ])
  in
  let os_override =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"OS"
          ~doc:"Override the platform 'os' variable (e.g. linux, macos)"
          [ "os" ])
  in
  let info =
    Cmd.info "depexts"
      ~doc:"List system packages required by the dependency closure"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Solve the dep tree and print every system package (C library, \
             build tool) it needs. Tagged for the current distro.";
          `Pre "  sudo apt install \\$(oi depexts)";
          `S "OPTIONS";
          `P
            "$(b,--by-package) groups output by the opam package that needs \
             each system dependency.";
          `P "$(b,--os=NAME) prints what would be needed on another distro.";
          `P
            "$(b,--with) and $(b,--with-repo) extend the solve. Same forms as \
             $(b,oi run).";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ with_repos_term $ with_deps_term $ by_package $ os_override)

(* -- registry docker ---------------------------------------------------- *)

let registry_docker_cmd =
  let default_distros : Registry_docker.Distro.t list =
    [
      `Alpine `Latest;
      `Debian `Stable;
      `Ubuntu `V22_04;
      `Ubuntu `V24_04;
      `Ubuntu `V25_10;
      `Fedora `Latest;
    ]
  in
  let run () output_dir src_context =
    with_error_handling @@ fun () ->
    (try Unix.mkdir output_dir 0o755 with Unix.Unix_error (EEXIST, _, _) -> ());
    let df_oi = Registry_docker.dockerfile_oi ~src_context in
    let oi_path = output_dir / "Dockerfile.oi" in
    Registry_docker.write_dockerfile oi_path df_oi;
    let per_distro_paths =
      List.map
        (fun d ->
          let fname = Registry_docker.one_distro_filename d in
          let path = output_dir / fname in
          let df = Registry_docker.dockerfile_one_distro d in
          Registry_docker.write_dockerfile path df;
          (d, path))
        default_distros
    in
    let compose_path = output_dir / "docker-compose.yml" in
    let compose_yaml =
      Registry_docker.docker_compose_yaml ~distros:default_distros
        ~registry_host_path:"./registry" ()
    in
    Registry_docker.write_file compose_path compose_yaml;
    Fmt.pr "Wrote:@.";
    Fmt.pr "  %s@." oi_path;
    List.iter (fun (_, path) -> Fmt.pr "  %s@." path) per_distro_paths;
    Fmt.pr "  %s@." compose_path;
    Fmt.pr "@.";
    Fmt.pr "Static oi release binary:@.";
    Fmt.pr "  docker buildx build -f %s --output type=local,dest=./oi-bin .@."
      oi_path;
    Fmt.pr "Run the registry build + export:@.";
    Fmt.pr "  docker compose up --build   # writes ./registry/<os_key>/@."
  in
  let output_dir =
    Arg.(
      value & opt string "."
      & info ~docv:"DIR"
          ~doc:
            "Directory to write the Dockerfiles and docker-compose.yml \
             (created if missing)."
          [ "o"; "output" ])
  in
  let src_context =
    Arg.(
      value & opt string "."
      & info ~docv:"PATH"
          ~doc:
            "Path to the oi source tree, relative to the Docker build context. \
             Defaults to the context root."
          [ "src" ])
  in
  let info =
    Cmd.info "docker"
      ~doc:
        "Generate per-distro Dockerfiles and a docker-compose.yml that run oi \
         registry build + export"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Writes $(b,Dockerfile.oi) (standalone static musl build of \
             $(b,oi)), one $(b,Dockerfile.<distro>) per distro (alpine latest, \
             debian stable, ubuntu 22.04/24.04/25.10, fedora latest), and a \
             $(b,docker-compose.yml) that bind-mounts $(b,./registry) onto \
             $(b,/out) in every service.";
          `P
            "The per-distro images are intentionally generic: they install \
             depexts and copy in a static $(b,oi) binary. The compose file \
             drives the actual work via a $(b,command:) override. Each \
             container owns its own oi state — on first use it clones the \
             reporepo from $(b,oi)'s built-in default URL (overridable by \
             setting $(b,OI_REPOREPO_URL) in the service environment), \
             iterates every overlay's $(b,x-root-packages) list and builds \
             each as $(b,@HANDLE/PKG), then finishes with $(b,oi registry \
             export /out). Containers are independent and safe to run in \
             parallel. The resulting tree at $(b,./registry/<os_key>/) \
             carries one archive per layer plus a sqlite $(b,index.db) \
             tagged with each layer's overlay handle/version, so clients \
             can scope to a specific overlay by querying the index. Run \
             the whole project with:";
          `Pre "  docker compose up --build";
          `P
            "$(b,compose up) returns once every distro has finished. The \
             resulting tree is ready to serve over static HTTP or to \
             $(b,rsync) onto a registry server — no further $(b,oi) commands \
             needed.";
          `S Manpage.s_examples;
          `P "Generate the compose project in the current directory:";
          `Pre "  oi registry docker -o ./registry-build";
        ]
  in
  Cmd.v info
    Term.(const run $ log_term $ output_dir $ src_context)

(* -- registry mirror ------------------------------------------------------ *)

(* Human-readable byte size ("1.2GB", "47MB", …). Defined here rather
   than reusing Cache.pp_size because we want to print directly into a
   string for simple output, not via an Fmt formatter. *)
let human_bytes b =
  if Int64.compare b 1_000_000_000L > 0 then
    Fmt.str "%.1fGB" (Int64.to_float b /. 1e9)
  else if Int64.compare b 1_000_000L > 0 then
    Fmt.str "%.1fMB" (Int64.to_float b /. 1e6)
  else if Int64.compare b 1_000L > 0 then
    Fmt.str "%.1fKB" (Int64.to_float b /. 1e3)
  else Fmt.str "%LdB" b

let registry_mirror_stats_cmd =
  let run () cache_dir =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, _fs, _clock, _sys, _platform, _os_key, cache =
      bootstrap env cache_dir
    in
    let s = Oi.Source_mirror.stats ~cache in
    Fmt.pr "Mirror: %s@." (Oi.Source_mirror.dir ~cache);
    Fmt.pr "  blobs:      %d@." s.count;
    Fmt.pr "  total size: %s@." (human_bytes s.total_size)
  in
  let info =
    Cmd.info "stats"
      ~doc:"Show how many source tarballs are mirrored and their total size"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "One-line summary: how many distinct source tarballs are currently \
             mirrored and how much disk they take. Reach for this before an \
             $(b,oi registry export) to gauge how much will ship, or just to \
             track mirror growth over time.";
        ]
  in
  Cmd.v info Term.(const run $ log_term $ cache_dir_term)

let registry_mirror_gc_cmd =
  let run () cache_dir =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, _fs, _clock, _sys, _platform, _os_key, cache =
      bootstrap env cache_dir
    in
    let n = Oi.Source_mirror.gc ~cache in
    Fmt.pr "Removed %d orphaned blob(s)@." n
  in
  let info =
    Cmd.info "gc"
      ~doc:"Delete mirrored tarballs that no package still references"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Removes source tarballs from the mirror when no package in the \
             index still references them. This happens after you've built a \
             newer version of a package but kept the mirror around: the old \
             tarball lingers on disk even though nothing points at it any \
             more. Safe to run at any time; the cache will refetch if a later \
             rebuild needs an older source.";
        ]
  in
  Cmd.v info Term.(const run $ log_term $ cache_dir_term)

let registry_mirror_verify_cmd =
  let run () cache_dir =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, _fs, _clock, sys, _platform, _os_key, cache =
      bootstrap env cache_dir
    in
    match Oi.Source_mirror.verify ~sys ~cache with
    | [] -> Fmt.pr "All blobs verified OK@."
    | errs ->
        List.iter
          (fun (sha, msg) ->
            Fmt.epr "%a %s: %s@." Fmt.(styled `Red string) "BAD" sha msg)
          errs;
        Fmt.epr "%d blob(s) failed verification@." (List.length errs);
        exit 1
  in
  let info =
    Cmd.info "verify"
      ~doc:"Detect corrupted tarballs in the mirror by re-hashing them"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Walks every tarball in the mirror, recomputes its checksum, and \
             complains loudly if the bytes on disk don't match what the index \
             says they should. Run this before a big $(b,oi registry export) \
             if the mirror has been sitting around for a while, or after \
             disk-level problems.";
        ]
  in
  Cmd.v info Term.(const run $ log_term $ cache_dir_term)

let registry_mirror_list_cmd =
  let run () cache_dir package =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, _fs, _clock, _sys, _platform, _os_key, cache =
      bootstrap env cache_dir
    in
    let entries = Oi.Source_mirror.list ~cache ?package () in
    (* One line per (source, package) reference. Columns:
         <pkg.version>  <kind>  <size>  <sha256 (first 12)>  <url>
       sha256 is shortened for readability; pipe the raw column to
       sqlite3 if you need full hashes. *)
    List.iter
      (fun (e : Oi.Source_mirror.entry) ->
        let pkg = e.package_name ^ "." ^ e.package_version in
        let kind =
          match e.kind with `Main -> "main" | `Extra n -> "extra:" ^ n
        in
        let short_sha =
          if String.length e.sha256 >= 12 then String.sub e.sha256 0 12
          else e.sha256
        in
        Fmt.pr "%-40s  %-16s  %-12s  %10s  %s@." pkg kind short_sha
          (Fmt.str "%a" Oi.Cache.pp_size e.size)
          e.url)
      entries;
    if entries = [] then
      match package with
      | Some p -> Fmt.pr "No sources in mirror for package %s@." p
      | None -> Fmt.pr "Mirror is empty@."
  in
  let package =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"PKG"
          ~doc:"Restrict the listing to sources referenced by this package"
          [ "p"; "package" ])
  in
  let info =
    Cmd.info "list"
      ~doc:
        "Show every source tarball in the mirror, one row per package that \
         uses it"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Prints one row for each tarball reference in the mirror, with the \
             package and version that pulled it in, the kind of source (main \
             or an extra patch), a short hash of the tarball, its on-disk \
             size, and the upstream URL it came from. The same tarball can \
             show up twice if two packages share a source, in which case both \
             rows point at the same short hash.";
          `P
            "$(b,-p NAME) restricts the listing to a single package, handy \
             when tracking down which sources a specific package contributed \
             to the mirror.";
        ]
  in
  Cmd.v info Term.(const run $ log_term $ cache_dir_term $ package)

let registry_mirror_cmd =
  let info =
    Cmd.info "mirror" ~doc:"Manage the local copy of upstream source tarballs"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Whenever $(b,oi) builds a package, it keeps a copy of the source \
             tarball it fetched from upstream. Taken together, those copies \
             form a mirror of the opam ecosystem's sources for the packages \
             you actually use. The mirror is shipped alongside the binary \
             cache when you $(b,oi registry export), so downstream clients and \
             offline rebuilds don't have to hit the upstream servers.";
          `P
            "These subcommands let you inspect and maintain that mirror: show \
             totals, list individual entries, verify the tarballs still hash \
             correctly, and garbage-collect blobs that are no longer \
             referenced.";
        ]
  in
  Cmd.group info
    [
      registry_mirror_stats_cmd;
      registry_mirror_list_cmd;
      registry_mirror_gc_cmd;
      registry_mirror_verify_cmd;
    ]

let registry_cmd =
  let info =
    Cmd.info "registry"
      ~doc:"Manage the cache of pre-built packages and the remote registry"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi) keeps a local cache of pre-built OCaml packages so \
             repeated work is avoided, and it can pull those pre-built \
             packages from a remote registry rather than building them itself. \
             This group of commands inspects and manages both.";
          `P
            "Most users only need $(b,oi registry show) to peek at the cache, \
             and $(b,oi registry build) / $(b,oi registry export) if they're \
             running their own registry. The $(b,mirror) subgroup handles the \
             companion mirror of upstream source tarballs.";
        ]
  in
  Cmd.group info
    [
      registry_show_cmd;
      registry_index_cmd;
      registry_export_cmd;
      registry_build_cmd;
      registry_docker_cmd;
      registry_mirror_cmd;
    ]

(* -- repo (reporepo of overlay pins) ------------------------------------ *)

let reporepo_term =
  let default =
    match Sys.getenv_opt "OI_REPOREPO" with
    | Some v when v <> "" -> v
    | _ -> Oi.Reporepo.default_path
  in
  Arg.(
    value & opt string default
    & info ~docv:"DIR"
        ~doc:
          "Local directory containing the reporepo clone to operate on. Falls \
           back to $(b,\\$OI_REPOREPO), then to $(b,\\$OI_DATA_DIR/reporepo) \
           under the XDG data hierarchy."
        [ "reporepo" ])

let reporepo_url_term =
  let default =
    match Sys.getenv_opt "OI_REPOREPO_URL" with
    | Some v when v <> "" -> v
    | _ -> Oi.Reporepo.default_url
  in
  Arg.(
    value & opt string default
    & info ~docv:"URL"
        ~doc:
          "Git URL to clone the reporepo from when the local clone doesn't \
           exist yet. Falls back to $(b,\\$OI_REPOREPO_URL) and then to the \
           built-in upstream. Once the local clone exists, $(b,oi) never pulls \
           from this URL again — the working copy is yours to edit, commit, \
           and push."
        [ "reporepo-url" ])

let depend_term =
  Arg.(
    value & opt_all string []
    & info ~docv:"HANDLE[=VERSION]"
        ~doc:
          "Make this overlay depend on another one. Use $(b,HANDLE=VERSION) to \
           pin a specific recorded version, or just $(b,HANDLE) to accept any. \
           Repeatable. When omitted on a non-base overlay, $(b,oi) auto-fills \
           $(b,default)/$(b,relocatable) at their current latest versions."
        [ "depend"; "d" ])

let parse_depend_spec s =
  match String.index_opt s '=' with
  | None -> (s, None)
  | Some i ->
      let h = String.sub s 0 i in
      let v = String.sub s (i + 1) (String.length s - i - 1) in
      (h, Some v)

let parse_handle_version s =
  match String.index_opt s '=' with
  | None -> (s, None)
  | Some i ->
      (String.sub s 0 i, Some (String.sub s (i + 1) (String.length s - i - 1)))

let print_entry_oneline (e : Oi.Reporepo.entry) =
  let short = String.sub e.commit 0 (min 7 (String.length e.commit)) in
  Fmt.pr "%-24s  %-16s  %-8s  %s@." e.handle e.version short e.url

(* Upstream tip status for a reporepo entry, computed by re-running
   [git ls-remote] against its URL + ref. *)
type upstream_status =
  | Fresh  (** Pinned commit matches the upstream tip. *)
  | Stale of string  (** Upstream tip differs; carries its 40-char sha. *)
  | Unknown  (** [git ls-remote] failed (offline, auth, moved URL…). *)

let short_sha s = String.sub s 0 (min 7 (String.length s))

let check_upstream ~sys (e : Oi.Reporepo.entry) =
  match Oi.Reporepo.ls_remote_sha ~sys ?ref_:e.ref_ e.url with
  | tip when tip = e.commit -> Fresh
  | tip -> Stale tip
  | exception _ -> Unknown

let print_entry_with_upstream (e : Oi.Reporepo.entry) status =
  let tag =
    match status with
    | Fresh -> Fmt.str "%a" Fmt.(styled `Green string) "up-to-date"
    | Unknown -> Fmt.str "%a" Fmt.(styled `Yellow string) "unreachable"
    | Stale tip ->
        Fmt.str "%a (%s)" Fmt.(styled `Red string) "stale" (short_sha tip)
  in
  Fmt.pr "%-24s  %-16s  %-8s  %-28s  %s@." e.handle e.version
    (short_sha e.commit) tag e.url

let repo_list_cmd =
  let run () reporepo reporepo_url no_check =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr = Eio.Stdenv.process_mgr env in
    let fs = Eio.Stdenv.fs env in
    let sys =
      D10.Sysops.create ~stdout:(Eio.Stdenv.stdout env)
        ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs ()
    in
    Oi.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
      ~url:reporepo_url;
    match Oi.Reporepo.load ~path:reporepo with
    | [] -> Fmt.pr "Reporepo %s is empty.@." reporepo
    | entries ->
        Fmt.pr "Reporepo: %s@.@." reporepo;
        let latest_entries =
          entries
          |> List.map (fun (e : Oi.Reporepo.entry) -> e.handle)
          |> List.sort_uniq String.compare
          |> List.filter_map (fun handle -> Oi.Reporepo.latest entries ~handle)
        in
        if no_check then List.iter print_entry_oneline latest_entries
        else begin
          (* Parallel [git ls-remote] per entry. Four at a time keeps
             the pipe/fd footprint small without making a 30-entry
             reporepo serial. Failures downgrade to [Unknown] — a
             flaky network must not make [oi repo list] unusable. *)
          let indexed = List.mapi (fun i e -> (i, e)) latest_entries in
          let statuses = Array.make (List.length indexed) Unknown in
          Eio.Fiber.List.iter ~max_fibers:4
            (fun (i, e) -> statuses.(i) <- check_upstream ~sys e)
            indexed;
          List.iteri
            (fun i e -> print_entry_with_upstream e statuses.(i))
            latest_entries
        end
  in
  let no_check =
    Arg.(
      value & flag
      & info ~doc:"Skip the per-entry [git ls-remote] tip check." [ "no-check" ])
  in
  let info =
    Cmd.info "list" ~doc:"List overlays registered in the reporepo"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "One line per handle: pinned commit, upstream status, URL. Status \
             comes from $(b,git ls-remote) (up to four in parallel):";
          `I ("$(b,up-to-date)", "Pinned commit matches upstream.");
          `I ("$(b,stale)", "Upstream has newer commits.");
          `I ("$(b,unreachable)", "Remote refused or offline.");
          `P
            "Fix a stale row with $(b,oi repo bump HANDLE). Pass \
             $(b,--no-check) to skip the network call.";
        ]
  in
  Cmd.v info
    Term.(const run $ log_term $ reporepo_term $ reporepo_url_term $ no_check)

let repo_show_cmd =
  let run () reporepo reporepo_url handle =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr = Eio.Stdenv.process_mgr env in
    let fs = Eio.Stdenv.fs env in
    let sys =
      D10.Sysops.create ~stdout:(Eio.Stdenv.stdout env)
        ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs ()
    in
    Oi.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
      ~url:reporepo_url;
    let entries = Oi.Reporepo.load ~path:reporepo in
    let matches =
      List.filter (fun (e : Oi.Reporepo.entry) -> e.handle = handle) entries
      |> List.sort (fun (a : Oi.Reporepo.entry) (b : Oi.Reporepo.entry) ->
          OpamPackage.Version.compare
            (OpamPackage.Version.of_string b.version)
            (OpamPackage.Version.of_string a.version))
    in
    if matches = [] then
      Oi.Error.not_found handle "no overlay %s in reporepo %s" handle reporepo;
    List.iter
      (fun (e : Oi.Reporepo.entry) ->
        Fmt.pr "%s.%s@." e.handle e.version;
        Fmt.pr "  url:    %s@." e.url;
        Fmt.pr "  commit: %s@." e.commit;
        (match e.ref_ with Some r -> Fmt.pr "  ref:    %s@." r | None -> ());
        (match e.depends with
        | [] -> ()
        | ds ->
            Fmt.pr "  depends:@.";
            List.iter
              (fun (h, v) ->
                match v with
                | None -> Fmt.pr "    %s@." h
                | Some ver -> Fmt.pr "    %s = %s@." h ver)
              ds);
        (match e.root_packages with
        | [] -> ()
        | groups ->
            Fmt.pr "  root-packages:@.";
            List.iter
              (fun group ->
                match group with
                | [] -> ()
                | [ p ] -> Fmt.pr "    %s@." p
                | multi ->
                    Fmt.pr "    [%s]@." (String.concat " " multi))
              groups);
        Fmt.pr "@.")
      matches
  in
  let handle =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"HANDLE" ~doc:"Overlay handle to inspect" [])
  in
  let info =
    Cmd.info "show"
      ~doc:"Show every version of one overlay, with commits and dependencies"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Prints the full history for $(b,HANDLE): every version of the \
             overlay that's been recorded, each with the git URL and commit it \
             pins, the branch (if one is tracked with $(b,--ref)), and any \
             other overlays it depends on.";
          `P
            "Useful for auditing what a particular user's overlay pulls in, \
             and for telling at a glance whether bumping it would drag other \
             overlays along.";
        ]
  in
  Cmd.v info
    Term.(const run $ log_term $ reporepo_term $ reporepo_url_term $ handle)

let ref_term =
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"REF"
        ~doc:
          "Track a specific branch or tag instead of the repository's default \
           branch. The branch name is remembered, so later $(b,oi repo bump) \
           invocations keep following the same branch rather than silently \
           falling back to the default. Example: $(b,--ref=relocatable) for \
           $(b,dra27/opam-repository), whose payload lives on the \
           $(b,relocatable) branch."
        [ "ref"; "r" ])

let repo_add_cmd =
  let run () reporepo reporepo_url handle url ref_ depend_specs =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr = Eio.Stdenv.process_mgr env in
    let fs = Eio.Stdenv.fs env in
    let sys =
      D10.Sysops.create ~stdout:(Eio.Stdenv.stdout env)
        ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs ()
    in
    Oi.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
      ~url:reporepo_url;
    let depends =
      match depend_specs with
      | [] -> None
      | _ -> Some (List.map parse_depend_spec depend_specs)
    in
    let e =
      Oi.Reporepo.add ~fs ~sys ~path:reporepo ~handle ~url ?ref_ ?depends ()
    in
    Fmt.pr "Added %s.%s@ url=%s@ commit=%s@ at %s@." e.handle e.version e.url
      e.commit e.opam_path;
    if e.depends <> [] then begin
      Fmt.pr "Depends:@.";
      List.iter
        (fun (h, v) ->
          match v with
          | Some ver -> Fmt.pr "  %s = %s@." h ver
          | None -> Fmt.pr "  %s@." h)
        e.depends
    end
  in
  let handle =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"HANDLE"
          ~doc:"Opam-valid overlay name (e.g. $(b,avsm), $(b,samoht))" [])
  in
  let url =
    Arg.(
      required
      & pos 1 (some string) None
      & info ~docv:"URL" ~doc:"Upstream opam-repository git URL" [])
  in
  let info =
    Cmd.info "add" ~doc:"Register a new overlay in the reporepo"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Creates a new overlay named $(b,HANDLE) in the reporepo, pointed \
             at $(b,URL) (a git repository containing someone's collection of \
             opam packages). $(b,oi) records the current commit on the default \
             branch, or the branch you pick with $(b,--ref), so the overlay \
             stays frozen at a known-good snapshot until you explicitly run \
             $(b,oi repo bump).";
          `P
            "When you register an overlay $(i,other than) the base pair \
             ($(b,default), $(b,relocatable)), $(b,oi) automatically records a \
             dependency on whatever versions of those two are currently in the \
             reporepo. That way the new overlay and the base it was built \
             against stay together: picking the overlay later also picks up \
             exactly the $(b,default) / $(b,relocatable) commits it was \
             registered with.";
          `P "Examples:";
          `Pre
            "  oi repo add default https://github.com/ocaml/opam-repository.git\n\
            \  oi repo add relocatable \
             https://github.com/dra27/opam-repository.git --ref relocatable \
             --depend default\n\
            \  oi repo add avsm \
             https://tangled.org/anil.recoil.org/aoah-opam-repo.git";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ reporepo_term $ reporepo_url_term $ handle $ url
      $ ref_term $ depend_term)

let repo_bump_cmd =
  let run () reporepo reporepo_url handle url ref_ depend_specs =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr = Eio.Stdenv.process_mgr env in
    let fs = Eio.Stdenv.fs env in
    let sys =
      D10.Sysops.create ~stdout:(Eio.Stdenv.stdout env)
        ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs ()
    in
    Oi.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
      ~url:reporepo_url;
    let depends =
      match depend_specs with
      | [] -> None
      | _ -> Some (List.map parse_depend_spec depend_specs)
    in
    match
      Oi.Reporepo.bump ~fs ~sys ~path:reporepo ~handle ?url ?ref_ ?depends ()
    with
    | `Bumped e ->
        Fmt.pr "Bumped %s to %s@ commit=%s@ at %s@." e.handle e.version e.commit
          e.opam_path
    | `Unchanged e ->
        Fmt.pr
          "No change: %s.%s already pins the current upstream commit (%s).@."
          e.handle e.version e.commit
  in
  let handle =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"HANDLE" ~doc:"Overlay handle to bump" [])
  in
  let url =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"URL"
          ~doc:
            "Override the upstream URL. Defaults to whatever the latest \
             version pinned."
          [ "url" ])
  in
  let info =
    Cmd.info "bump" ~doc:"Bring an overlay up to the latest upstream commit"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Fetches the current commit on the overlay's tracked branch and \
             records it as a new entry in the reporepo. Use this when you want \
             $(b,oi) to start seeing new packages or fixes that have landed \
             upstream since the last time you bumped.";
          `P
            "Safe to run at any time: if the upstream commit, branch, URL, and \
             dependencies still match the previous entry, $(b,oi) prints \
             $(b,No change) and leaves the reporepo alone. That makes bump \
             double as the \"am I behind upstream?\" check, so you can run it \
             from cron or a git pre-commit hook without worrying about \
             spurious no-op churn.";
          `P
            "When there $(i,is) a new commit, $(b,oi) writes a new \
             $(b,YYYYMMDD.N) entry and keeps the old one, giving the reporepo \
             a git-like timeline of which commits you've pinned over time. If \
             something breaks after a bump, point $(b,oi) back at an earlier \
             version without consulting the upstream repo's history.";
          `P
            "When $(b,oi) bumps an overlay $(i,other than) $(b,default) or \
             $(b,relocatable), it also refreshes that overlay's dependency on \
             the bases to whatever their current latest versions are. Bumping \
             therefore re-locks the overlay against the newest base set, which \
             is normally what you want. Pass $(b,--depend) entries explicitly \
             to override.";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ reporepo_term $ reporepo_url_term $ handle $ url
      $ ref_term $ depend_term)

let repo_set_roots_cmd =
  (* Parse a PKG token: a comma-separated list becomes a multi-package
     solve group; a bare name becomes a singleton group. Empty tokens
     between commas are dropped. *)
  let parse_group token =
    String.split_on_char ',' token
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  let run () reporepo reporepo_url handle pkgs =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr = Eio.Stdenv.process_mgr env in
    let fs = Eio.Stdenv.fs env in
    let sys =
      D10.Sysops.create ~stdout:(Eio.Stdenv.stdout env)
        ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs ()
    in
    Oi.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
      ~url:reporepo_url;
    let groups =
      List.filter_map
        (fun t ->
          match parse_group t with [] -> None | g -> Some g)
        pkgs
    in
    match
      Oi.Reporepo.bump ~fs ~sys ~path:reporepo ~handle
        ~root_packages:groups ()
    with
    | `Bumped e ->
        Fmt.pr "Bumped %s to %s (root-packages: %d entr%s)@." e.handle e.version
          (List.length e.root_packages)
          (if List.length e.root_packages = 1 then "y" else "ies")
    | `Unchanged e ->
        Fmt.pr "No change: %s.%s already has that root-packages list.@."
          e.handle e.version
  in
  let handle =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"HANDLE" ~doc:"Overlay to update" [])
  in
  let pkgs =
    Arg.(
      value
      & pos_right 0 string []
      & info ~docv:"PKG"
          ~doc:
            "Package specs to record as the overlay's root packages (the \
             list that $(b,oi registry build --all) iterates over). Each \
             argument is one solve group: a bare name becomes a \
             single-package solve, while a comma-separated list becomes \
             a multi-package group (useful for compiler variants — e.g. \
             $(b,ocaml-option-flambda,ocaml-option-static,ocaml) forces \
             the solver to pick an $(b,ocaml) version compatible with \
             both options). Pass no $(b,PKG) arguments to clear the list."
          [])
  in
  let info =
    Cmd.info "set-roots"
      ~doc:"Record which packages should be pre-built for an overlay"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Writes $(b,x-root-packages: [...]) on a new bumped version of \
             $(b,HANDLE). The recorded list drives $(b,oi registry build \
             --all), which iterates every overlay in the reporepo and \
             builds each handle's root groups. Single-package groups \
             solve and build as one $(b,@handle/pkg); multi-package \
             groups (comma-separated on the CLI) solve together so the \
             resulting layers capture a specific variant.";
          `P
            "Passing zero $(b,PKG) arguments clears the list. The new version \
             is stamped $(b,YYYYMMDD.N) exactly like $(b,oi repo bump) — the \
             previous entry stays around as history.";
          `S Manpage.s_examples;
          `P "Record three independent root packages:";
          `Pre "  oi repo set-roots relocatable dune utop merlin";
          `P "Record a compiler variant alongside plain packages:";
          `Pre
            "  oi repo set-roots relocatable \
             ocaml-option-flambda,ocaml-option-static,ocaml dune utop";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ reporepo_term $ reporepo_url_term $ handle $ pkgs)

let repo_remove_cmd =
  let run () reporepo reporepo_url handle_spec =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr = Eio.Stdenv.process_mgr env in
    let fs = Eio.Stdenv.fs env in
    let sys =
      D10.Sysops.create ~stdout:(Eio.Stdenv.stdout env)
        ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs ()
    in
    Oi.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
      ~url:reporepo_url;
    let handle, version = parse_handle_version handle_spec in
    Oi.Reporepo.remove ~fs ~path:reporepo ~handle ?version ();
    Fmt.pr "Removed %s%s from %s@." handle
      (match version with None -> " (all versions)" | Some v -> "." ^ v)
      reporepo
  in
  let handle_spec =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"HANDLE[=VERSION]"
          ~doc:
            "Overlay to remove. Without $(b,=VERSION) every version of the \
             handle is deleted."
          [])
  in
  let info =
    Cmd.info "remove" ~doc:"Delete an overlay from the reporepo"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Removes an overlay entry from the reporepo. With \
             $(b,HANDLE=VERSION) only that specific version is deleted. With \
             just $(b,HANDLE) every recorded version of that handle is \
             removed.";
          `P
            "This only edits the reporepo; the upstream repositories \
             themselves are never touched. Any already-cloned overlay bundles \
             under the data directory stick around until you run $(b,oi \
             clean), so re-adding the handle doesn't force another full clone.";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ reporepo_term $ reporepo_url_term $ handle_spec)

let repo_push_cmd =
  let run () reporepo reporepo_url push_url =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr = Eio.Stdenv.process_mgr env in
    let fs = Eio.Stdenv.fs env in
    let sys =
      D10.Sysops.create ~stdout:(Eio.Stdenv.stdout env)
        ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs ()
    in
    Oi.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
      ~url:reporepo_url;
    Fmt.pr "%a %s@." Fmt.(styled `Bold string) "reporepo:" reporepo;
    (match push_url with
    | None -> ()
    | Some u ->
        Oi.Reporepo.set_push_url ~sys ~path:reporepo u;
        Fmt.pr "%a push URL of origin set to %s@."
          Fmt.(styled `Green string)
          "ok" u);
    let on_step_start n title =
      Fmt.pr "@.%a %s@." Fmt.(styled `Bold string) (Fmt.str "[%d/3]" n) title
    in
    let outcome = Oi.Reporepo.push ~on_step_start ~sys ~path:reporepo () in
    Fmt.pr "@.%a@." Fmt.(styled `Bold string) "summary:";
    List.iter
      (function
        | Oi.Reporepo.Step_commit { files = [] } ->
            Fmt.pr "  commit: %a (working tree clean)@."
              Fmt.(styled `Faint string)
              "skipped"
        | Oi.Reporepo.Step_commit { files } ->
            Fmt.pr "  commit: %a (%d file(s))@."
              Fmt.(styled `Green string)
              "ok" (List.length files);
            List.iter (fun f -> Fmt.pr "    %s@." f) files
        | Oi.Reporepo.Step_pull { commits = 0 } ->
            Fmt.pr "  pull:   %a (already up to date)@."
              Fmt.(styled `Faint string)
              "skipped"
        | Oi.Reporepo.Step_pull { commits } ->
            Fmt.pr "  pull:   %a (%d new upstream commit(s))@."
              Fmt.(styled `Green string)
              "ok" commits
        | Oi.Reporepo.Step_push { commits = 0 } ->
            Fmt.pr "  push:   %a (nothing to push)@."
              Fmt.(styled `Faint string)
              "skipped"
        | Oi.Reporepo.Step_push { commits } ->
            Fmt.pr "  push:   %a (%d local commit(s) sent)@."
              Fmt.(styled `Green string)
              "ok" commits)
      outcome
  in
  let push_url =
    Arg.(
      value
      & opt (some string) None
      & info [ "push-url" ] ~docv:"URL"
          ~doc:
            "Persistently set $(b,origin)'s push URL on the local reporepo \
             checkout (via $(b,git remote set-url --push origin URL)) before \
             pushing. Useful when the clone URL is read-only HTTPS but you \
             push over SSH. The fetch URL is left alone, so subsequent $(b,oi \
             repo) commands keep pulling from the original location.")
  in
  let info =
    Cmd.info "push"
      ~doc:"Pull, commit local edits, and push the reporepo to its remote"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Three-step sync of the reporepo working copy: first stages and \
             auto-commits any uncommitted changes (so edits from $(b,oi repo \
             bump) and friends are captured), then runs $(b,git pull --rebase) \
             to bring in upstream history, and finally $(b,git push)es the \
             local branch if it is now ahead of its upstream tracking branch. \
             Idempotent: runs against a clean, up-to-date reporepo are no-ops.";
          `P
            "Authentication uses the system $(b,git) configuration. Whatever \
             credentials work for $(b,git push) inside the reporepo directory \
             work here too — $(b,oi) shells out to $(b,git) and never touches \
             credentials itself.";
          `P
            "Pass $(b,--push-url URL) to flip the push remote on the local \
             checkout (handy when the clone URL is read-only HTTPS but you \
             have SSH push access). The flag is persistent — it edits \
             $(b,.git/config) once and subsequent $(b,oi repo push) runs reuse \
             it.";
          `S Manpage.s_examples;
          `P "Bump an overlay and publish the new pin in one shot:";
          `Pre "  oi repo bump avsm && oi repo push";
          `P "Switch the reporepo's push URL to SSH, then push:";
          `Pre
            "  oi repo push --push-url \
             git@tangled.org:anil.recoil.org/reporepo.git";
        ]
  in
  Cmd.v info
    Term.(const run $ log_term $ reporepo_term $ reporepo_url_term $ push_url)

let repo_cmd =
  let info =
    Cmd.info "repo"
      ~doc:"Manage a directory of package-source bundles you want to pull from"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "A $(i,reporepo) is a small directory that acts as a lock-file for \
             $(b,oi)'s base set of package sources. Each entry in it (called a \
             $(i,handle)) names somebody's collection of opam packages and \
             pins it to a specific git commit. When $(b,oi) builds anything, \
             it reads the reporepo to figure out exactly which commits to \
             fetch for the base OCaml repository, the relocatable compiler \
             fork, and any personal overlays you've opted in to.";
          `P
            "On the command line, handles can be used as shortcuts: $(b,oi run \
             @avsm/irmin) uses the version of $(b,irmin) from avsm's overlay; \
             $(b,oi run --with-repo=avsm ...) pulls the overlay in without \
             naming a specific package. Inside an opam file, $(b,x-reporepo: \
             [\"avsm\"]) has the same effect as $(b,--with-repo=avsm) on every \
             oi command run in the project.";
          `P
            "These subcommands let you inspect and edit the reporepo. The \
             first one you run on a new machine auto-clones the upstream \
             reporepo into a stable location under your data directory (see \
             $(b,FILES) below), so you don't need a manual bootstrap step. \
             After that, $(b,oi) never auto-pulls: the clone is yours to edit, \
             commit, and push like any other git working copy. A typical \
             workflow is $(b,oi repo bump HANDLE) whenever you want to pick up \
             upstream commits, then $(b,git push) or $(b,git request-pull) \
             from the reporepo directory to share those pins with other users.";
          `P
            "$(b,oi repo bump) is idempotent: when the upstream commit already \
             matches, it prints $(b,No change) and leaves the reporepo alone. \
             That makes it safe to run from cron or a pre-commit hook as an \
             \"am I behind upstream?\" check.";
          `S "FILES";
          `I
            ( "$(b,\\$OI_REPOREPO) (default: $(b,\\$OI_DATA_DIR/reporepo))",
              "The local git working copy of the reporepo. On first use of any \
               $(b,oi repo) subcommand, $(b,oi) runs $(b,git clone \
               \\$OI_REPOREPO_URL \\$OI_REPOREPO). $(b,cd) into it to make \
               edits by hand, add commits, and push them back upstream." );
          `S "EXAMPLE WORKFLOW";
          `Pre
            "  # First oi repo command on a new machine auto-clones\n\
            \  # the upstream reporepo into ~/.local/share/oi/reporepo/.\n\
            \  oi repo list\n\n\
            \  # Pin someone's overlay, compose it into the solver\n\
            \  oi repo add handle https://example.com/pkgs.git\n\
            \  oi run @handle/some-tool\n\n\
            \  # Pull upstream changes into the overlay we track\n\
            \  oi repo bump handle\n\n\
            \  # Publish our edits back to the reporepo upstream\n\
            \  cd ~/.local/share/oi/reporepo\n\
            \  git add -A && git commit -m 'bump handle' && git push";
        ]
  in
  Cmd.group info
    [
      repo_list_cmd;
      repo_show_cmd;
      repo_add_cmd;
      repo_bump_cmd;
      repo_set_roots_cmd;
      repo_remove_cmd;
      repo_push_cmd;
    ]

(* -- main ---------------------------------------------------------------- *)

let () =
  let info =
    Cmd.info "oi" ~version:"0.3.6"
      ~doc:"A fast, stateless OCaml package manager"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "$(b,oi) is a fast, stateless OCaml package manager. It reads \
             $(b,*.opam) files (the dependency manifests OCaml projects ship) \
             and opam repositories (the public package collections the OCaml \
             community maintains), resolves what's needed, and builds, \
             installs, or runs the result on demand. It caches every build so \
             repeated invocations reuse what's already there.";
          `S "QUICK START";
          `P
            "Run any tool from opam. First call builds, later calls hit the \
             cache.";
          `Pre "  oi run utop\n  oi run ocamlformat -- --help";
          `P
            "Pin a specific version with $(b,pkg.version), $(b,pkg=version), \
             or an opam relop:";
          `Pre
            "  oi run --with=dune.3.20.0 -- dune --version\n\
            \  oi run --with=fmt>=0.9 my_script.ml";
          `P
            "Run a package straight from a git repo. $(b,oi) clones the URL \
             and pins every $(b,*.opam) at its root:";
          `Pre
            "  oi run --with=https://github.com/owner/project.git target\n\
            \  oi run --with=git+https://example.org/foo.git#branch foo";
          `P "Run a $(b,.ml) script. Declare deps on the first line:";
          `Pre
            "  [@@@opam fmt cmdliner lwt>=5.0 ppx_deriving.show]\n\
            \  let () = ...\n\n\
            \  oi run my_script.ml\n\
            \  oi run https://example.com/hello.ml";
          `P
            "Inside a project, $(b,oi sync) installs deps into \
             $(b,_oi/prefix/) and writes $(b,.envrc). The sync also installs \
             dev tools ($(b,odoc), $(b,merlin), $(b,ocaml-lsp-server), plus \
             $(b,mdx) and $(b,ocamlformat) when the project uses them) into \
             $(b,_oi/tools/).";
          `Pre
            "  oi sync\n\
            \  direnv allow      # or: eval \"\\$(oi env)\"\n\
            \  oi exec dune build";
          `P "Add a dep. Edits $(b,dune-project) and re-syncs:";
          `Pre "  oi add logs\n  oi add \"fmt>=0.9\"";
          `P
            "An $(i,overlay) is someone's curated opam repository, pinned to a \
             git commit and referenced by a short $(i,handle). The \
             $(i,reporepo) is the directory of overlays $(b,oi) knows about. \
             See $(b,oi repo --help) to manage it. Prefix any target or \
             $(b,--with) value with $(i,@handle/) to pull from that overlay:";
          `Pre
            "  oi run @avsm/owntracks\n  oi run --with=@avsm/crockford roguedoi";
          `P "Find which package ships a binary:";
          `Pre "  oi which dune\n  oi which 'ocaml*'";
          `P "Preview without doing:";
          `Pre "  oi plan utop\n  oi run -n utop";
          `S "COMMAND CATEGORIES";
          `I
            ( "$(b,Getting started)",
              "$(b,run) executes a binary or $(b,.ml) script, fetching missing \
               deps and caching them." );
          `I
            ( "$(b,Working in a project)",
              "$(b,sync) installs project deps and dev tools. $(b,exec) runs \
               commands in the project environment. $(b,env) prints the same \
               environment for $(b,eval). $(b,add) adds a new dep to \
               $(b,dune-project). $(b,depexts) lists system packages needed by \
               the closure." );
          `I
            ( "$(b,Checking what's going on)",
              "$(b,plan) shows the build plan. $(b,which) finds which package \
               ships a binary. $(b,config) dumps platform, caches, project \
               state, and dev-tool probes." );
          `I
            ( "$(b,Sharing builds and managing disk)",
              "$(b,registry) manages the pre-built package cache and source \
               mirror. $(b,clean) frees disk space." );
          `I
            ( "$(b,Picking package sources)",
              "$(b,repo) manages the reporepo (see QUICK START): register \
               overlays, inspect their pinned commits, bump them forward." );
          `S "SCRIPT FORMAT";
          `P "First line of a $(b,.ml) script:";
          `Pre "  [@@@opam fmt cmdliner>=1.2.0 lwt]";
          `P
            "Each token is an opam package with an optional version constraint \
             ($(b,>=), $(b,>), $(b,<=), $(b,<), $(b,=)). A $(b,.sub) suffix \
             selects a findlib sub-library, e.g. $(b,ppx_deriving.show).";
          `P
            "Packages starting with $(b,ppx_) are wired in as PPX \
             preprocessors. Run $(b,oi run -vv SCRIPT.ml) to see the generated \
             build file.";
          `S Manpage.s_environment;
          `P
            "$(b,oi) uses two directories: a $(i,data) directory for \
             long-lived state (opam repositories, the relocatable compiler) \
             and a $(i,cache) directory for rebuildable data (pre-built \
             packages, assembled prefixes, the source mirror). Each can be \
             pointed elsewhere by setting one environment variable, or by \
             passing a command-line flag that takes precedence.";
          `I
            ( "$(b,OI_DATA_DIR)",
              "Override the data directory for $(b,oi) alone. Falls back to \
               $(b,XDG_DATA_HOME/oi), then $(b,~/.local/share/oi)." );
          `I
            ( "$(b,OI_CACHE_DIR)",
              "Override the cache directory for $(b,oi) alone. Falls back to \
               $(b,XDG_CACHE_HOME/oi), then $(b,~/.cache/oi)." );
          `I
            ( "$(b,OI_REPOREPO)",
              "Override the location of the reporepo clone. Defaults to \
               $(b,\\$OI_DATA_DIR/reporepo)." );
          `I
            ( "$(b,OI_REPOREPO_URL)",
              "Override the upstream URL used to clone the reporepo on first \
               use. Defaults to the built-in upstream. Has no effect once the \
               clone exists — $(b,oi) never auto-pulls." );
        ]
  in
  let cmd =
    Cmd.group info
      [
        run_cmd;
        add_cmd;
        exec_cmd;
        which_cmd;
        plan_cmd;
        sync_cmd;
        env_cmd;
        config_cmd;
        depexts_cmd;
        registry_cmd;
        repo_cmd;
        clean_cmd;
      ]
  in
  exit (Cmd.eval cmd)
