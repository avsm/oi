[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

open Cmdliner

let ( / ) = Filename.concat
let app_name = "oi"

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
          "Force refresh of all timestamp-driven caches (opam repos, extra \
           repos, pin-depends sources, and any other aged caches). Without \
           this flag, caches older than 24 hours are refreshed automatically; \
           newer ones are reused. For $(b,pin-depends) entries on git URLs \
           this is how new upstream commits become visible: the pinned source \
           is re-pulled, HEAD is re-resolved, and the resolved commit is baked \
           into the synthesized opam file's $(b,url:) fragment. That fragment \
           participates in the layer hash, so a new upstream commit \
           invalidates the binary cache for the pin (and its reverse deps) and \
           triggers a rebuild. Between refreshes, upstream changes on a git \
           pin are not observed."
        [ "refresh" ])

(* Common CLI flag: [--with-repo URL], repeatable. Available on every
   command that solves — extras are merged with project-declared
   [x-opam-repositories:] at the call site. *)
let with_repos_term =
  Arg.(
    value & opt_all string []
    & info ~docv:"URL"
        ~doc:
          "Additional opam repository URL to include in solving. Repeatable; \
           merges with any [x-opam-repositories:] declared in the project's \
           *.opam files."
        [ "with-repo" ])

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
          "Additional dependency to include in solving. Accepts a package name \
           or an opam atom with a version constraint (e.g. --with=fmt>=0.9). \
           Repeatable."
        [ "with" ])

(* Convert a CLI [--with-repo URL] entry into a [Project.extra_repo] with a
   deterministic hashed name. We keep the CLI shape (a list of bare URLs)
   and invent a stable local clone directory name here at the boundary. *)
let cli_extra_repo_of_url url_s : Oi.Project.extra_repo =
  let hash = Digest.string url_s |> Digest.to_hex in
  let name = "extra-" ^ String.sub hash 0 10 in
  { name; url = url_s }

let cli_extra_repos urls = List.map cli_extra_repo_of_url urls

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
   success. *)
let ensure_remote_index ~sys ~fs ~cache ~os_key ~registry =
  if registry = "" then None
  else
    let cache_root = Oi.Cache.root_s cache in
    let os_dir = cache_root / "layers" / os_key in
    let local_path = os_dir / "remote-index.db" in
    let fresh =
      try
        let st = Unix.stat local_path in
        Unix.gettimeofday () -. st.Unix.st_mtime < remote_index_max_age
      with Unix.Unix_error _ -> false
    in
    if fresh then Some local_path
    else
      let url = url_join registry (os_key / "index.db") in
      let dst = Eio.Path.(fs / local_path) in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / os_dir);
      if D10.Sysops.Curl.fetch sys ~url ~dst then Some local_path
      else if Sys.file_exists local_path then begin
        Logs.warn (fun m ->
            m "Failed to fetch registry index, using stale cache");
        Some local_path
      end
      else begin
        Logs.warn (fun m -> m "Failed to fetch registry index from %s" registry);
        None
      end

(* Merge the remote index into the local index, creating the local index
   if it doesn't exist. *)
let merge_remote_into_local ~index_path ~remote_path =
  let db = D10.Index.open_ ~path:index_path in
  (try D10.Index.merge_remote db ~remote_path
   with Failure msg ->
     Logs.warn (fun m -> m "Failed to merge remote index: %s" msg));
  D10.Index.close db

(* Ensure the local index exists, rebuilding it from the layer cache if
   missing. Call before any index query in oi run / oi which. *)
let ensure_local_index ~sys ~fs ~clock ~cache ~os_key =
  let index_path = Oi.Cache.root_s cache / "layers" / os_key / "index.db" in
  if not (Sys.file_exists index_path) then begin
    Logs.info (fun m -> m "Building local index for %s" os_key);
    let d10 : D10.Config.t =
      { sys; fs; clock; root = Oi.Cache.root cache; os_key }
    in
    let db = D10.Index.open_ ~path:index_path in
    D10.Index.rebuild d10 db;
    D10.Index.close db
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

let get_packages_dirs ~data_dir =
  let dirs = Oi.Repo.packages_dirs ~data_dir in
  if dirs = [] then
    Oi.Error.config_error "No repositories configured. Run 'oi config' first.";
  dirs

(* True when the cwd is a dev-mode project: at least one *.opam file
   is present AND none of them declare a numeric [version:]. opam
   treats a missing [version:] as the implicit pin-version "dev", so
   a project without explicit versions is deliberately tracking dev
   code and the downstream [dev]-version warning would just be noise.
   A mixed-version workspace (some .opam with numeric versions, some
   without) counts as non-dev — the numeric files represent a real
   release cadence. *)
let project_is_dev ~fs cwd =
  let opam_files =
    try
      Eio.Path.read_dir Eio.Path.(fs / cwd)
      |> List.filter (fun f -> Filename.check_suffix f ".opam")
    with Eio.Exn.Io _ -> []
  in
  let has_numeric_version file =
    let path = cwd / file in
    match OpamFile.OPAM.read_opt (OpamFile.make (OpamFilename.raw path)) with
    | exception _ -> false
    | None -> false
    | Some opam -> (
        match OpamFile.OPAM.version_opt opam with
        | None -> false
        | Some v ->
            let s = OpamPackage.Version.to_string v in
            String.length s > 0 && s.[0] >= '0' && s.[0] <= '9')
  in
  opam_files <> [] && not (List.exists has_numeric_version opam_files)

(* Warn when the solver picks a package with a non-numeric opam version
   (e.g. "dev", "master") AND that candidate came from a non-built-in
   repository. opam's version comparator treats pure-alpha segments as
   higher than any numeric, so an overlay entry with [version: unset]
   or [version: "dev"] silently wins against every stable release that
   satisfies the same [>= X] constraint. Pinned packages are trusted
   since the user asked for them explicitly; dev-mode projects are
   trusted because pulling in dev transitives is the explicit point of
   the overlay. *)
let warn_suspicious_versions ?(dev_mode = false) ~data_dir ~pin_dir
    ~packages_dirs solved =
  if dev_mode then ()
  else
    let builtins = Oi.Repo.packages_dirs ~data_dir in
    let trusted d =
      List.mem d builtins
      || match pin_dir with Some p -> d = p | None -> false
    in
    let source_dir pkg =
      List.find_opt
        (fun d ->
          let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
          Sys.file_exists (d / name / OpamPackage.to_string pkg / "opam"))
        packages_dirs
    in
    List.iter
      (fun pkg ->
        let ver = OpamPackage.(Version.to_string (version pkg)) in
        if String.length ver > 0 && not (ver.[0] >= '0' && ver.[0] <= '9') then
          match source_dir pkg with
          | Some d when not (trusted d) ->
              Fmt.epr
                "%a %s picked from non-default repo %s.@.  Its version %S is \
                 non-numeric; opam's version comparator ranks alphabetic \
                 segments above any numeric version, so the solver preferred \
                 it over every stable release satisfying the same \
                 constraint.@.  If this is unintended, cap the dependency in \
                 your *.opam or pin the specific version you want.@."
                Fmt.(styled `Yellow string)
                "warning:"
                (OpamPackage.to_string pkg)
                d ver
          | _ -> ())
      solved

let make_d10 ~sys ~fs ~clock ~cache ~os_key : D10.Config.t =
  { sys; fs; clock; root = Oi.Cache.root cache; os_key }

(* Standard per-command bootstrap. Returns the fields most commands derive
   from the Eio environment, plus the configured cache. *)
let bootstrap env cache_dir =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let sys = D10.Sysops.create ~proc_mgr ~fs in
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

(* Parse a CLI target as either "name", "name.version", or an opam atom
   like "name>=1.0" / "name=1.0". Returns the bare name and an optional
   version constraint for the solver. *)
let parse_pkg_target s =
  match OpamPackage.of_string_opt s with
  | Some pkg -> (OpamPackage.name pkg, Some (`Eq, OpamPackage.version pkg))
  | None -> OpamFormula.atom_of_string s

(* -- Remote registry helpers ---------------------------------------------- *)

(** Try fetching uncached [Source] layers from [remote]. Returns a new action
    plan with downloaded layers promoted to [Binary]. No-op when [remote] is
    [None] or every layer is already cached. *)
let fetch_remote_layers ~remote ~d10 ~packages_dirs ~ctx ~pkgs build_plan =
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
          Eio.Fiber.all
            (List.map
               (fun hash () ->
                 let sha256 =
                   Option.map
                     (fun (e : D10.Layer.index_entry) -> e.sha256)
                     (Hashtbl.find_opt index hash)
                 in
                 if D10.Layer.pull_remote d10 ~remote:r ~hash ?sha256 () then
                   Logs.info (fun m -> m "Fetched %s from registry" hash))
               available);
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
    ?(refresh = false) ?project_dir ?remote
    ?(constraints = OpamPackage.Name.Map.empty) names =
  let dev_mode =
    match project_dir with None -> false | Some cwd -> project_is_dev ~fs cwd
  in
  let extra_pkg_dirs = Oi.Repo.ensure_extra ~data_dir ~refresh extra_repos in
  let pin_dir = Oi.Pin.materialize ~fs ~sys ~cache ~refresh pins in
  let packages_dirs =
    Stdlib.Option.to_list pin_dir @ extra_pkg_dirs @ get_packages_dirs ~data_dir
  in
  let cache_root = Oi.Cache.root_s cache in
  let build_prefix = cache_root / "build" / "prefix" in
  let ctx = Oi.Opam_ctx.create ~prefix:build_prefix ~packages_dirs ~conf in
  let pkgs =
    match Oi.Solve.solve ctx ~packages_dirs ~constraints names with
    | Ok pkgs -> pkgs
    | Error msg -> Oi.Error.no_solution msg
  in
  warn_suspicious_versions ~dev_mode ~data_dir ~pin_dir ~packages_dirs pkgs;
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
    fetch_remote_layers ~remote ~d10 ~packages_dirs ~ctx ~pkgs build_plan
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
      Oi.Plan.create ctx ~cache_root ~os_key ~ocaml_version:conf.ocaml_version
        build_plan
    in
    Oi.Execute.run ~proc_mgr ~fs
      ~clock:(clock :> D10.Config.clk)
      ~sys ~os_key exec_plan;
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
    script_path with_deps args =
  let file_deps = Oi.Script.parse_deps_from_file script_path in
  let cli_deps = List.map Oi.Script.parse_dep with_deps in
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
              ~dune_cache_root:(Oi.Cache.dune_root cache))
         (cached_bin :: args))
  else begin
    let packages_dirs = Oi.Repo.packages_dirs ~data_dir in
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
      let dev_mode =
        let cwd_s, _ = resolved_cwd fs in
        project_is_dev ~fs cwd_s
      in
      warn_suspicious_versions ~dev_mode ~data_dir ~pin_dir:None ~packages_dirs
        pkgs;
      let d10 = make_d10 ~sys ~fs ~clock ~cache ~os_key in
      let plan = Oi.Action.plan ctx ~d10 ~packages_dirs pkgs in
      let exec_plan =
        Oi.Plan.create ctx ~cache_root ~os_key ~ocaml_version:conf.ocaml_version
          plan
      in
      Oi.Execute.run ~proc_mgr ~fs
        ~clock:(clock :> D10.Config.clk)
        ~sys ~os_key exec_plan
    end;
    let build_dir = run_dir_s in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / build_dir);
    Oi.Script.generate_project ~script:script_path ~deps:all_deps ~dir:build_dir;
    let build_env =
      Oi.Prefix.make_env ~prefix ~dune_cache_root:(Oi.Cache.dune_root cache)
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
      with_repos args =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    init_opam_root ~fs ~data_dir;
    Oi.Repo.ensure ~data_dir ~refresh ();
    let conf = make_conf ~platform in
    let remote = remote_of_registry registry in
    let dune_cache_root = Oi.Cache.dune_root cache in
    let extra_deps = List.map Oi.Script.parse_dep with_deps in
    let extra_constraints = Oi.Script.constraints extra_deps in
    (* Resolve the cwd once; reused for project-extras loading and the
       script-file existence check below. *)
    let cwd_s, cwd = resolved_cwd fs in
    (* Load project extras (if any *.opam in cwd). A missing/unreadable
       directory degrades to "no extras"; malformed metadata still raises
       [Error.E] so the user sees the problem. *)
    let project_extras, project_pins =
      match Oi.Project.load ~fs cwd_s with
      | exception Sys_error _ -> ([], [])
      | exception Eio.Exn.Io _ -> ([], [])
      | p -> (p.extra_repos, p.pins)
    in
    let cli_extras = cli_extra_repos with_repos in
    let all_extras = merge_extras ~cli:cli_extras ~project:project_extras in
    let solve_assemble_run pkg_names =
      Logs.info (fun m ->
          m "Solving for packages: %s" (String.concat ", " pkg_names));
      let names = List.map OpamPackage.Name.of_string pkg_names in
      let layer_hashes =
        solve_and_ensure_layers ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf
          ~os_key ~dry_run ~extra_repos:all_extras ~pins:project_pins ~refresh
          ~project_dir:cwd_s ?remote ~constraints:extra_constraints names
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
             ~env:(Oi.Prefix.make_env ~prefix ~dune_cache_root)
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
    (* Only .ml files are treated as scripts *)
    if Filename.check_suffix target ".ml" then begin
      if not (path_exists cwd target) then
        Oi.Error.not_found target "file not found: %s" target;
      (* For scripts, solve deps first to get a prefix with the compiler *)
      let all_script_deps =
        Oi.Script.parse_deps_from_file target @ extra_deps
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
            ~refresh ?remote ~constraints dep_opam_names
      in
      if dry_run && dep_opam_names = [] then
        (* No deps to solve, but still in dry-run mode — just exit *)
        exit 0;
      let prefix =
        assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes
      in
      run_script ~sys ~fs ~proc_mgr ~clock ~os_key ~prefix ~conf ~cache
        ~data_dir target with_deps args
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
      in
      let solve_assemble_run_with pkg_names =
        solve_assemble_run (pkg_names @ extra_names)
      in
      (* Step 0: If --with deps are given, try solving for just those first.
         The target binary might come from a --with package. *)
      let from_with =
        if extra_names <> [] then solve_assemble_run extra_names else false
      in
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
            @ Oi.Repo.ensure_extra ~data_dir ~refresh all_extras
            @ Oi.Repo.packages_dirs ~data_dir
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
    Cmd.info "run" ~doc:"Run an OCaml script or an installed binary"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Solve dependencies, build from source (or restore from cache), \
             assemble a prefix, and run the target. Subsequent runs with the \
             same dependencies are instant.";
          `S "BINARY MODE";
          `P
            "When TARGET is not a .ml file, oi looks for a binary of that \
             name. If --with is given, those packages are solved first. \
             Otherwise oi tries the target name as a package, then dash-split \
             prefixes (e.g. $(b,ocluster-admin) tries $(b,ocluster-admin), \
             then $(b,ocluster)).";
          `P "Examples:";
          `Pre
            "  oi run utop\n\
            \  oi run ocamlformat -- --help\n\
            \  oi run --with=crockford roguedoi";
          `S "SCRIPT MODE";
          `P
            "When TARGET ends in .ml, oi treats it as an OCaml script. \
             Dependencies are declared on the first line using an OCaml \
             attribute:";
          `Pre "  [@@@opam fmt cmdliner lwt]";
          `P "Version constraints use standard opam syntax:";
          `Pre "  [@@@opam fmt>=0.9.0 cmdliner>=1.2.0]";
          `P
            "The script is compiled into a dune project with the declared \
             packages as libraries. The compiled binary is cached by a hash of \
             the script contents and its dependencies, so edits trigger a \
             rebuild but unchanged scripts run instantly.";
          `P "Examples:";
          `Pre
            "  oi run my_script.ml\n\
            \  oi run my_script.ml --with=tls -- arg1 arg2";
          `S "DRY RUN";
          `P
            "With -n/--dry-run, oi solves and checks the layer cache but does \
             not build or run anything. The output shows each package as \
             $(b,source) (needs building), $(b,binary) (cached), $(b,remote) \
             (available from registry), or $(b,virtual) (no-op).";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ dry_run $ registry_term $ target $ with_deps_term $ with_repos_term
      $ args)

(* -- plan ---------------------------------------------------------------- *)

let plan_cmd =
  let run () data_dir cache_dir refresh targets with_repos with_deps =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    init_opam_root ~fs ~data_dir;
    Oi.Repo.ensure ~data_dir ~refresh ();
    let conf = make_conf ~platform in
    let cwd_s, _ = resolved_cwd fs in
    let project_extras, project_pins =
      match Oi.Project.load ~fs cwd_s with
      | exception Sys_error _ -> ([], [])
      | exception Eio.Exn.Io _ -> ([], [])
      | p -> (p.extra_repos, p.pins)
    in
    let all_extras =
      merge_extras ~cli:(cli_extra_repos with_repos) ~project:project_extras
    in
    let extra_pkg_dirs = Oi.Repo.ensure_extra ~data_dir ~refresh all_extras in
    let pin_dir = Oi.Pin.materialize ~fs ~sys ~cache ~refresh project_pins in
    let packages_dirs =
      Stdlib.Option.to_list pin_dir
      @ extra_pkg_dirs
      @ get_packages_dirs ~data_dir
    in
    let extra_deps = List.map Oi.Script.parse_dep with_deps in
    let extra_constraints = Oi.Script.constraints extra_deps in
    let extra_names =
      List.filter_map
        (fun (d : Oi.Script.dep) ->
          if OpamPackage.Name.to_string d.name = "ocaml" then None
          else Some d.name)
        extra_deps
    in
    let names = List.map OpamPackage.Name.of_string targets @ extra_names in
    let build_prefix = Oi.Cache.root_s cache / "build" / "prefix" in
    let ctx = Oi.Opam_ctx.create ~prefix:build_prefix ~packages_dirs ~conf in
    let pkgs =
      match
        Oi.Solve.solve ctx ~packages_dirs ~constraints:extra_constraints names
      with
      | Ok pkgs -> pkgs
      | Error msg -> Oi.Error.no_solution msg
    in
    warn_suspicious_versions ~dev_mode:(project_is_dev ~fs cwd_s) ~data_dir
      ~pin_dir ~packages_dirs pkgs;
    let d10 =
      make_d10 ~sys ~fs ~clock:(clock :> D10.Config.clk) ~cache ~os_key
    in
    let action_plan = Oi.Action.plan ctx ~d10 ~packages_dirs pkgs in
    let plan =
      Oi.Plan.create ctx ~cache_root:(Oi.Cache.root_s cache) ~os_key
        ~ocaml_version:conf.ocaml_version action_plan
    in
    Fmt.pr "%a@." Oi.Plan.pp plan
  in
  let targets =
    Arg.(
      non_empty & pos_all string []
      & info ~docv:"PKG" ~doc:"Package(s) to plan" [])
  in
  let info =
    Cmd.info "plan" ~doc:"Show the resolved build plan for package(s)"
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ targets $ with_repos_term $ with_deps_term)

(* -- env ----------------------------------------------------------------- *)

let env_cmd =
  let run () data_dir cache_dir refresh with_repos with_deps =
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
        Oi.Repo.ensure ~data_dir ~refresh ();
        let conf = make_conf ~platform in
        let extras = cli_extra_repos with_repos in
        let extra_cli = List.map Oi.Script.parse_dep with_deps in
        let extra_constraints = Oi.Script.constraints extra_cli in
        let extra_names =
          List.filter_map
            (fun (d : Oi.Script.dep) ->
              if OpamPackage.Name.to_string d.name = "ocaml" then None
              else Some d.name)
            extra_cli
        in
        let layer_hashes =
          solve_and_ensure_layers ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir
            ~conf ~os_key ~refresh ~extra_repos:extras
            ~constraints:extra_constraints
            (OpamPackage.Name.of_string "ocaml" :: extra_names)
        in
        assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes
      end
    in
    let vars = Oi.Prefix.env_vars ~prefix ~dune_cache_root in
    let current_path =
      try Sys.getenv "PATH" with Not_found -> "/usr/bin:/bin"
    in
    List.iter
      (fun (k, v) ->
        let v =
          if k = "PATH" then (prefix / "bin") ^ ":" ^ current_path else v
        in
        Fmt.pr "export %s=\"%s\"@." k v)
      vars
  in
  let info =
    Cmd.info "env" ~doc:"Print shell environment for the current project"
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ with_repos_term $ with_deps_term)

(* -- init ---------------------------------------------------------------- *)

(* -- which --------------------------------------------------------------- *)

let which_cmd =
  let run () cache_dir registry pattern =
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
    D10.Index.close db;
    if results = [] then Fmt.pr "No binaries matching %s@." pattern
    else begin
      (* Determine which hashes are available locally *)
      let d10 : D10.Config.t =
        { sys; fs; clock = clk; root = Oi.Cache.root cache; os_key }
      in
      List.iter
        (fun (binary, pkg_name, pkg_ver, hash) ->
          let source =
            if D10.Layer.succeeded d10 ~hash then
              Fmt.str "%a" Fmt.(styled `Green string) "local"
            else Fmt.str "%a" Fmt.(styled `Cyan string) "remote"
          in
          Fmt.pr "%-20s %s.%-12s (%s)@." binary pkg_name pkg_ver source)
        results
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
  let info = Cmd.info "which" ~doc:"Search for binaries in the layer index" in
  Cmd.v info
    Term.(const run $ log_term $ cache_dir_term $ registry_term $ pattern)

(* -- sync ---------------------------------------------------------------- *)

(* Run a full sync in [cwd]: solve the deps declared in *.opam files,
   build/fetch layers, assemble [cwd]/_oi/prefix, and (re)write .envrc.
   Returns the path to the assembled prefix. When [quiet] is true,
   narration goes to Logs.info (hidden at default verbosity); otherwise
   it prints to stdout. *)
let do_sync ?(quiet = false) ?(refresh = false) ?(with_repos = [])
    ?(with_deps = []) ~proc_mgr ~fs ~clock ~sys ~platform ~os_key ~cache
    ~data_dir ~registry ~cwd () =
  let say fmt =
    if quiet then Fmt.kstr (fun s -> Logs.info (fun m -> m "%s" s)) fmt
    else Fmt.kstr (fun s -> Fmt.pr "%s@." s) fmt
  in
  init_opam_root ~fs ~data_dir;
  Oi.Repo.ensure ~data_dir ~refresh ();
  let project = Oi.Project.load ~fs cwd in
  let deps = project.deps in
  if deps = [] && with_deps = [] then
    Oi.Error.config_error "No .opam files found in %s." cwd;
  say "Dependencies from opam files: %s" (String.concat ", " deps);
  let all_extras =
    merge_extras ~cli:(cli_extra_repos with_repos) ~project:project.extra_repos
  in
  if all_extras <> [] then
    say "Extra repositories: %s"
      (String.concat ", "
         (List.map
            (fun (e : Oi.Project.extra_repo) -> Fmt.str "%s (%s)" e.name e.url)
            all_extras));
  let conf = make_conf ~platform in
  let remote = remote_of_registry registry in
  let extra_cli = List.map Oi.Script.parse_dep with_deps in
  let extra_constraints = Oi.Script.constraints extra_cli in
  let extra_names =
    List.filter_map
      (fun (d : Oi.Script.dep) ->
        if OpamPackage.Name.to_string d.name = "ocaml" then None
        else Some d.name)
      extra_cli
  in
  let names = List.map OpamPackage.Name.of_string deps @ extra_names in
  let layer_hashes =
    solve_and_ensure_layers ~sys ~proc_mgr ~fs ~clock ~cache ~data_dir ~conf
      ~os_key ~extra_repos:all_extras ~pins:project.pins ~refresh
      ~project_dir:cwd ~constraints:extra_constraints ?remote names
  in
  let oi_dir = cwd / "_oi" in
  let prefix = oi_dir / "prefix" in
  Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / prefix);
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / oi_dir);
  let d10 = make_d10 ~sys ~fs ~clock ~cache ~os_key in
  D10.Prefix.assemble d10 ~layer_hashes ~dst:Eio.Path.(fs / prefix);
  let envrc_path = Eio.Path.(fs / cwd / ".envrc") in
  let dune_cache_root = Oi.Cache.dune_root cache in
  let envrc = Oi.Prefix.envrc_content ~prefix ~dune_cache_root in
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
          |> List.filter (fun f -> Filename.check_suffix f ".opam")
        with Sys_error _ -> []
      in
      List.exists
        (fun f ->
          try (Unix.stat (cwd / f)).Unix.st_mtime > prefix_mtime
          with Unix.Unix_error _ -> false)
        opam_files

let sync_cmd =
  let run () data_dir cache_dir refresh registry with_repos with_deps =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    let cwd, _ = resolved_cwd fs in
    ignore
      (do_sync ~refresh ~with_repos ~with_deps ~proc_mgr ~fs ~clock ~sys
         ~platform ~os_key ~cache ~data_dir ~registry ~cwd ())
  in
  let info =
    Cmd.info "sync"
      ~doc:
        "Scan *.opam files, solve dependencies, build, and assemble _oi/prefix/"
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ registry_term $ with_repos_term $ with_deps_term)

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
    let dep = Oi.Script.parse_dep pkg_spec in
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
    let env =
      Oi.Prefix.make_env ~prefix ~dune_cache_root:(Oi.Cache.dune_root cache)
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
    Cmd.info "add"
      ~doc:"Add an opam package dependency to the local project's dune-project"
      ~man:
        [
          `S "DESCRIPTION";
          `P
            "$(b,oi add PKG) solves the current project with $(b,PKG) as an \
             extra dependency, and — if the solve succeeds — edits \
             $(b,dune-project) to add $(b,PKG) to a $(b,\\(package ...\\)) \
             stanza's $(b,\\(depends ...\\)) list, then runs $(b,dune build) \
             so dune regenerates the $(b,*.opam) files to match. Finally \
             re-runs $(b,oi sync) so $(b,_oi/prefix) reflects the committed \
             source of truth.";
          `P
            "Requires $(b,\\(generate_opam_files\\)) to be enabled in \
             $(b,dune-project) — the whole point of the command is to let dune \
             remain the canonical source of the dependency list.";
          `P
            "Nothing is written to disk if the pre-flight solve fails, so you \
             can safely use this to probe whether a dependency would be \
             compatible.";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ registry_term $ with_repos_term $ package $ pkg_spec)

(* -- exec ---------------------------------------------------------------- *)

let exec_cmd =
  let run () data_dir cache_dir refresh registry with_repos with_deps cmd args =
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
        (do_sync ~quiet:true ~refresh ~with_repos ~with_deps ~proc_mgr ~fs
           ~clock ~sys ~platform ~os_key ~cache ~data_dir ~registry ~cwd ())
    end;
    let env_arr =
      Oi.Prefix.make_env ~prefix ~dune_cache_root:(Oi.Cache.dune_root cache)
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
    Cmd.info "exec"
      ~doc:"Run a command in the project's _oi/prefix/ environment"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Runs $(b,CMD) with PATH, OCAMLLIB, and related variables set so \
             that the toolchain assembled under $(b,_oi/prefix/) is picked up \
             — identical to sourcing the $(b,.envrc) written by $(b,oi sync).";
          `P
            "Auto-syncs when $(b,_oi/prefix/) is missing or when any \
             $(b,*.opam) in the current directory is newer than the prefix, so \
             $(b,oi exec) works without a separate $(b,oi sync) step.";
          `P "Examples:";
          `Pre
            "  oi exec dune build\n\
            \  oi exec -- ocamlformat --check .\n\
            \  oi exec utop";
        ]
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ registry_term $ with_repos_term $ with_deps_term $ cmd $ args)

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
    Fmt.pr "@,%a@," Fmt.(styled `Bold string) "Repositories";
    let config = Oi.Repo.config in
    List.iter
      (fun (r : Oi.Repo.remote) ->
        let dir = Oi.Repo.repo_dir ~data_dir r.name in
        let is_default = r.name = config.default in
        let marker = if is_default then "* " else "  " in
        let status =
          if Sys.file_exists (dir / ".git") then
            let hash =
              try D10.Sysops.Git.head_short sys ~dir:Eio.Path.(fs / dir)
              with _ -> "?"
            in
            Fmt.str "%a (%s)" Fmt.(styled `Green string) "cloned" hash
          else Fmt.str "%a" Fmt.(styled `Yellow string) "not cloned"
        in
        Fmt.pr "%s%a  %s  %s@," marker
          Fmt.(styled `Bold string)
          r.name status r.url)
      config.remotes;
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
        end
  in
  let info =
    Cmd.info "config" ~doc:"Show platform, directories, and repository status"
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
    Cmd.info "clean" ~doc:"Remove cached data and workspace artifacts"
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
    Cmd.info "show" ~doc:"Show layer cache stats and package details"
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
    Cmd.info "index" ~doc:"Build a SQLite index of the binary layer cache"
  in
  Cmd.v info Term.(const run $ log_term $ cache_dir_term)

(* -- registry ------------------------------------------------------------ *)

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

let registry_export_cmd =
  let run () cache_dir registry output =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let _proc_mgr, fs, clock, sys, _platform, os_key, cache =
      bootstrap env cache_dir
    in
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
          try Sys.remove scratch with Sys_error _ -> ()
        end
        else
          Logs.info (fun m ->
              m "No remote layer index at %s/%s/index.db (skipping merge)"
                registry os_key)
      end;
      let nl, nb, _ = D10.Index.stats db ~os_key in
      D10.Index.close db;
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
        (try Oi.Source_mirror.merge_remote ~index_path ~remote_path:scratch
         with Failure msg ->
           Logs.warn (fun m -> m "Failed to merge remote sources index: %s" msg));
        try Sys.remove scratch with Sys_error _ -> ()
      end
      else
        Logs.info (fun m ->
            m "No remote sources index at %s/sources/index.db (skipping merge)"
              registry)
    end;
    if n_sources > 0 then
      Fmt.pr "  sources: %d blob(s) at %s/sources/@." n_sources output
  in
  let output =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"DIR" ~doc:"Output directory for the registry" [])
  in
  let info =
    Cmd.info "export"
      ~doc:"Export cached layers as tar.zst archives for HTTP serving"
  in
  Cmd.v info
    Term.(const run $ log_term $ cache_dir_term $ registry_term $ output)

let registry_build_cmd =
  (* Group solutions by layer-hash compatibility. Two solutions are compatible
     if every package name appearing in both has the same layer hash — this
     catches version differences AND different dep contexts (e.g. depopts). *)
  let group_solutions ctx ~packages_dirs solutions =
    (* Per-package layer hashes, keyed by package name as a string. *)
    let hash_map_of pkgs =
      let m = Hashtbl.create 64 in
      let plan = Oi.Action.plan ctx ~packages_dirs pkgs in
      List.iter
        (fun (node : Oi.Action.node) ->
          Hashtbl.replace m
            (OpamPackage.Name.to_string (OpamPackage.name node.pkg))
            node.layer_hash)
        (Oi.Action.nodes plan);
      m
    in
    let compatible hmap group_hmap =
      Hashtbl.fold
        (fun name hash ok ->
          ok
          &&
          match Hashtbl.find_opt group_hmap name with
          | None -> true
          | Some h -> String.equal h hash)
        hmap true
    in
    let groups = ref [] in
    List.iter
      (fun ((_, pkgs) as solution) ->
        let hmap = hash_map_of pkgs in
        let merged = ref false in
        groups :=
          List.map
            (fun (gsols, ghmap) ->
              if !merged then (gsols, ghmap)
              else if compatible hmap ghmap then begin
                merged := true;
                Hashtbl.iter
                  (fun n h ->
                    if not (Hashtbl.mem ghmap n) then Hashtbl.replace ghmap n h)
                  hmap;
                (solution :: gsols, ghmap)
              end
              else (gsols, ghmap))
            !groups;
        if not !merged then groups := ([ solution ], hmap) :: !groups)
      solutions;
    List.rev !groups
  in
  let run () data_dir cache_dir refresh dry_run registry with_repos with_deps
      targets =
    with_error_handling @@ fun () ->
    with_eio_root @@ fun env _sw ->
    let proc_mgr, fs, clock, sys, platform, os_key, cache =
      bootstrap env cache_dir
    in
    init_opam_root ~fs ~data_dir;
    Oi.Repo.ensure ~data_dir ~refresh ();
    let conf = make_conf ~platform in
    let remote = remote_of_registry registry in
    let extra_pkg_dirs =
      Oi.Repo.ensure_extra ~data_dir ~refresh (cli_extra_repos with_repos)
    in
    let packages_dirs = extra_pkg_dirs @ get_packages_dirs ~data_dir in
    let cache_root = Oi.Cache.root_s cache in
    let build_prefix = cache_root / "build" / "prefix" in
    let ctx = Oi.Opam_ctx.create ~prefix:build_prefix ~packages_dirs ~conf in
    let d10 = make_d10 ~sys ~fs ~clock ~cache ~os_key in
    (* [--with] adds extra packages to every target's root set plus any
       version constraints they carry. *)
    let extra_cli = List.map Oi.Script.parse_dep with_deps in
    let base_constraints = Oi.Script.constraints extra_cli in
    let extra_names =
      List.filter_map
        (fun (d : Oi.Script.dep) ->
          if OpamPackage.Name.to_string d.name = "ocaml" then None
          else Some d.name)
        extra_cli
    in
    (* 1. Solve each target independently *)
    let n_solve_failed = ref 0 in
    let solutions =
      List.filter_map
        (fun target ->
          let name, version_constraint = parse_pkg_target target in
          let constraints =
            match version_constraint with
            | None -> base_constraints
            | Some c -> OpamPackage.Name.Map.add name c base_constraints
          in
          match
            Oi.Solve.solve ctx ~packages_dirs ~constraints (name :: extra_names)
          with
          | Ok pkgs ->
              Fmt.pr "Solved %s: %d packages@." target (List.length pkgs);
              warn_suspicious_versions ~data_dir ~pin_dir:None ~packages_dirs
                pkgs;
              Some (target, pkgs)
          | Error msg ->
              Fmt.epr "%a %s: %s@."
                Fmt.(styled `Red string)
                "FAIL (solve)" target msg;
              incr n_solve_failed;
              None)
        targets
    in
    if solutions = [] then Oi.Error.msg "no packages solved successfully";
    (* 2. Group compatible solutions *)
    let groups = group_solutions ctx ~packages_dirs solutions in
    let n_groups = List.length groups in
    Fmt.pr "%d target(s) in %d compatible group(s)@." (List.length solutions)
      n_groups;
    (* 3. Build each group *)
    let n_build_failed = ref 0 in
    List.iteri
      (fun gi (group_solutions, _) ->
        let group_targets =
          List.map fst group_solutions |> String.concat ", "
        in
        (* Merge packages, deduplicate by name+version *)
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
        (* Fresh context per group — Plan.create mutates ctx to track
           installed packages, so groups must not share state. *)
        let group_ctx =
          Oi.Opam_ctx.create ~prefix:build_prefix ~packages_dirs ~conf
        in
        let sorted_pkgs =
          Oi.Solve.topo_sort ~packages_dirs group_ctx merged_pkgs
        in
        if n_groups > 1 then
          Fmt.pr "Group %d/%d [%s]: %d packages@." (gi + 1) n_groups
            group_targets (List.length sorted_pkgs)
        else Fmt.pr "%d unique packages@." (List.length sorted_pkgs);
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
          Fmt.pr "%d to build, %d cached@." n_build n_cached;
          (* Build the exec plan up front so the mirror-record pass sees
             every source, whether this group rebuilt or not — opam's
             download-cache may still hold tarballs from a prior run
             that didn't make it into the mirror (e.g. a crash in the
             first rollout of this feature). *)
          let exec_plan =
            Oi.Plan.create group_ctx ~cache_root ~os_key
              ~ocaml_version:conf.ocaml_version build_plan
          in
          if n_build > 0 then begin
            let build_plan =
              fetch_remote_layers ~remote ~d10 ~packages_dirs ~ctx:group_ctx
                ~pkgs:sorted_pkgs build_plan
            in
            try
              (* Read-side mirror wiring: opam probes the local mirror
                 first (free on hit), then the remote registry's sources/
                 subdir (if a registry is configured), then falls back to
                 the upstream url: on miss. [build_plan] here may have
                 been rewritten by [fetch_remote_layers], so we rebuild
                 [exec_plan] fresh rather than reusing the outer one. *)
              let exec_plan =
                Oi.Plan.create group_ctx ~cache_root ~os_key
                  ~ocaml_version:conf.ocaml_version build_plan
              in
              let cache_urls =
                let local = Oi.Source_mirror.url ~cache in
                match remote with
                | Some (`Http_remote r) ->
                    [ local; Oi.Source_mirror.remote_url ~registry:r ]
                | _ -> [ local ]
              in
              Oi.Execute.run ~cache_urls ~proc_mgr ~fs
                ~clock:(clock :> D10.Config.clk)
                ~sys ~os_key exec_plan
            with
            | Oi.Error.E e ->
                Fmt.epr "%a@." Oi.Error.pp e;
                incr n_build_failed
            | Failure msg ->
                Fmt.epr "%a %s: %s@."
                  Fmt.(styled `Red string)
                  "FAIL (build)" group_targets msg;
                incr n_build_failed
          end;
          (* Write-side mirror: promote every source blob that lives in
             opam's download-cache into the registry mirror, regardless
             of [method_]. Binary-cached layers still have source info
             on the plan and may still have their tarballs in opam's
             cache from an earlier run; [record] is a silent no-op for
             anything not in the cache. *)
          try
            List.iter
              (fun (group : Oi.Plan.group) ->
                List.iter
                  (fun (p : Oi.Plan.package_plan) ->
                    let package = OpamPackage.of_string p.pkg in
                    Stdlib.Option.iter
                      (fun (src : Oi.Plan.source_info) ->
                        let checksums =
                          List.map OpamHash.of_string src.checksums
                        in
                        let url = OpamUrl.parse ~handle_suffix:true src.url in
                        Oi.Source_mirror.record ~sys ~cache ~package ~kind:`Main
                          ~url ~checksums)
                      p.source;
                    List.iter
                      (fun (name, (src : Oi.Plan.source_info)) ->
                        let checksums =
                          List.map OpamHash.of_string src.checksums
                        in
                        let url = OpamUrl.parse ~handle_suffix:true src.url in
                        Oi.Source_mirror.record ~sys ~cache ~package
                          ~kind:(`Extra name) ~url ~checksums)
                      p.extra_sources)
                  group.packages)
              exec_plan.groups
          with Failure msg ->
            Fmt.epr "%a %s: %s@."
              Fmt.(styled `Yellow string)
              "WARN (mirror)" group_targets msg
        end)
      groups;
    if not dry_run then begin
      Fmt.pr "Done: %d target(s)" (List.length solutions);
      if !n_solve_failed > 0 then Fmt.pr ", %d failed to solve" !n_solve_failed;
      if !n_build_failed > 0 then
        Fmt.pr ", %d group(s) failed to build" !n_build_failed;
      Fmt.pr "@."
    end
  in
  let targets =
    Arg.(
      non_empty & pos_all string []
      & info ~docv:"PKG" ~doc:"Opam packages to build layers for" [])
  in
  let dry_run =
    Arg.(
      value & flag
      & info ~doc:"Show the merged build plan without building"
          [ "n"; "dry-run" ])
  in
  let info =
    Cmd.info "build"
      ~doc:"Solve and build layers for multiple packages into the local cache"
  in
  Cmd.v info
    Term.(
      const run $ log_term $ data_dir_term $ cache_dir_term $ refresh_term
      $ dry_run $ registry_term $ with_repos_term $ with_deps_term $ targets)

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
    Oi.Repo.ensure ~data_dir ~refresh ();
    let proj = Oi.Project.load ~fs cwd_s in
    if proj.deps = [] && with_deps = [] then
      Oi.Error.config_error
        "No dependencies declared in %s (need at least one *.opam with \
         non-local depends, or use --with)"
        cwd_s;
    let pin_dir = Oi.Pin.materialize ~fs ~sys ~cache ~refresh proj.pins in
    let all_extras =
      merge_extras ~cli:(cli_extra_repos with_repos) ~project:proj.extra_repos
    in
    let extras = Oi.Repo.ensure_extra ~data_dir ~refresh all_extras in
    let packages_dirs =
      Stdlib.Option.to_list pin_dir @ extras @ get_packages_dirs ~data_dir
    in
    let conf =
      let c = make_conf ~platform in
      match os_override with None -> c | Some os -> { c with os }
    in
    let build_prefix = Oi.Cache.root_s cache / "build" / "prefix" in
    let ctx = Oi.Opam_ctx.create ~prefix:build_prefix ~packages_dirs ~conf in
    let extra_cli = List.map Oi.Script.parse_dep with_deps in
    let extra_constraints = Oi.Script.constraints extra_cli in
    let extra_names =
      List.filter_map
        (fun (d : Oi.Script.dep) ->
          if OpamPackage.Name.to_string d.name = "ocaml" then None
          else Some d.name)
        extra_cli
    in
    let names = List.map OpamPackage.Name.of_string proj.deps @ extra_names in
    let solved =
      match
        Oi.Solve.solve ctx ~packages_dirs ~constraints:extra_constraints names
      with
      | Ok pkgs -> pkgs
      | Error msg -> Oi.Error.no_solution msg
    in
    warn_suspicious_versions ~dev_mode:(project_is_dev ~fs cwd_s) ~data_dir
      ~pin_dir ~packages_dirs solved;
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
      ~doc:"Print system packages required by the project's dependencies"
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
  let run () packages_file output_dir src_context =
    with_error_handling @@ fun () ->
    let pkgs = Registry_docker.parse_packages_file packages_file in
    if pkgs = [] then
      Oi.Error.msg "no packages found in %s (all blank/comment lines)"
        packages_file;
    (try Unix.mkdir output_dir 0o755 with Unix.Unix_error (EEXIST, _, _) -> ());
    let pkgs_path = output_dir / "packages.txt" in
    Registry_docker.write_packages_file pkgs_path pkgs;
    let df_oi = Registry_docker.dockerfile_oi ~src_context in
    let oi_path = output_dir / "Dockerfile.oi" in
    Registry_docker.write_dockerfile oi_path df_oi;
    let per_distro_paths =
      List.map
        (fun d ->
          let fname = Registry_docker.one_distro_filename d in
          let path = output_dir / fname in
          let df =
            Registry_docker.dockerfile_one_distro ~src_context
              ~packages_ctx_path:pkgs_path d
          in
          Registry_docker.write_dockerfile path df;
          (d, path))
        default_distros
    in
    let compose_path = output_dir / "docker-compose.yml" in
    let compose_yaml =
      Registry_docker.docker_compose_yaml ~distros:default_distros
        ~registry_host_path:"./registry"
    in
    Registry_docker.write_file compose_path compose_yaml;
    Fmt.pr "Wrote:@.";
    Fmt.pr "  %s (%d packages)@." pkgs_path (List.length pkgs);
    Fmt.pr "  %s@." oi_path;
    List.iter (fun (_, path) -> Fmt.pr "  %s@." path) per_distro_paths;
    Fmt.pr "  %s@." compose_path;
    Fmt.pr "@.";
    Fmt.pr "Static oi release binary:@.";
    Fmt.pr "  docker buildx build -f %s --output type=local,dest=./oi-bin .@."
      oi_path;
    Fmt.pr "Run the registry build + export (all distros in parallel):@.";
    Fmt.pr "  docker compose up --build   # exports to ./registry/@."
  in
  let packages_file =
    Arg.(
      required
      & pos 0 (some file) None
      & info ~docv:"FILE"
          ~doc:
            "Packages file: one opam target per line ($(b,name), \
             $(b,name.version), $(b,name>=1.0)). $(b,#) starts a comment."
          [])
  in
  let output_dir =
    Arg.(
      value & opt string "."
      & info ~docv:"DIR"
          ~doc:
            "Directory to write Dockerfile.oi, Dockerfile.registry, and \
             packages.txt (created if missing)."
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
            "Each per-distro image has a $(b,CMD) that runs $(b,oi registry \
             build --registry=) for the packages in $(b,packages.txt) then \
             $(b,oi registry export /out). Running the compose project \
             executes all distros in parallel and leaves the exported layers \
             on the host:";
          `Pre "  docker compose up --build";
          `P
            "Each service exits when its build+export completes; $(b,compose \
             up) returns when every service has finished.";
        ]
  in
  Cmd.v info
    Term.(const run $ log_term $ packages_file $ output_dir $ src_context)

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
  let info = Cmd.info "stats" ~doc:"Show source mirror statistics" in
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
      ~doc:
        "Remove source blobs no longer referenced by any package in the mirror \
         index"
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
      ~doc:"Re-hash every mirrored blob and report any mismatches"
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
      ~doc:"List every source tarball currently stored in the mirror"
  in
  Cmd.v info Term.(const run $ log_term $ cache_dir_term $ package)

let registry_mirror_cmd =
  let info =
    Cmd.info "mirror"
      ~doc:
        "Manage the source tarball mirror (content-addressed, opam cache \
         format)"
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
    Cmd.info "registry" ~doc:"Manage the layer cache and remote registry"
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

(* -- main ---------------------------------------------------------------- *)

let () =
  let info =
    Cmd.info "oi" ~version:"0.1.4" ~doc:"Stateless OCaml package builder"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "oi is a stateless OCaml package builder. It solves dependencies \
             with opam-0install, builds packages in parallel stages using a \
             relocatable OCaml compiler, and caches each package as a binary \
             layer. Assembled prefixes are created on demand via hardlinks. No \
             global state, no switches, no ~/.opam.";
          `S "QUICK START";
          `P "Run any binary from the opam repository:";
          `Pre "  oi run utop\n  oi run ocamlformat -- --help";
          `P "Run an OCaml script with dependencies:";
          `Pre "  oi run my_script.ml";
          `P "The first line of the script declares opam packages:";
          `Pre "  [@@@opam fmt cmdliner lwt>=5.0]";
          `P "Show what would be built without building:";
          `Pre "  oi run -n utop";
          `P "Show the fully resolved build plan:";
          `Pre "  oi plan utop";
          `S "HOW IT WORKS";
          `P
            "On first use, oi fetches a relocatable OCaml compiler and the \
             opam package repository. For each $(b,oi run), it:";
          `P "1. Solves dependencies (opam-0install solver)";
          `P "2. Checks the binary layer cache for each package";
          `P "3. Builds uncached packages in parallel stages";
          `P "4. Captures each build as a content-addressed layer";
          `P "5. Assembles a prefix from all layers via hardlinks";
          `P "6. Runs the target binary or script";
          `P "Subsequent runs with the same dependencies skip steps 2-5.";
          `S "SCRIPT FORMAT";
          `P
            "OCaml scripts (.ml files) declare opam dependencies on the first \
             line using an attribute:";
          `Pre "  [@@@opam fmt cmdliner>=1.2.0 lwt]";
          `P
            "Each token is parsed as an opam package atom. Version constraints \
             use opam syntax: $(b,>=), $(b,>), $(b,<=), $(b,<), $(b,=). The \
             packages are installed as dune libraries, so use the findlib/dune \
             library name in your code (e.g. $(b,open Fmt) for the fmt \
             package).";
          `S "ENVIRONMENT";
          `P (Xdge.Cmd.env_docs app_name);
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
        clean_cmd;
      ]
  in
  exit (Cmd.eval cmd)
