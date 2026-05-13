[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.toolchain"

module Log = (val Logs.src_log log_src : Logs.LOG)

type info = {
  handle : string;
  ocaml_version : string;
  install_prefix : string;
  hash : string;
  relocatable : bool;
      (** [true] when the toolchain's compiler can be installed into a per-solve
          consumer prefix. Skips the fixed-prefix install and the PATH/OCAMLPATH
          layering, so the binary cache pipeline works end-to-end the same way
          it does in the no-toolchain flow. The version pin in
          {!opam_ctx_of_info} still drives compiler selection. [false] (e.g.
          oxcaml) keeps the legacy fixed-prefix behaviour. *)
  packages : OpamPackage.Set.t;
  compiler_name : OpamPackage.Name.t;
  root_names : OpamPackage.Name.Set.t;
  packages_dirs : string list;
  tools : string list;
  dep_handles : string list;
}

let ready_marker info = info.install_prefix / ".oi-toolchain-ready"
let is_ready info = info.relocatable || Sys.file_exists (ready_marker info)

let opam_ctx_of_info (info : info) : Solver.Ctx.toolchain =
  {
    install_prefix = info.install_prefix;
    hash = info.hash;
    relocatable = info.relocatable;
    packages = info.packages;
    root_names = info.root_names;
  }

let apply_conf info (conf : Solver.Ctx.conf) =
  match info with
  | None -> conf
  | Some (i : info) -> { conf with ocaml_version = i.ocaml_version }

(* -- Install root derivation -------------------------------------------- *)

(* Toolchains live under $XDG_CACHE_HOME so they sit alongside the rest
   of oi's cache. User said explicitly: use XDG_CACHE_HOME, not
   /opt-style system paths. *)
let default_root = Cache.toolchains_root

(* -- Reporepo-backed toolchain discovery ------------------------------ *)

(* All toolchains live in the reporepo as definition-only entries
   (url-less, depends-only) carrying [x-oi-toolchain-name],
   [x-oi-toolchain-compiler], [x-oi-relocatable], and
   [x-oi-toolchain-roots]. The reporepo handle (e.g.
   [toolchain-oxcaml]) and the CLI toolchain name (e.g. [oxcaml])
   live in separate namespaces; the latter is the [x-oi-toolchain-name]
   field. *)

(* Read the reporepo's entries, swallowing parse failures. The empty
   list is a real possibility on a fresh machine where the reporepo
   hasn't been cloned yet — the caller decides whether that's fatal. *)
let load_entries () =
  let path = Source.Reporepo.env_path () in
  if not (Sys.file_exists path) then []
  else try Source.Reporepo.load ~path with Error.E _ -> []

(* Find the latest reporepo entry that defines a toolchain with CLI
   name [name]. Multiple reporepo handles defining the same CLI name
   is an error (ambiguous lookup). *)
let find_entry_by_toolchain_name ~name =
  let entries = load_entries () in
  let handles =
    entries
    |> List.filter_map (fun (e : Source.Reporepo.entry) ->
        match e.toolchain_name with
        | Some n when n = name -> Some e.handle
        | _ -> None)
    |> List.sort_uniq String.compare
  in
  match handles with
  | [] -> None
  | [ h ] -> Source.Reporepo.latest entries ~handle:h
  | _ ->
      Error.fail_config_error
        "multiple reporepo handles define toolchain %S: %s — fix by removing \
         the duplicate definitions"
        name
        (String.concat ", " handles)

(* For [oi show] / man-page rendering: surface the URL of the FIRST
   depends overlay as the toolchain's "primary source". Toolchain
   entries are url-less, but their first depends overlay is the
   compiler-bearing one (e.g. [oxcaml] for the oxcaml toolchain), so
   that's the URL to show. *)
let url_of ~handle =
  match find_entry_by_toolchain_name ~name:handle with
  | None -> None
  | Some e -> (
      match e.depends with
      | [] -> None
      | (h, _) :: _ -> (
          let entries = load_entries () in
          match Source.Reporepo.latest entries ~handle:h with
          | Some dep when dep.url <> "" -> Some dep.url
          | _ -> None))

let depends_of ~handle =
  match find_entry_by_toolchain_name ~name:handle with
  | None -> None
  | Some e -> Some (List.map fst e.depends)

(* -- Listing for [oi config] ------------------------------------------- *)

type summary = {
  handle : string;
  url : string;
  ref_ : string option;
  relocatable : bool;
  is_default : bool;
  depends : string list;
  roots : string list;
  tools : string list;
  installs : (string * bool) list;
}

(* Scan [default_root ()] for directories named [<handle>-*]. Each one
   represents a previously-resolved (handle, ocaml-version, hash)
   triple — present in the cache from any prior [oi run --toolchain].
   We can't reconstruct the hash without solving, so we just report
   what's on disk. The boolean is the [.oi-toolchain-ready] marker, so
   the user can tell completed installs from interrupted ones. *)
let installs_for ~handle =
  let root = default_root () in
  if not (Sys.file_exists root && Sys.is_directory root) then []
  else
    let prefix = handle ^ "-" in
    Sys.readdir root |> Array.to_list
    |> List.filter (fun n ->
        String.length n > String.length prefix
        && String.sub n 0 (String.length prefix) = prefix)
    |> List.sort String.compare
    |> List.map (fun n ->
        let install_prefix = root / n / "_opam" in
        let ready = Sys.file_exists (install_prefix / ".oi-toolchain-ready") in
        (install_prefix, ready))

(* For displaying the toolchain's "primary source" URL + ref in [oi
   config]. Same lookup as [url_of] but returns the ref too. *)
let primary_source ~entries (e : Source.Reporepo.entry) =
  match e.depends with
  | [] -> ("", None)
  | (h, _) :: _ -> (
      match Source.Reporepo.latest entries ~handle:h with
      | Some dep -> (dep.url, dep.ref_)
      | None -> ("", None))

let available () =
  let entries = load_entries () in
  let handles =
    entries
    |> List.filter_map (fun (e : Source.Reporepo.entry) ->
        match e.toolchain_name with Some _ -> Some e.handle | None -> None)
    |> List.sort_uniq String.compare
  in
  List.filter_map
    (fun h ->
      match Source.Reporepo.latest entries ~handle:h with
      | None -> None
      | Some e ->
          let cli_name =
            Stdlib.Option.value e.toolchain_name ~default:e.handle
          in
          let url, ref_ = primary_source ~entries e in
          let relocatable = Stdlib.Option.value e.relocatable ~default:true in
          let roots = List.flatten e.toolchain_roots in
          Some
            {
              handle = cli_name;
              url;
              ref_;
              relocatable;
              is_default = e.default_toolchain;
              depends = List.map fst e.depends;
              roots;
              tools = e.toolchain_tools;
              installs = installs_for ~handle:cli_name;
            })
    handles

(* -- Resolve ------------------------------------------------------------ *)

(* Parse a [name] / [name.version] / [name=version] spec into a
   [(name, version opt)] pair. *)
let parse_spec s =
  match String.index_opt s '=' with
  | Some i ->
      let n = String.sub s 0 i in
      let v = String.sub s (i + 1) (String.length s - i - 1) in
      (n, Some v)
  | None -> (
      match String.index_opt s '.' with
      | Some i ->
          let n = String.sub s 0 i in
          let v = String.sub s (i + 1) (String.length s - i - 1) in
          (n, Some v)
      | None -> (s, None))

(* Derive the install-prefix hash. Combines [D10.Layer.hash] (which
   hashes the solved packages' opam [effective_part]) with the conf's
   platform fields. Same set of inputs that drive opam-0install's
   solve, so any change → fresh install_prefix → old prefix is left
   alone. *)
let compute_hash ~packages_dirs ~(conf : Solver.Ctx.conf) pkgs =
  let layer_hash = D10.Layer.hash ~packages_dirs pkgs in
  let material =
    String.concat "\n"
      [
        conf.arch;
        conf.os;
        conf.os_distribution;
        conf.os_version;
        conf.os_family;
        layer_hash;
      ]
  in
  Digest.string material |> Digest.to_hex

(* Pick the OCaml-version-string out of the solved package set,
   using the toolchain entry's [x-oi-toolchain-compiler] spec to
   identify which package the version should come from. The spec is
   required on every toolchain definition (validated at parse time
   in [Source.Reporepo.parse_entry_file]) so there's no fallback
   guess list anymore. *)
let pick_ocaml_version ~explicit_compiler pkgs =
  let name, _ = parse_spec explicit_compiler in
  List.find_opt
    (fun p -> OpamPackage.Name.to_string (OpamPackage.name p) = name)
    pkgs
  |> Stdlib.Option.map (fun p ->
      OpamPackage.Version.to_string (OpamPackage.version p))

(* Look up the toolchain entry by name and fail with a friendly error listing
   the known handles when there's no match. *)
let entry_or_fail ~handle ~path =
  match find_entry_by_toolchain_name ~name:handle with
  | Some e -> e
  | None ->
      let known =
        load_entries ()
        |> List.filter_map (fun (e : Source.Reporepo.entry) -> e.toolchain_name)
        |> List.sort_uniq String.compare
      in
      Error.fail_config_error
        "toolchain %S not registered in reporepo at %s. Known: %s" handle path
        (if known = [] then "(none — add toolchain definition entries)"
         else String.concat ", " known)

(* Resolve the toolchain's transitive dep handles → list of URL-bearing
   overlay packages_dirs. The toolchain entry itself is url-less. *)
let dep_packages_dirs ~entries ~path (entry : Source.Reporepo.entry) =
  let dep_roots =
    List.map
      (fun (h, v) : Source.Reporepo.root -> { handle = h; version = v })
      entry.depends
  in
  let resolved =
    if dep_roots = [] then []
    else Source.Reporepo.resolve entries ~roots:dep_roots |> List.rev
  in
  List.filter_map
    (fun (e : Source.Reporepo.entry) ->
      if e.url = "" then None
      else Some (Source.Reporepo.assert_overlay_dir ~path ~handle:e.handle))
    resolved

(* Build the constraint map + name list from the toolchain's
   [x-oi-toolchain-roots] specs. *)
let constraints_of_root_specs root_specs =
  let constraints =
    List.fold_left
      (fun m spec ->
        match parse_spec spec with
        | _, None -> m
        | n, Some v ->
            OpamPackage.Name.Map.add
              (OpamPackage.Name.of_string n)
              (`Eq, OpamPackage.Version.of_string v)
              m)
      OpamPackage.Name.Map.empty root_specs
  in
  let names =
    List.map
      (fun spec -> OpamPackage.Name.of_string (fst (parse_spec spec)))
      root_specs
  in
  (constraints, names)

(* Run [Solver.raw_solve] with the toolchain's constraints — no auto-pinning
   of OCaml-family packages, so the overlay's [+ox] versions aren't fought
   against [conf.ocaml_version]. *)
let solve_toolchain_packages ~handle ~conf ~packages_dirs ~constraints names =
  let env v = Solver.filter_env conf (OpamVariable.Full.of_string v) in
  let pkgs =
    match Solver.raw_solve ~env ~packages_dirs ~constraints names with
    | Ok pkgs -> pkgs
    | Error msg ->
        Error.fail_config_error "toolchain %s: solve failed: %s" handle msg
  in
  Solver.topo_sort ~packages_dirs ~conf pkgs

let pick_explicit_compiler ~handle (entry : Source.Reporepo.entry) =
  match entry.toolchain_compiler with
  | Some s -> s
  | None ->
      (* [Source.Reporepo.parse_entry_file] guarantees a toolchain entry has
         [x-oi-toolchain-compiler] set; this branch only triggers if the
         reporepo bypassed validation. *)
      Error.fail_config_error "toolchain %s: %s.%s has no %s" handle
        entry.handle entry.version Keys.toolchain_compiler

let resolve ~fs ~sys ~data_dir:_ ~(conf : Solver.Ctx.conf) ~handle =
  (* Auto-clone the reporepo if missing so [--toolchain=HANDLE] still works on
     a fresh machine — the toolchain definitions live there now. *)
  let path = Source.Reporepo.env_path () in
  Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path
    ~url:(Source.Reporepo.env_url ())
    ();
  let entry = entry_or_fail ~handle ~path in
  let entries = load_entries () in
  Log.info (fun m ->
      m "Resolving toolchain %s (reporepo handle %s.%s, depends: %s)" handle
        entry.handle entry.version
        (String.concat ", " (List.map fst entry.depends)));
  let packages_dirs = dep_packages_dirs ~entries ~path entry in
  let root_specs = List.flatten entry.toolchain_roots in
  if root_specs = [] then
    Error.fail_config_error "toolchain %s: %s.%s declares no %s" handle
      entry.handle entry.version Keys.toolchain_roots;
  let constraints, names = constraints_of_root_specs root_specs in
  let pkgs =
    solve_toolchain_packages ~handle ~conf ~packages_dirs ~constraints names
  in
  Log.info (fun m ->
      m "Toolchain %s solved packages: %s" handle
        (String.concat ", " (List.map OpamPackage.to_string pkgs)));
  let explicit_compiler = pick_explicit_compiler ~handle entry in
  let ocaml_version =
    match pick_ocaml_version ~explicit_compiler pkgs with
    | Some v -> v
    | None ->
        Error.fail_config_error
          "toolchain %s: solved set contains no %s package" handle
          (fst (parse_spec explicit_compiler))
  in
  let hash = compute_hash ~packages_dirs ~conf pkgs in
  let short = if String.length hash >= 8 then String.sub hash 0 8 else hash in
  let dir_name = Fmt.str "%s-%s-%s" handle ocaml_version short in
  (* [opam switch create <dir> --empty] creates a local switch under
     [<dir>/_opam/], so [install_prefix] needs the suffix baked in. *)
  let install_prefix = default_root () / dir_name / "_opam" in
  let compiler_name =
    OpamPackage.Name.of_string (fst (parse_spec explicit_compiler))
  in
  {
    handle;
    ocaml_version;
    install_prefix;
    hash;
    relocatable = Stdlib.Option.value entry.relocatable ~default:true;
    packages = OpamPackage.Set.of_list pkgs;
    compiler_name;
    root_names = OpamPackage.Name.Set.of_list names;
    packages_dirs;
    tools = entry.toolchain_tools;
    dep_handles = List.map fst entry.depends;
  }

(* -- Install ------------------------------------------------------------ *)

(* Dedicated opam root for toolchain installs, kept separate from
   both the user's [~/.opam] and oi's own caches. Holds repo metadata
   and per-switch state for every toolchain oi has ever installed. *)
let opam_root_dir () = default_root () / "_opam-root"

(* Stable per-overlay opam repository name. Built from the local
   clone's basename so two different versions of the same overlay
   (e.g. [overlay-default-20260418.0] and [overlay-default-20260424.0])
   don't collide on the same opam repo slot. *)
let repo_name_of_packages_dir packages_dir =
  OpamRepositoryName.of_string
    (Filename.dirname packages_dir |> Filename.basename)

(* Pair each [packages_dir] (a [<overlay>/packages] path) with the
   opam URL for its repo root (the parent dir, which holds the
   [repo] marker file). *)
let repo_specs_of_packages_dirs packages_dirs =
  List.map
    (fun d ->
      let name = repo_name_of_packages_dir d in
      let url =
        Fmt.kstr
          (OpamUrl.parse ~from_file:false)
          "file://%s" (Filename.dirname d)
      in
      (name, url))
    packages_dirs

(* Ensure the dedicated toolchain opam root exists with a minimal
   [config] file. Idempotent — opam skips the rewrite if the file's
   already there. Mirrors the bare-bones init that
   [Solver.Ctx.init_opam] does for oi's own root. *)
let ensure_opam_root ~fs root =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / root);
  let root_dir = OpamFilename.Dir.of_string root in
  let config_path = OpamPath.config root_dir in
  if not (OpamFile.exists config_path) then begin
    Log.info (fun m -> m "Initialising opam root at %s" root);
    let config =
      OpamFile.Config.empty
      |> OpamFile.Config.with_opam_version (OpamVersion.of_string "2.0")
      |> OpamFile.Config.with_opam_root_version OpamFile.Config.root_version
    in
    OpamFile.Config.write config_path config
  end;
  root_dir

(* Run [f] with [OpamStateConfig]'s [root_dir] pointed at the
   toolchain root, restoring whatever was there beforehand. oi's
   [Solver.Ctx.create] mutates [root_dir] for every consumer solve
   anyway, so leaving the toolchain root active wouldn't break
   anything by itself, but restoring keeps the ambient state
   honest. *)
let with_opam_root ~root_dir f =
  let saved = OpamStateConfig.(!r.root_dir) in
  OpamStateConfig.update ~root_dir ();
  Fun.protect ~finally:(fun () -> OpamStateConfig.update ~root_dir:saved ()) f

(* User-facing preamble explaining why opam is about to print a wall of
   progress. opam's libs emit [foo] retrieved / installed lines
   unconditionally — we can't quiet them, so we frame them instead. *)
let print_install_preamble (info : info) =
  Fmt.pr "@.%a One-off build of toolchain %a (%s)@." Style.pp_accent_string "▸"
    Style.pp_header_string info.handle info.ocaml_version;
  Fmt.pr "  %s isn't relocatable yet, so it's installed once at a fixed@."
    info.handle;
  Fmt.pr "  prefix and reused on subsequent runs. This will go away once@.";
  Fmt.pr "  %s ships a relocatable variant.@." info.handle;
  Fmt.pr "  Prefix:  %s@.@." info.install_prefix;
  Stdlib.flush stdout

(* Register each overlay's [packages/] dir as an opam repository (equivalent
   to [opam repository add NAME URL --root ROOT]) and fetch its metadata. *)
let setup_repositories rt repos repo_names =
  let rt =
    List.fold_left
      (fun rt (name, url) ->
        if OpamRepositoryName.Map.mem name rt.OpamStateTypes.repositories then
          rt
        else OpamRepositoryCommand.add rt name url None)
      rt repos
  in
  let _failed, rt =
    OpamRepositoryCommand.update_with_auto_upgrade rt repo_names
  in
  rt

(* Install toolchain roots beyond the compiler (e.g. [ocamlfind],
   [ocamlbuild], [dune]) — these aren't pulled in by the compiler invariant
   alone. The compiler itself is excluded since [install_compiler] handled
   it already. *)
let install_extra_roots (info : info) st =
  let extra_atoms =
    OpamPackage.Name.Set.fold
      (fun n acc ->
        if OpamPackage.Name.equal n info.compiler_name then acc
        else (n, None) :: acc)
      info.root_names []
  in
  if extra_atoms = [] then st
  else begin
    Log.info (fun m ->
        m "Installing %d extra toolchain packages: %s" (List.length extra_atoms)
          (String.concat ", "
             (List.map (fun (n, _) -> OpamPackage.Name.to_string n) extra_atoms)));
    OpamClient.install st extra_atoms
  end

(* Equivalent to [opam switch create <switch_dir> <ocaml_version> --repos
   ...]: opam resolves the compiler family and installs it via
   [install_compiler]. *)
let switch_and_install gt ~rt ~repos:repo_names ~switch_dir (info : info) =
  let switch = OpamSwitch.of_string switch_dir in
  let invariant =
    OpamSwitchCommand.guess_compiler_invariant ~repos:repo_names rt
      [ info.ocaml_version ]
  in
  let (), st =
    OpamSwitchCommand.create gt ~rt ~repos:repo_names ~update_config:false
      ~invariant switch (fun st ->
        ((), OpamSwitchCommand.install_compiler st ~ask:false))
  in
  let st = install_extra_roots info st in
  OpamSwitchState.drop st

(* Drive the actual opam create + install dance. Wraps the
   [with_opam_root] / [OpamGlobalState.with_] / [OpamRepositoryState.with_]
   pile so the caller stays flat. *)
let perform_opam_install ~root_dir ~repos ~repo_names ~switch_dir (info : info)
    =
  with_opam_root ~root_dir (fun () ->
      OpamGlobalState.with_ `Lock_write @@ fun gt ->
      OpamRepositoryState.with_ `Lock_write gt @@ fun rt ->
      let rt = setup_repositories rt repos repo_names in
      switch_and_install gt ~rt ~repos:repo_names ~switch_dir info)

let write_ready_marker ~fs (info : info) =
  Fmt.kstr
    (Eio.Path.save ~create:(`Or_truncate 0o644)
       Eio.Path.(fs / ready_marker info))
    "handle: %s\nocaml: %s\nhash: %s\n" info.handle info.ocaml_version info.hash

let do_install ~fs ~reporter (info : info) =
  Fmt.kstr
    (fun s -> reporter.Build_progress.event (Status s))
    "Installing toolchain %s" info.handle;
  let switch_dir = Filename.dirname info.install_prefix in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / switch_dir);
  print_install_preamble info;
  Log.info (fun m ->
      m "Installing toolchain %s (%s) → %s" info.handle info.ocaml_version
        info.install_prefix);
  (* opam's local-switch path is the parent of [install_prefix]: passing
     [<switch_dir>] to [OpamSwitchCommand.create] makes opam populate
     [<switch_dir>/_opam/], i.e. [info.install_prefix]. Wipe [_opam] to
     start clean since opam refuses to overwrite a non-empty path. *)
  if Sys.file_exists info.install_prefix then
    Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / info.install_prefix);
  let started = Unix.gettimeofday () in
  let root_dir = ensure_opam_root ~fs (opam_root_dir ()) in
  let repos = repo_specs_of_packages_dirs info.packages_dirs in
  let repo_names = List.map fst repos in
  Log.info (fun m ->
      m "Creating opam switch at %s with compiler %s and repositories: %s"
        switch_dir info.ocaml_version
        (String.concat "," (List.map OpamRepositoryName.to_string repo_names)));
  perform_opam_install ~root_dir ~repos ~repo_names ~switch_dir info;
  write_ready_marker ~fs info;
  let elapsed = Unix.gettimeofday () -. started in
  Fmt.pr "@.%a Toolchain %s ready at %s (%.0fs)@." Style.pp_strong_ok_string "▸"
    info.handle info.install_prefix elapsed;
  Log.info (fun m ->
      m "Toolchain %s (%s) ready at %s" info.handle info.ocaml_version
        info.install_prefix)

let ensure_installed ?(reporter = Build_progress.null) ~fs (info : info) =
  if info.relocatable then
    Log.debug (fun m ->
        m
          "toolchain %s is relocatable — skipping fixed-prefix install (the \
           consumer solve will build the compiler into its own prefix)"
          info.handle)
  else if is_ready info then
    Log.debug (fun m ->
        m "toolchain %s already installed at %s" info.handle info.install_prefix)
  else do_install ~fs ~reporter info
