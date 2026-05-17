open Cmdliner

let ( / ) = Filename.concat

(* Best-effort handle name from a packages_dir path:
   [.../v2/avsm/packages] → "avsm"; [.../packages] → "default". *)
let handle_of_packages_dir d =
  let parent = Filename.dirname d in
  let name = Filename.basename parent in
  if name = "" || name = "/" then "default" else name

(* Locate the [<pkgs_dir>/<name>/<name.version>] dir that contributed
   [pkg]'s opam file. Same lookup as [Plan.find_pkg_source_dir], but
   we need it without a resolved [Plan.t] in scope. *)
let find_pkg_source_dir ~packages_dirs (pkg : OpamPackage.t) =
  let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
  let full = OpamPackage.to_string pkg in
  List.find_map
    (fun d ->
      if Sys.file_exists (d / name / full) then Some (d, name, full) else None)
    packages_dirs

(* Read [pkg]'s [url:] block — the package's "main" source. Multi-archive
   packages (with [extra-sources:]) deliberately ignored: per the
   bundle's "pick the first one" contract, we only carry the main url
   for each package. Returns [None] for sourceless / virtual packages. *)
let main_source ~packages_dirs (pkg : OpamPackage.t) =
  match find_pkg_source_dir ~packages_dirs pkg with
  | None -> None
  | Some (d, name, full) -> (
      let path = d / name / full / "opam" in
      try
        let opam =
          OpamFile.OPAM.read (OpamFile.make (OpamFilename.of_string path))
        in
        match OpamFile.OPAM.url opam with
        | None -> None
        | Some u -> Some (OpamFile.URL.url u, OpamFile.URL.checksum u)
      with Sys_error _ | Failure _ -> None)

(* Identity key for dedup. Tarballs key on their first declared
   checksum (content-addressed); git URLs key on the URL string
   (which carries the [#commit] pin). Two packages whose main source
   maps to the same key share one extracted directory — the second
   one's [*.opam] file lives inside the first's tree, dune walks
   both. *)
let archive_key (url : OpamUrl.t) (checksums : OpamHash.t list) =
  match checksums with
  | ck :: _ -> "h:" ^ OpamHash.to_string ck
  | [] -> "u:" ^ OpamUrl.to_string url

(* Split the solver's closure into [(consumer, toolchain)]. Toolchain
   packages stay out of the bundle entirely — they're built by oi's
   [--toolchain] machinery, and listing them in [depends:] would force
   [opam install --deps-only] to materialise the compiler outside that
   pinning. *)
let partition_toolchain ~(toolchain : Oi.Toolchain.info option) pkgs =
  match toolchain with
  | None -> (pkgs, [])
  | Some info ->
      List.partition (fun p -> not (OpamPackage.Set.mem p info.packages)) pkgs

(* -- Reporepo subset ---------------------------------------------------- *)

let mkdir_p d =
  let rec go d =
    if Sys.file_exists d then ()
    else begin
      go (Filename.dirname d);
      try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end
  in
  go d

(* Byte-copy fallback when [link] is unavailable (cross-fs etc.). *)
let copy_file_contents src dst =
  let i = open_in_bin src and o = open_out_bin dst in
  Fun.protect
    ~finally:(fun () ->
      close_in_noerr i;
      close_out_noerr o)
    (fun () ->
      let buf = Bytes.create 65536 in
      let rec loop () =
        let n = input i buf 0 (Bytes.length buf) in
        if n > 0 then begin
          output_bytes o buf;
          loop ()
        end
      in
      loop ())

(* Hardlink [src] to [dst], falling back to a byte copy on failure
   (typically [EXDEV] across filesystems). *)
let link_or_copy src dst =
  try Unix.link src dst with Unix.Unix_error _ -> copy_file_contents src dst

let copy_symlink src dst =
  let target = Unix.readlink src in
  try Unix.symlink target dst with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let mkdir_existing_ok dst =
  try Unix.mkdir dst 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

(* Recursive copy of [src] into [dst], hardlinking regular files when
   possible (cross-fs gracefully falls back to byte copy). Used to
   snapshot a reporepo [<pkg>/<pkg.ver>/] directory — that tree
   carries the [opam] file plus any [files/] subdir holding
   [extra-files:] patches. *)
let rec copy_tree src dst =
  Sys.readdir src |> Array.iter (fun name -> copy_entry src dst name)

and copy_entry src dst name =
  let s = src / name and d = dst / name in
  match (Unix.lstat s).Unix.st_kind with
  | Unix.S_DIR ->
      mkdir_existing_ok d;
      copy_tree s d
  | Unix.S_REG when not (Sys.file_exists d) -> link_or_copy s d
  | Unix.S_LNK -> copy_symlink s d
  | _ -> ()

let copy_reporepo_subset ~packages_dirs ~bundle_repo (pkgs : OpamPackage.t list)
    =
  let n = ref 0 in
  List.iter
    (fun pkg ->
      match find_pkg_source_dir ~packages_dirs pkg with
      | None -> ()
      | Some (d, name, full) ->
          let src = d / name / full in
          let dst =
            bundle_repo / "v2" / handle_of_packages_dir d / "packages" / name
            / full
          in
          if Sys.file_exists src && not (Sys.file_exists dst) then begin
            mkdir_p dst;
            copy_tree src dst;
            incr n
          end)
    pkgs;
  !n

(* -- Per-package extraction --------------------------------------------- *)

(* Extract [pkg]'s main source into [dst] via the same fetch chain
   [oi build] uses. [cache_urls] points opam at the local mirror
   (which we just warmed up), so tarballs we already fetched are
   read from disk; git URLs are cloned + checked out at the pinned
   commit. opam's [pull_tree] handles tarball strip-1, git
   clone-and-checkout, and patch application uniformly. *)
let extract_main_source ~cache_urls ~dst (pkg : OpamPackage.t) (url : OpamUrl.t)
    (checksums : OpamHash.t list) =
  let cache_dir =
    OpamRepositoryPath.download_cache OpamStateConfig.(!r.root_dir)
  in
  let result =
    OpamRepository.pull_tree
      (OpamPackage.to_string pkg)
      ~cache_dir ~cache_urls
      (OpamFilename.Dir.of_string dst)
      checksums [ url ]
    |> OpamProcess.Job.run
  in
  match result with
  | OpamTypes.Result _ | OpamTypes.Up_to_date _ -> Ok ()
  | OpamTypes.Not_available (_, msg) -> Error msg

type install = {
  extracted : OpamPackage.t list;  (** Got their own [vendor/<pkg>/]. *)
  duplicates : (OpamPackage.t * string) list;
      (** Share an archive with an [extracted] package; second arg is the
          basename of the directory hosting both opam files. *)
  virtuals : OpamPackage.t list;
      (** No main [url:] in the opam file (sourceless package). *)
  failures : (string * string) list;
}

(* pkg-name → bundle subdir. Duplicates point at the [extracted]
   package they share an archive with. *)
let dir_of_pkg install =
  let m = Hashtbl.create 64 in
  let bind pkg dir =
    Hashtbl.replace m (OpamPackage.Name.to_string (OpamPackage.name pkg)) dir
  in
  List.iter
    (fun pkg -> bind pkg (OpamPackage.Name.to_string (OpamPackage.name pkg)))
    install.extracted;
  List.iter (fun (pkg, dir) -> bind pkg dir) install.duplicates;
  m

let dir_is_dune_buildable dir =
  Sys.file_exists (dir / "dune-project") || Sys.file_exists (dir / "dune")

(* Accumulator for [install_sources]; mutated as packages are visited. *)
type install_acc = {
  by_key : (string, string) Hashtbl.t;
  mutable extracted : OpamPackage.t list;
  mutable duplicates : (OpamPackage.t * string) list;
  mutable virtuals : OpamPackage.t list;
  mutable failures : (string * string) list;
}

let new_install_acc () =
  {
    by_key = Hashtbl.create 64;
    extracted = [];
    duplicates = [];
    virtuals = [];
    failures = [];
  }

(* Extract [pkg] into [vendor_dir/<name>/], unless an existing tree
   already covers it. Records the outcome in [acc]. *)
let install_one ~cache_urls ~vendor_dir ~acc pkg url checksums =
  let key = archive_key url checksums in
  match Hashtbl.find_opt acc.by_key key with
  | Some dir -> acc.duplicates <- (pkg, dir) :: acc.duplicates
  | None -> (
      let pkg_name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
      let dst = vendor_dir / pkg_name in
      let record_extracted () =
        Hashtbl.add acc.by_key key pkg_name;
        acc.extracted <- pkg :: acc.extracted
      in
      if Sys.file_exists dst then record_extracted ()
      else
        match extract_main_source ~cache_urls ~dst pkg url checksums with
        | Ok () -> record_extracted ()
        | Error msg -> acc.failures <- (pkg_name, msg) :: acc.failures)

let install_sources ~cache_urls ~vendor_dir ~packages_dirs pkgs =
  (* [by_key] maps an archive's identity to the basename of the
     directory that hosts its extracted source. The first package to
     claim an archive owns the directory; later ones merely note the
     name they should pin against. *)
  let acc = new_install_acc () in
  List.iter
    (fun pkg ->
      match main_source ~packages_dirs pkg with
      | None -> acc.virtuals <- pkg :: acc.virtuals
      | Some (url, checksums) ->
          install_one ~cache_urls ~vendor_dir ~acc pkg url checksums)
    pkgs;
  {
    extracted = List.rev acc.extracted;
    duplicates = List.rev acc.duplicates;
    virtuals = List.rev acc.virtuals;
    failures = List.rev acc.failures;
  }

(* -- workspace files ----------------------------------------------------

   [root.opam] drives [oi build]: [depends:] names externally-resolved
   packages (non-dune sources and virtuals) at their solved versions;
   dune-buildable in-tree packages are omitted because dune builds them
   from [vendor/]. [x-repos:] and [x-reporepo-hash:] stamp the reporepo
   for reproducible re-solves.

   [dune-project] is the workspace marker; the root [dune] file's
   [(vendored_dirs vendor)] tells dune to relax warnings-as-errors and
   strict project checks for the extracted sources. *)

let dune_project_contents = "(lang dune 3.20)\n"
let dune_root_contents = "(vendored_dirs vendor)\n"

let render_root_opam ~target_label ~packages ~repos ~reporepo_hash =
  let buf = Buffer.create 1024 in
  let pf fmt = Fmt.kstr (Buffer.add_string buf) fmt in
  pf "opam-version: \"2.0\"\n";
  pf "synopsis: \"oi source bundle for %s\"\n" target_label;
  pf "depends: [\n";
  List.iter
    (fun pkg ->
      pf "  %S {= %S}\n"
        (OpamPackage.Name.to_string (OpamPackage.name pkg))
        (OpamPackage.Version.to_string (OpamPackage.version pkg)))
    packages;
  pf "]\n";
  if repos <> [] then begin
    pf "x-repos: [\n";
    List.iter (fun tok -> pf "  %S\n" tok) repos;
    pf "]\n"
  end;
  Stdlib.Option.iter
    (fun sha -> pf "%s: %S\n" Oi.Keys.reporepo_hash sha)
    reporepo_hash;
  Buffer.contents buf

let write_workspace_files ~output ~target_label ~packages ~repos ~reporepo_hash
    =
  let write path content =
    Out_channel.with_open_bin path (fun oc ->
        Out_channel.output_string oc content)
  in
  write (output / "dune-project") dune_project_contents;
  write (output / "dune") dune_root_contents;
  write (output / "root.opam")
    (render_root_opam ~target_label ~packages ~repos ~reporepo_hash)

(* -- Local mirror warm-up ------------------------------------------------

   The actual [pull_tree] extraction reads from opam's [cache_dir] /
   the local mirror; warm both up-front via the same
   [Source.Mirror.fetch_archives] [oi build --archives-only] uses, so
   extractions afterwards are pure CPU + disk. *)
let warm_local_mirror ~fs ~cache ~packages_dirs pkgs =
  let archives = Oi.Source.Mirror.collect_archives ~packages_dirs pkgs in
  if archives = [] then None
  else begin
    Oi.Say.step "Fetching %d source archive(s)" (List.length archives);
    let last = ref "" in
    let on_progress ~fetched ~total ~current =
      let msg =
        match current with
        | None -> Fmt.str "fetched %d/%d" fetched total
        | Some c -> Fmt.str "fetched %d/%d  %s" fetched total c
      in
      if msg <> !last then begin
        last := msg;
        Oi.Say.progress msg
      end
    in
    let summary =
      Oi.Source.Mirror.fetch_archives ~fs ~cache ~on_progress archives
    in
    Oi.Say.progress_clear ();
    Some summary
  end

(* -- @handle/pkg shorthand ---------------------------------------------- *)

(* Strip [@handle/pkg] prefixes the same way [oi build] / [oi run] do:
   the handle joins [with_repos] (overlay shows up in the solve), the
   bare [pkg] takes its place in [targets], and the pkg-with-version
   spec joins [with_deps] for constraint propagation. *)
let split_handle_targets ~with_repos ~with_deps targets =
  let targets, with_repos, with_deps =
    List.fold_left
      (fun (ts, repos, deps) t ->
        match Target.split_handle_prefix t with
        | None -> (t :: ts, repos, deps)
        | Some (h, pkg_spec) ->
            let pkg, _ = OpamFormula.atom_of_string pkg_spec in
            ( OpamPackage.Name.to_string pkg :: ts,
              repos @ [ h ],
              deps @ [ pkg_spec ] ))
      ([], with_repos, with_deps)
      targets
  in
  (List.rev targets, with_repos, with_deps)

(* -- Main flow ---------------------------------------------------------- *)

(* [Eio.Path.mkdirs] mishandles relative paths whose split-parent is the
   empty string (it ends up calling [mkdirat] with [""]). Resolve to
   absolute against cwd before any filesystem op. *)
let absolutize_output output =
  if Filename.is_relative output then Filename.concat (Sys.getcwd ()) output
  else output

(* Create the bundle's three top-level dirs and return their absolute
   paths. *)
let prepare_output_dirs ~fs output =
  let output = absolutize_output output in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / output);
  let vendor_dir = output / "vendor" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / vendor_dir);
  let bundle_repo = output / "reporepo" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / bundle_repo);
  (output, vendor_dir, bundle_repo)

type solve_inputs = {
  with_repos : string list;
  all_extras : Oi.Project.extra_repo list;
  extra_constraints : OpamFormula.version_constraint OpamPackage.Name.Map.t;
  url_project : Oi.Project.Url.t;
}

let collect_solve_inputs ~fs ~sys ~cache ~refresh ~with_repos ~with_deps
    ~(project : Oi.Project.t) =
  let extra_deps, url_project =
    Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh with_deps
  in
  let extra_constraints = Oi.Project.Script.constraints extra_deps in
  let with_repos =
    with_repos
    @ Oi.Pipeline.filter_compatible_overlays
        ~reporepo_path:(Terms.reporepo_path ()) ~toolchain:None
        (project.overlays @ url_project.overlays)
  in
  let cli_extras = Target.cli_extra_repos ~fs ~sys ?toolchain:None with_repos in
  let all_extras =
    Target.merge_extras ~cli:cli_extras
      ~project:(project.extra_repos @ url_project.extra_repos)
  in
  { with_repos; all_extras; extra_constraints; url_project }

let toolchain_packages_dirs ~fs ~sys ~data_dir = function
  | Some (info : Oi.Toolchain.info) -> info.packages_dirs
  | None -> Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ()

let discover_packages_dirs ~fs ~sys ~cache ~refresh ~data_dir
    ~(project : Oi.Project.t) ~(url_project : Oi.Project.Url.t) ~all_extras
    ~toolchain =
  Stdlib.Option.to_list project.packages_dir
  @ Stdlib.Option.to_list
      (Oi.Source.Pin.materialize ~fs ~sys ~cache ~refresh
         (project.pins @ url_project.pins))
  @ Oi.Source.Repo.ensure_many ~fs ~data_dir ~refresh all_extras
  @ toolchain_packages_dirs ~fs ~sys ~data_dir toolchain

(* Enable {with-test} / {with-doc} for every known package, not
   just the solve roots: the bundle's [dune build] compiles
   in-tree tests of all consumer packages, so their test deps
   need to land in the closure. The set is a name catalogue —
   over-listing is harmless since the filter only fires for
   packages the solver actually visits. *)
let test_doc_universe_of packages_dirs =
  packages_dirs
  |> List.concat_map (fun d ->
      try
        Sys.readdir d |> Array.to_list
        |> List.filter (fun n -> n <> "" && n.[0] <> '.')
      with Sys_error _ -> [])
  |> List.filter_map (fun n ->
      try Some (OpamPackage.Name.of_string n) with Failure _ -> None)
  |> OpamPackage.Name.Set.of_list

let solve_or_fail ~ctx ~sys ~fs ~cache_root ~packages_dirs ~names
    ~extra_constraints =
  let test_doc_universe = test_doc_universe_of packages_dirs in
  match
    Oi.Solver.solve ~test:test_doc_universe ~doc:test_doc_universe ~sys ~fs
      ~cache_root ctx ~packages_dirs ~constraints:extra_constraints names
  with
  | Ok pkgs -> pkgs
  | Error msg -> Oi.Error.no_solution msg

(* Drop dune-buildable in-tree packages from [depends:] — dune
   builds them from [vendor/] directly. Non-dune sources,
   virtuals, and packages whose source we didn't extract stay
   in [depends:] for [oi build] to resolve via the reporepo. *)
let external_packages ~install ~vendor_dir consumer_pkgs =
  let dirs = dir_of_pkg install in
  let needs_external_resolution pkg =
    let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
    match Hashtbl.find_opt dirs name with
    | None -> true
    | Some dir -> not (dir_is_dune_buildable (vendor_dir / dir))
  in
  consumer_pkgs
  |> List.filter needs_external_resolution
  |> List.sort (fun a b ->
      OpamPackage.Name.compare (OpamPackage.name a) (OpamPackage.name b))

(* Normalise [with_repos] to [x-repos:] tokens: URLs (anything with a
   [:]) pass through; bare names get an [@] prefix. *)
let normalize_x_repos with_repos =
  with_repos
  |> List.filter_map (fun tok ->
      if tok = "" then None
      else if String.contains tok ':' || tok.[0] = '@' then Some tok
      else Some ("@" ^ tok))
  |> List.sort_uniq String.compare

let current_reporepo_hash ~sys =
  let path = Terms.reporepo_path () in
  if not (Sys.file_exists (path / ".git")) then None
  else
    try
      Some
        (D10.Sysops.Cmd.run_out sys [ "git"; "-C"; path; "rev-parse"; "HEAD" ])
    with Eio.Exn.Io _ | Failure _ -> None

let report_fetch_failures = function
  | None -> ()
  | Some (s : Oi.Source.Mirror.fetch_summary) ->
      List.iter
        (fun (url, msg) -> Oi.Say.warn "fetch failed %s: %s" url msg)
        s.failed

let report_install (install : install) ~vendor_dir =
  Oi.Say.field "sources" "%d extracted, %d shared, %d virtual → %s"
    (List.length install.extracted)
    (List.length install.duplicates)
    (List.length install.virtuals)
    vendor_dir;
  List.iter
    (fun (pkg, msg) -> Oi.Say.warn "extract %s: %s" pkg msg)
    install.failures

let short_sha = function
  | None -> "(none)"
  | Some s -> String.sub s 0 (min 12 (String.length s))

(* Resolve the initial CLI / project state into the inputs the solver
   needs. Pure plumbing — separated out so the main flow stays linear. *)
let resolve_initial_inputs ~fs ~cwd_s ~targets ~with_repos ~with_deps =
  let project = Oi.Project.load ~fs cwd_s in
  let targets = if targets = [] then project.deps else targets in
  if targets = [] then
    Oi.Error.fail_config_error
      "oi source: at least one TARGET is required (or run from a project dir \
       with *.opam files)";
  let targets, with_repos, with_deps =
    split_handle_targets ~with_repos ~with_deps targets
  in
  (project, targets, with_repos, with_deps)

(* Outputs of the solve stage that downstream steps consume. *)
type solved = {
  consumer_pkgs : OpamPackage.t list;
  packages_dirs : string list;
  inputs : solve_inputs;
}

(* Solve the dep closure for [targets]. Bundles the toolchain pick,
   packages-dirs assembly, and root-name massaging so the main flow is
   linear. *)
let run_solve ~harness ~refresh ~with_repos ~with_deps ~toolchain_override
    ~targets ~data_dir ~(project : Oi.Project.t) =
  let { Harness.fs; sys; platform; cache; _ } = harness in
  let conf =
    Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  let inputs =
    collect_solve_inputs ~fs ~sys ~cache ~refresh ~with_repos ~with_deps
      ~project
  in
  let toolchain =
    Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf ~install:false
      ~override:toolchain_override
      ~handles:(List.sort_uniq String.compare inputs.with_repos)
      ()
  in
  let conf, tc_ctx = Oi.Pipeline.solver_inputs toolchain conf in
  let packages_dirs =
    discover_packages_dirs ~fs ~sys ~cache ~refresh ~data_dir ~project
      ~url_project:inputs.url_project ~all_extras:inputs.all_extras ~toolchain
  in
  let cache_root = Oi.Cache.root_s cache in
  let ctx =
    Oi.Solver.Ctx.create
      ~prefix:(cache_root / "build" / "prefix")
      ~packages_dirs ~conf ?toolchain:tc_ctx ()
  in
  let names =
    List.map OpamPackage.Name.of_string targets
    @ List.map OpamPackage.Name.of_string inputs.url_project.roots
    |> Oi.Pipeline.strip_compiler_roots_for_override
         ~override:toolchain_override ~toolchain
  in
  Oi.Say.step "Solving %d target(s) (+test, +doc)" (List.length names);
  let pkgs =
    solve_or_fail ~ctx ~sys ~fs ~cache_root ~packages_dirs ~names
      ~extra_constraints:inputs.extra_constraints
  in
  let consumer_pkgs, toolchain_pkgs = partition_toolchain ~toolchain pkgs in
  Oi.Say.info "%d package(s) in solve closure (%d toolchain)"
    (List.length consumer_pkgs)
    (List.length toolchain_pkgs);
  ignore cache_root;
  { consumer_pkgs; packages_dirs; inputs }

(* After sources are installed, write [root.opam] / [dune-project] /
   [dune] and emit the final summary line. Returns the install result so
   the caller can propagate the failure exit code. *)
let finalize_workspace ~sys ~output ~vendor_dir ~targets ~(solved : solved)
    ~(install : install) =
  let packages = external_packages ~install ~vendor_dir solved.consumer_pkgs in
  let repos = normalize_x_repos solved.inputs.with_repos in
  let reporepo_hash = current_reporepo_hash ~sys in
  write_workspace_files ~output
    ~target_label:(String.concat ", " targets)
    ~packages ~repos ~reporepo_hash;
  Oi.Say.field "workspace"
    "%d external dep(s) (of %d in solve), %d x-repos, reporepo-hash %s"
    (List.length packages)
    (List.length solved.consumer_pkgs)
    (List.length repos) (short_sha reporepo_hash);
  Oi.Say.ok "wrote source bundle to %s" output

let run_bundle ~harness ~refresh ~with_repos ~with_deps ~toolchain_override
    ~targets ~output ~data_dir =
  let { Harness.fs; sys; cache; _ } = harness in
  if output = "" then Oi.Error.fail_config_error "oi source: -o DIR is required";
  let cwd_s, _ = Workspace.resolved_cwd fs in
  let project, targets, with_repos, with_deps =
    resolve_initial_inputs ~fs ~cwd_s ~targets ~with_repos ~with_deps
  in
  let output, vendor_dir, bundle_repo = prepare_output_dirs ~fs output in
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
  let solved =
    run_solve ~harness ~refresh ~with_repos ~with_deps ~toolchain_override
      ~targets ~data_dir ~project
  in
  report_fetch_failures
    (warm_local_mirror ~fs ~cache ~packages_dirs:solved.packages_dirs
       solved.consumer_pkgs);
  let cache_urls = [ Oi.Source.Mirror.url ~cache ] in
  let install =
    install_sources ~cache_urls ~vendor_dir ~packages_dirs:solved.packages_dirs
      solved.consumer_pkgs
  in
  report_install install ~vendor_dir;
  let n_repo =
    copy_reporepo_subset ~packages_dirs:solved.packages_dirs ~bundle_repo
      solved.consumer_pkgs
  in
  Oi.Say.field "reporepo" "%d package dir(s) → %s" n_repo bundle_repo;
  finalize_workspace ~sys ~output ~vendor_dir ~targets ~solved ~install;
  if install.failures <> [] then exit 1

let man_block =
  [
    `S Manpage.s_description;
    `P
      "Solve $(b,TARGET)'s dep closure (including $(b,with-test) and \
       $(b,with-doc)), fetch every source archive, and lay them out under \
       $(b,DIR) as a vendored workspace. Hand off to $(b,oi build), $(b,dune \
       pkg lock), or $(b,opam install --deps-only).";
    `S "OUTPUT LAYOUT";
    `I
      ( "$(b,DIR/vendor/<pkg>/)",
        "Per-package source tree. Tarballs unpacked, $(b,git+) URLs cloned at \
         the pinned commit. Packages sharing an archive share a directory." );
    `I
      ( "$(b,DIR/reporepo/)",
        "Snapshot of the opam metadata the solve consulted." );
    `I
      ( "$(b,DIR/root.opam)",
        "Bundle manifest. $(b,depends:) pins externally-resolved packages; \
         dune-buildable in-tree packages build from $(b,vendor/). \
         $(b,x-repos:) and $(b,x-reporepo-hash:) stamp the reporepo." );
    `I
      ( "$(b,DIR/dune-project), $(b,DIR/dune)",
        "Workspace marker; $(b,(vendored_dirs vendor)) relaxes \
         warnings-as-errors over $(b,vendor/)." );
    `S "NOTES";
    `P "$(b,extra-sources:) blocks are skipped.";
    `P "Toolchain packages are pinned via $(b,--toolchain), not $(b,depends:).";
    `P
      "For a portable, $(b,oi)-free build from registry archives see $(b,oi \
       dist makefile).";
    `S Manpage.s_examples;
    `Pre
      "  oi source @avsm/owntracks-cli -o ./bundle\n\
      \  oi source dune fmt -o /tmp/dune-fmt-src\n\
      \  oi source @avsm/oi --toolchain ocaml-5.4 -o ./oi-src";
  ]

let cmd =
  let run (c : Terms.common) refresh _registry _use_registry with_repos
      with_deps _jobs toolchain_override targets output =
    Harness.run @@ fun ~sw env ->
    let harness =
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    run_bundle ~harness ~refresh ~with_repos ~with_deps ~toolchain_override
      ~targets ~output ~data_dir:c.data_dir
  in
  let output =
    Arg.(
      value & opt string ""
      & info ~docv:"DIR"
          ~doc:
            "Output directory (required). Created if missing; existing entries \
             are kept."
          [ "o"; "output" ])
  in
  let targets =
    Arg.(
      value & pos_all string []
      & info ~docv:"TARGET" ~doc:"Package name or $(b,@HANDLE/PKG). Repeatable."
          [])
  in
  let info =
    Cmd.info "source"
      ~doc:"Vendor a target's sources into a self-contained bundle"
      ~man:man_block
  in
  Cmd.v info
    Term.(
      const run $ Terms.common $ Terms.refresh $ Terms.registry
      $ Terms.use_registry $ Terms.with_repos $ Terms.with_deps $ Terms.jobs
      $ Terms.toolchain $ targets $ output)
