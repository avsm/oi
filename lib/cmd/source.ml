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

(* Recursive copy of [src] into [dst], hardlinking regular files when
   possible (cross-fs gracefully falls back to byte copy). Used to
   snapshot a reporepo [<pkg>/<pkg.ver>/] directory — that tree
   carries the [opam] file plus any [files/] subdir holding
   [extra-files:] patches. *)
let rec copy_tree src dst =
  Sys.readdir src
  |> Array.iter (fun name ->
      let s = src / name and d = dst / name in
      match (Unix.lstat s).Unix.st_kind with
      | Unix.S_DIR ->
          (try Unix.mkdir d 0o755
           with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
          copy_tree s d
      | Unix.S_REG when not (Sys.file_exists d) -> (
          try Unix.link s d
          with Unix.Unix_error _ ->
            let i = open_in_bin s and o = open_out_bin d in
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
                loop ()))
      | Unix.S_LNK -> (
          let target = Unix.readlink s in
          try Unix.symlink target d
          with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
      | _ -> ())

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

let install_sources ~cache_urls ~vendor_dir ~packages_dirs pkgs =
  (* [by_key] maps an archive's identity to the basename of the
     directory that hosts its extracted source. The first package to
     claim an archive owns the directory; later ones merely note the
     name they should pin against. *)
  let by_key : (string, string) Hashtbl.t = Hashtbl.create 64 in
  let extracted = ref [] in
  let duplicates = ref [] in
  let virtuals = ref [] in
  let failures = ref [] in
  List.iter
    (fun pkg ->
      match main_source ~packages_dirs pkg with
      | None -> virtuals := pkg :: !virtuals
      | Some (url, checksums) -> (
          let key = archive_key url checksums in
          match Hashtbl.find_opt by_key key with
          | Some dir -> duplicates := (pkg, dir) :: !duplicates
          | None -> (
              let pkg_name =
                OpamPackage.Name.to_string (OpamPackage.name pkg)
              in
              let dst = vendor_dir / pkg_name in
              let record_extracted () =
                Hashtbl.add by_key key pkg_name;
                extracted := pkg :: !extracted
              in
              if Sys.file_exists dst then record_extracted ()
              else
                match
                  extract_main_source ~cache_urls ~dst pkg url checksums
                with
                | Ok () -> record_extracted ()
                | Error msg -> failures := (pkg_name, msg) :: !failures)))
    pkgs;
  {
    extracted = List.rev !extracted;
    duplicates = List.rev !duplicates;
    virtuals = List.rev !virtuals;
    failures = List.rev !failures;
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

let cmd =
  let run (c : Terms.common) refresh _registry _use_registry with_repos
      with_deps _jobs toolchain_override targets output =
    Harness.run @@ fun ~sw env ->
    let { Harness.fs; sys; platform; cache; _ } =
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    let data_dir = c.data_dir in
    if output = "" then
      Oi.Error.fail_config_error "oi source: -o DIR is required";
    (* Project mode: with no positional TARGET, fall back to the
       cwd's [*.opam] project — vendor the closure of its direct
       deps, the same trigger [oi build] uses. *)
    let cwd_s, _ = Workspace.resolved_cwd fs in
    let project = Oi.Project.load ~fs cwd_s in
    let targets = if targets = [] then project.deps else targets in
    if targets = [] then
      Oi.Error.fail_config_error
        "oi source: at least one TARGET is required (or run from a project \
         dir with *.opam files)";
    (* [Eio.Path.mkdirs] mishandles relative paths whose split-parent
       is the empty string (it ends up calling [mkdirat] with [""]).
       Resolve to absolute against cwd before any filesystem op. *)
    let output =
      if Filename.is_relative output then Filename.concat (Sys.getcwd ()) output
      else output
    in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / output);
    let vendor_dir = output / "vendor" in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / vendor_dir);
    let bundle_repo = output / "reporepo" in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / bundle_repo);
    Oi.Pipeline.init_opam_root ~fs ~data_dir;
    ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
    let conf =
      Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
    in
    let targets, with_repos, with_deps =
      split_handle_targets ~with_repos ~with_deps targets
    in
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
    let cli_extras =
      Target.cli_extra_repos ~fs ~sys ?toolchain:None with_repos
    in
    let all_extras =
      Target.merge_extras ~cli:cli_extras
        ~project:(project.extra_repos @ url_project.extra_repos)
    in
    let toolchain =
      Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf ~install:false
        ~override:toolchain_override
        ~handles:(List.sort_uniq String.compare with_repos)
        ()
    in
    let conf, tc_ctx = Oi.Pipeline.solver_inputs toolchain conf in
    let packages_dirs =
      Stdlib.Option.to_list project.packages_dir
      @ Stdlib.Option.to_list
          (Oi.Source.Pin.materialize ~fs ~sys ~cache ~refresh
             (project.pins @ url_project.pins))
      @ Oi.Source.Repo.ensure_many ~fs ~data_dir ~refresh all_extras
      @
      match toolchain with
      | Some (info : Oi.Toolchain.info) -> info.packages_dirs
      | None -> Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ()
    in
    let cache_root = Oi.Cache.root_s cache in
    let ctx =
      Oi.Solver.Ctx.create
        ~prefix:(cache_root / "build" / "prefix")
        ~packages_dirs ~conf ?toolchain:tc_ctx ()
    in
    let names =
      List.map OpamPackage.Name.of_string targets
      @ List.map OpamPackage.Name.of_string url_project.roots
      |> Oi.Pipeline.strip_compiler_roots_for_override
           ~override:toolchain_override ~toolchain
    in
    Oi.Say.step "Solving %d target(s) (+test, +doc)" (List.length names);
    (* Enable {with-test} / {with-doc} for every known package, not
       just the solve roots: the bundle's [dune build] compiles
       in-tree tests of all consumer packages, so their test deps
       need to land in the closure. The set is a name catalogue —
       over-listing is harmless since the filter only fires for
       packages the solver actually visits. *)
    let test_doc_universe =
      packages_dirs
      |> List.concat_map (fun d ->
          try
            Sys.readdir d |> Array.to_list
            |> List.filter (fun n -> n <> "" && n.[0] <> '.')
          with Sys_error _ -> [])
      |> List.filter_map (fun n ->
          try Some (OpamPackage.Name.of_string n) with Failure _ -> None)
      |> OpamPackage.Name.Set.of_list
    in
    let pkgs =
      match
        Oi.Solver.solve ~test:test_doc_universe ~doc:test_doc_universe ~sys
          ~fs ~cache_root ctx ~packages_dirs ~constraints:extra_constraints
          names
      with
      | Ok pkgs -> pkgs
      | Error msg -> Oi.Error.no_solution msg
    in
    let consumer_pkgs, toolchain_pkgs = partition_toolchain ~toolchain pkgs in
    Oi.Say.info "%d package(s) in solve closure (%d toolchain)"
      (List.length consumer_pkgs)
      (List.length toolchain_pkgs);
    (match warm_local_mirror ~fs ~cache ~packages_dirs consumer_pkgs with
    | None -> ()
    | Some s ->
        List.iter
          (fun (url, msg) -> Oi.Say.warn "fetch failed %s: %s" url msg)
          s.failed);
    let cache_urls = [ Oi.Source.Mirror.url ~cache ] in
    let install =
      install_sources ~cache_urls ~vendor_dir ~packages_dirs consumer_pkgs
    in
    Oi.Say.field "sources" "%d extracted, %d shared, %d virtual → %s"
      (List.length install.extracted)
      (List.length install.duplicates)
      (List.length install.virtuals)
      vendor_dir;
    List.iter
      (fun (pkg, msg) -> Oi.Say.warn "extract %s: %s" pkg msg)
      install.failures;
    let n_repo =
      copy_reporepo_subset ~packages_dirs ~bundle_repo consumer_pkgs
    in
    Oi.Say.field "reporepo" "%d package dir(s) → %s" n_repo bundle_repo;
    (* Drop dune-buildable in-tree packages from [depends:] — dune
       builds them from [vendor/] directly. Non-dune sources,
       virtuals, and packages whose source we didn't extract stay
       in [depends:] for [oi build] to resolve via the reporepo. *)
    let dirs = dir_of_pkg install in
    let needs_external_resolution pkg =
      let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
      match Hashtbl.find_opt dirs name with
      | None -> true
      | Some dir -> not (dir_is_dune_buildable (vendor_dir / dir))
    in
    let packages =
      consumer_pkgs
      |> List.filter needs_external_resolution
      |> List.sort (fun a b ->
          OpamPackage.Name.compare (OpamPackage.name a) (OpamPackage.name b))
    in
    (* Normalise [with_repos] to [x-repos:] tokens: URLs (anything
       with a [:]) pass through; bare names get an [@] prefix. *)
    let repos =
      with_repos
      |> List.filter_map (fun tok ->
          if tok = "" then None
          else if String.contains tok ':' || tok.[0] = '@' then Some tok
          else Some ("@" ^ tok))
      |> List.sort_uniq String.compare
    in
    let reporepo_hash =
      let path = Terms.reporepo_path () in
      if not (Sys.file_exists (path / ".git")) then None
      else
        try
          Some
            (D10.Sysops.Cmd.run_out sys
               [ "git"; "-C"; path; "rev-parse"; "HEAD" ])
        with Eio.Exn.Io _ | Failure _ -> None
    in
    write_workspace_files ~output
      ~target_label:(String.concat ", " targets)
      ~packages ~repos ~reporepo_hash;
    let short_sha = function
      | None -> "(none)"
      | Some s -> String.sub s 0 (min 12 (String.length s))
    in
    Oi.Say.field "workspace"
      "%d external dep(s) (of %d in solve), %d x-repos, reporepo-hash %s"
      (List.length packages)
      (List.length consumer_pkgs)
      (List.length repos) (short_sha reporepo_hash);
    Oi.Say.ok "wrote source bundle to %s" output;
    if install.failures <> [] then exit 1
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
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Solve $(b,TARGET)'s dep closure (including $(b,with-test) and \
             $(b,with-doc)), fetch every source archive, and lay them out \
             under $(b,DIR) as a vendored workspace. Hand off to $(b,oi \
             build), $(b,dune pkg lock), or $(b,opam install --deps-only).";
          `S "OUTPUT LAYOUT";
          `I
            ( "$(b,DIR/vendor/<pkg>/)",
              "Per-package source tree. Tarballs unpacked, $(b,git+) URLs \
               cloned at the pinned commit. Packages sharing an archive share \
               a directory." );
          `I
            ( "$(b,DIR/reporepo/)",
              "Snapshot of the opam metadata the solve consulted." );
          `I
            ( "$(b,DIR/root.opam)",
              "Bundle manifest. $(b,depends:) pins externally-resolved \
               packages; dune-buildable in-tree packages build from \
               $(b,vendor/). $(b,x-repos:) and $(b,x-reporepo-hash:) stamp the \
               reporepo." );
          `I
            ( "$(b,DIR/dune-project), $(b,DIR/dune)",
              "Workspace marker; $(b,(vendored_dirs vendor)) relaxes \
               warnings-as-errors over $(b,vendor/)." );
          `S "NOTES";
          `P "$(b,extra-sources:) blocks are skipped.";
          `P
            "Toolchain packages are pinned via $(b,--toolchain), not \
             $(b,depends:).";
          `S Manpage.s_examples;
          `Pre
            "  oi source @avsm/owntracks-cli -o ./bundle\n\
            \  oi source dune fmt -o /tmp/dune-fmt-src\n\
            \  oi source @avsm/oi --toolchain ocaml-5.4 -o ./oi-src";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.common $ Terms.refresh $ Terms.registry
      $ Terms.use_registry $ Terms.with_repos $ Terms.with_deps $ Terms.jobs
      $ Terms.toolchain $ targets $ output)
