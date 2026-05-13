let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.archive_builder"

module Log = (val Logs.src_log log_src : Logs.LOG)

type built = {
  path : string;
  sha256 : string;
  strip_components : int;
  subst_files : string list;
}

let archives_dir ~(d10 : D10.Config.t) =
  Eio.Path.native_exn d10.root / "d10ir" / "archives"

let tmp_dir ~(d10 : D10.Config.t) =
  Eio.Path.native_exn d10.root / "d10ir" / "tmp"

let sha256_of_file path =
  OpamHash.contents (OpamHash.compute ~kind:`SHA256 path)

(* Transient state that opam (or our own pipeline) leaves in the
   build_dir but that has no business being in the consolidated
   source archive. The hidden-file group ([.DS_Store], editor swap
   files, …) catches stray cruft that can land in a source tree if
   a user inspected the build dir locally before bake.

   [.git] is fully excluded — opam's git fetch records the wall-
   clock time of the fetch in [.git/logs/HEAD], so two bakes of the
   same url+commit produce different archive bytes. Dev packages
   that need [dune subst] to compute a [%%VERSION%%] placeholder are
   handled at archive-bake time by {!ensure_dune_project_version},
   which injects an opam-known version into [dune-project] before
   tar; see that function for the conditions under which it acts. *)
let exclude_patterns =
  [
    (* VCS metadata. *)
    ".git";
    ".hg";
    ".svn";
    (* build/cache dirs *)
    "_build";
    "_oi";
    ".opam-switch";
    ".merlin";
    "__pycache__";
    (* editor / OS cruft *)
    ".DS_Store";
    "*.swp";
    "*.swo";
    "*~";
    "*.bak";
    ".idea";
    ".vscode";
  ]

(* If the source URL is a git URL with a fragment (commit / branch /
   tag), return the fragment. Otherwise [None]. Used by
   {!ensure_dune_project_version} to suffix the injected version
   with a vcs-identifying tag, mirroring what [git describe] would
   produce if [.git] were present. *)
let git_fragment_of_url url_s =
  match OpamUrl.parse_opt ~handle_suffix:true url_s with
  | None -> None
  | Some u -> (
      match u.OpamUrl.backend with
      | `git -> (
          match u.OpamUrl.hash with Some h when h <> "" -> Some h | _ -> None)
      | _ -> None)

(* Sanitise a fragment so it's safe inside a dune-project [(version
   "...")] string and short enough to be useful. For commit hashes,
   keep the first 7 hex chars (git short form); for branch/tag names,
   keep at most 32 chars and replace anything outside [A-Za-z0-9._-]
   with [-]. *)
let short_vcs_tag s =
  let is_hex c =
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
  in
  let is_pure_hex = String.for_all is_hex s && String.length s >= 7 in
  if is_pure_hex then String.sub s 0 (min 7 (String.length s))
  else
    let buf = Buffer.create (min 32 (String.length s)) in
    let n = min 32 (String.length s) in
    for i = 0 to n - 1 do
      let c = s.[i] in
      if
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
        || c = '.' || c = '_' || c = '-'
      then Buffer.add_char buf c
      else Buffer.add_char buf '-'
    done;
    Buffer.contents buf

(* Inject a [(version "...")] field into [dune-project] when:
   - [build_dir/dune-project] exists (the package actually uses dune)
   - dune-project has no existing [(version ...)] field
   The injected version is the package's opam version, suffixed with
   [+<short-vcs-tag>] when the source URL is a git URL with a hash.

   Without this, [dune subst] errors out on dev-pinned packages whose
   source uses [%%VERSION%%] placeholders (because we strip [.git]
   from the archive for reproducibility). Doing this once at bake
   time keeps the build script clean — same effect as if the upstream
   dune-project had carried [(version)] in the first place. *)
let ensure_dune_project_version ~build_dir ~opam_version ~url_opt =
  let path = build_dir / "dune-project" in
  if not (Sys.file_exists path) then ()
  else
    let content =
      try In_channel.with_open_text path In_channel.input_all
      with Sys_error _ -> ""
    in
    let already_has_version =
      String.split_on_char '\n' content
      |> List.exists (String.starts_with ~prefix:"(version ")
    in
    if already_has_version then ()
    else
      let suffix =
        match Stdlib.Option.bind url_opt git_fragment_of_url with
        | None -> ""
        | Some frag -> "+" ^ short_vcs_tag frag
      in
      let v = opam_version ^ suffix in
      let injected = Fmt.str "\n(version \"%s\")\n" v in
      let new_content = content ^ injected in
      try
        Out_channel.with_open_text path (fun oc ->
            Out_channel.output_string oc new_content)
      with Sys_error _ -> ()

(* Bake requires GNU tar. Reporepo bumps must run on Linux where GNU
   tar is the default; macOS users with [gtar] from Homebrew can
   set [TAR=gtar] in the environment. The deterministic flags
   ([--sort=name], [--mtime=@0], [--owner=0], [--numeric-owner]) are
   not all available in BSD tar, and silently falling back changes
   the archive bytes — which would break the cross-machine
   portability invariant the IR relies on. *)
(* Parse [.gitmodules] for submodule paths so they can be excluded
   from the archive. opam's git fetch recurses submodules, which is
   fine for the build (the vendored copy ends up on disk) but breaks
   anything that also installs the submodule's library as a regular
   opam dep — dune sees two libraries with the same name and aborts
   (ocluster vendoring obuilder is the motivating case). Release
   tarballs ship with empty submodule dirs, so this exclusion makes
   git-pin bakes byte-equivalent to the release-tarball path.

   Format: GNU-style [.gitmodules] with [path = X] keys. We only need
   the [path] values; anything else (url, branch, …) is ignored. *)
let parse_gitmodules_path line =
  let line = String.trim line in
  match String.index_opt line '=' with
  | None -> None
  | Some i ->
      let k = String.trim (String.sub line 0 i) in
      let v =
        String.trim (String.sub line (i + 1) (String.length line - i - 1))
      in
      if k = "path" && v <> "" then Some v else None

let submodule_paths ~src_dir =
  let gm = Filename.concat src_dir ".gitmodules" in
  if not (Sys.file_exists gm) then []
  else
    try
      In_channel.with_open_text gm In_channel.input_lines
      |> List.filter_map parse_gitmodules_path
    with Sys_error _ -> []

let tar_zst ~proc_mgr ~src_dir ~dst_path =
  let submodule_excludes =
    List.map (fun p -> "./" ^ p) (submodule_paths ~src_dir)
  in
  let exclude_args =
    List.concat_map
      (fun p -> [ "--exclude"; p ])
      (exclude_patterns @ submodule_excludes)
  in
  let tar = match Sys.getenv_opt "TAR" with Some s -> s | None -> "tar" in
  let argv =
    [
      tar;
      "--zstd";
      "--sort=name";
      "--mtime=@0";
      "--owner=0";
      "--group=0";
      "--numeric-owner";
    ]
    @ exclude_args
    @ [ "-cf"; dst_path; "-C"; src_dir; "." ]
  in
  try Eio.Process.run proc_mgr argv
  with exn ->
    Fmt.failwith
      "tar failed for %s: %s\n\
       Bake requires GNU tar (the Linux default). On macOS install gtar via \
       Homebrew and set [TAR=gtar] in the environment, or run bumps on a Linux \
       host."
      src_dir (Printexc.to_string exn)

let pkg_version_of pkg_s =
  match String.index_opt pkg_s '.' with
  | None -> pkg_s
  | Some i -> String.sub pkg_s (i + 1) (String.length pkg_s - i - 1)

(* Stream [ic] into [oc] until EOF. Pulled out so [copy_file_bytes] stays
   flat (the [with_open_bin] callbacks would otherwise stack two nests). *)
let rec stream_ic_to_oc ~buf ic oc =
  let n = In_channel.input ic buf 0 (Bytes.length buf) in
  if n > 0 then begin
    Out_channel.output oc buf 0 n;
    stream_ic_to_oc ~buf ic oc
  end

(* Cross-fs fallback when [Sys.rename] fails: stream bytes from [src] to [dst]. *)
let copy_file_bytes ~src ~dst =
  let buf = Bytes.create 65536 in
  In_channel.with_open_bin src (fun ic ->
      Out_channel.with_open_bin dst (fun oc -> stream_ic_to_oc ~buf ic oc))

let install_or_drop_tmp ~tmp_path ~final_path =
  if not (Sys.file_exists final_path) then
    try Sys.rename tmp_path final_path
    with Sys_error _ -> copy_file_bytes ~src:tmp_path ~dst:final_path
  else try Sys.remove tmp_path with Sys_error _ -> ()

let warn_archive_divergence ~p sha =
  match p.Plan.d10_archive with
  | Some declared when declared <> sha ->
      Log.warn (fun m ->
          m
            "x-d10-archive divergence for %s: declared %s, regenerated %s. \
             Re-run [oi ir bake @%s/<pkg>] to update the opam file."
            p.pkg (String.sub declared 0 12) (String.sub sha 0 12)
            (match p.overlay with Some o -> o.handle | None -> "<handle>"))
  | _ -> ()

(* [x-d10-archive] short-circuit: when the opam file declares a pre-baked
   source sha and the archive is locally present, reuse it directly. Missing
   archives fall through to the regular fetch path; the regenerated archive
   should hash to the same sha for deterministic builds. *)
let archive_hit_for ~d10 (p : Plan.package_plan) =
  Stdlib.Option.bind p.d10_archive (fun sha ->
      let path = archives_dir ~d10 / Fmt.str "%s.tar.zst" sha in
      if Sys.file_exists path then Some (sha, path) else None)

(* Source fetch + patch + extra-files + version-injection pipeline. Same
   sequence [Execute] runs for the live build, just up to tar time. *)
let prepare_build_dir ~fs ~cache_root ~cache_urls (p : Plan.package_plan) =
  (* Clean prior build_dir so patches don't run twice; [fetch_source]
     short-circuits when [build_dir] exists. We deliberately don't apply opam
     [substs:] here — the archive ships raw [.in] files for portability. *)
  Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / p.build_dir);
  Execute.fetch_phase ~cache_urls ~fs ~cache_root p;
  Execute.copy_extra_files p;
  Execute.apply_patches p;
  let pkg_version = pkg_version_of p.pkg in
  let url_opt =
    Stdlib.Option.map (fun (s : Plan.source_info) -> s.url) p.source
  in
  ensure_dune_project_version ~build_dir:p.build_dir ~opam_version:pkg_version
    ~url_opt

let bake_archive ~proc_mgr ~d10 (p : Plan.package_plan) =
  let layer_short =
    if String.length p.layer_hash >= 8 then String.sub p.layer_hash 0 8
    else p.layer_hash
  in
  let tmp_path = tmp_dir ~d10 / Fmt.str "%s-%s.tar.zst" p.pkg layer_short in
  (try Sys.remove tmp_path with Sys_error _ -> ());
  tar_zst ~proc_mgr ~src_dir:p.build_dir ~dst_path:tmp_path;
  let sha = sha256_of_file tmp_path in
  warn_archive_divergence ~p sha;
  let final_path = archives_dir ~d10 / Fmt.str "%s.tar.zst" sha in
  install_or_drop_tmp ~tmp_path ~final_path;
  Log.debug (fun m -> m "archive %s -> %s" p.pkg final_path);
  (sha, final_path)

let build ?(reporter = Build_progress.null) ~proc_mgr ~fs ~d10 ~cache_root
    ?(cache_urls = []) (p : Plan.package_plan) =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / archives_dir ~d10);
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / tmp_dir ~d10);
  Fmt.kstr
    (fun s -> reporter.Build_progress.event (Status s))
    "Baking archive for %s" p.pkg;
  match archive_hit_for ~d10 p with
  | Some (sha, final_path) ->
      Log.debug (fun m -> m "archive %s (x-d10-archive) -> %s" p.pkg final_path);
      { path = final_path; sha256 = sha; strip_components = 0;
        subst_files = p.substs }
  | None ->
      prepare_build_dir ~fs ~cache_root ~cache_urls p;
      let sha, final_path = bake_archive ~proc_mgr ~d10 p in
      { path = final_path; sha256 = sha; strip_components = 0;
        subst_files = p.substs }

(* -- No-solve bake ------------------------------------------------------ *)

(* The fields we extract directly from an opam file to drive a bake
   without going through the solver. Mirrors a stripped-down
   [Plan.package_plan] but without [subst_vars] (which we synthesise),
   [build_commands], [install_commands], or any solve-derived state. *)
type bake_inputs = {
  pkg : string;
  build_dir : string;
  source : Plan.source_info option;
  extra_sources : (string * Plan.source_info) list;
  extra_files : (string * string) list;
  patches : Plan.patch list;
  substs : Plan.subst list;
}

(* Read an opam file and produce the bake inputs. Patches are
   filter-evaluated against a minimal platform_env derived from
   [conf]. extra_files are looked up under [pkg_dir/files/]. *)
let inputs_of_opam_file ~platform_env ~name ~version ~build_dir ~pkg_dir
    opam_path =
  let opam_file = OpamFile.make (OpamFilename.raw opam_path) in
  let opam = OpamFile.OPAM.read opam_file in
  let source =
    OpamFile.OPAM.url opam
    |> Stdlib.Option.map (fun urlf ->
        let url = OpamUrl.to_string (OpamFile.URL.url urlf) in
        let checksums =
          List.map OpamHash.to_string (OpamFile.URL.checksum urlf)
        in
        Plan.{ url; checksums })
  in
  let extra_sources =
    List.map
      (fun (basename, urlf) ->
        let n = OpamFilename.Base.to_string basename in
        let url = OpamUrl.to_string (OpamFile.URL.url urlf) in
        let checksums =
          List.map OpamHash.to_string (OpamFile.URL.checksum urlf)
        in
        (n, Plan.{ url; checksums }))
      (OpamFile.OPAM.extra_sources opam)
  in
  let extra_files =
    match OpamFile.OPAM.extra_files opam with
    | None -> []
    | Some xs ->
        List.filter_map
          (fun (base, _hash) ->
            let basename = OpamFilename.Base.to_string base in
            let src =
              Filename.concat (Filename.concat pkg_dir "files") basename
            in
            if Sys.file_exists src then Some (basename, src) else None)
          xs
  in
  let patches =
    List.filter_map
      (fun (base, filter) ->
        let keep =
          match filter with
          | None -> true
          | Some f -> OpamFilter.eval_to_bool ~default:false platform_env f
        in
        if keep then
          let file = OpamFilename.Base.to_string base in
          let filter_str = Stdlib.Option.map OpamFilter.to_string filter in
          Some Plan.{ file; filter = filter_str }
        else None)
      (OpamFile.OPAM.patches opam)
  in
  let substs =
    List.map OpamFilename.Base.to_string (OpamFile.OPAM.substs opam)
  in
  let pkg_str = Fmt.str "%s.%s" name version in
  {
    pkg = pkg_str;
    build_dir;
    source;
    extra_sources;
    extra_files;
    patches;
    substs;
  }

(* Reuses [Execute.{fetch_phase,copy_extra_files,apply_patches}] by
   building a minimal [Plan.package_plan] adapter — all those helpers
   only read source/extras/files/patches/build_dir. We avoid
   [Execute.apply_substs] (which needs [subst_vars]) and instead use
   our local [apply_substs_synth]. *)
let to_min_plan ~prefix (b : bake_inputs) : Plan.package_plan =
  {
    pkg = b.pkg;
    opam =
      OpamFile.OPAM.create
        (OpamPackage.create
           (OpamPackage.Name.of_string "synthetic")
           (OpamPackage.Version.of_string "0"));
    layer_hash = "";
    method_ = Identity.Source;
    dep_layers = [];
    source = b.source;
    extra_sources = b.extra_sources;
    extra_files = b.extra_files;
    patches = b.patches;
    substs = b.substs;
    subst_vars = [];
    build_commands = [];
    install_commands = [];
    install_file = "";
    env = [||];
    build_dir = b.build_dir;
    prefix;
    overlay = None;
    opam_path = None;
    pkgs_dir = None;
    depexts = [];
    d10_archive = None;
  }

(* Bake one opam file into a content-addressed [.tar.zst] under
   [<d10.root>/d10ir/archives/]. Solver-free: takes only an opam file
   path, package identity, and a platform_env for filter evaluation.
   Designed to be called per-package by [oi repo bump] right after
   materialise_handle, so a single bump produces every overlay
   archive in one pass without going near the solver. *)
(* A minimal opam filter env for evaluating [{os = "macos"}] /
   [{arch = "arm64"}] etc. Only resolves the well-known platform
   variables; any solver/dep-derived variable returns None. Same set
   that [Solver.Ctx.platform_env] populates, but constructed without
   instantiating an opam state. *)
let bake_platform_env ~(platform : Osrel.t) =
  let os_str = Osrel.OS.os_to_string platform.os.kind in
  let os_distribution = Osrel.OS.kind_to_string platform.os.kind in
  let arch_str = Osrel.Arch.to_string platform.arch in
  fun var ->
    let key = OpamVariable.Full.to_string var in
    match key with
    | "os" -> Some (OpamTypes.S os_str)
    | "os-distribution" -> Some (OpamTypes.S os_distribution)
    | "os-family" -> Some (OpamTypes.S platform.os.family)
    | "arch" -> Some (OpamTypes.S arch_str)
    | _ -> None

let prepare_no_solve_build_dir ~fs ~cache_root ~cache_urls ~b ~build_dir
    ~version =
  Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / build_dir);
  let prefix = Filename.concat cache_root (Filename.concat "build" "prefix") in
  let p = to_min_plan ~prefix b in
  Execute.fetch_phase ~cache_urls ~fs ~cache_root p;
  Execute.copy_extra_files p;
  Execute.apply_patches p;
  (* No apply_substs at bake time. Archive contains raw .in files; [D10ir.Direct]
     applies substs at build time so archive content is independent of any
     per-machine path values. *)
  let url_opt =
    Stdlib.Option.map (fun (s : Plan.source_info) -> s.url) b.source
  in
  ensure_dune_project_version ~build_dir ~opam_version:version ~url_opt

let bake_no_solve_archive ~proc_mgr ~d10 ~name ~version ~build_dir =
  let tmp_path = tmp_dir ~d10 / Fmt.str "%s.%s-bake.tar.zst" name version in
  (try Sys.remove tmp_path with Sys_error _ -> ());
  tar_zst ~proc_mgr ~src_dir:build_dir ~dst_path:tmp_path;
  let sha = sha256_of_file tmp_path in
  let final_path = archives_dir ~d10 / Fmt.str "%s.tar.zst" sha in
  install_or_drop_tmp ~tmp_path ~final_path;
  (sha, final_path)

let build_no_solve ?(reporter = Build_progress.null) ~proc_mgr ~fs ~d10
    ~cache_root ?(cache_urls = []) ~platform ~name ~version ~pkg_dir ~opam_path
    () =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / archives_dir ~d10);
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / tmp_dir ~d10);
  Fmt.kstr
    (fun s -> reporter.Build_progress.event (Status s))
    "Baking archive for %s.%s" name version;
  let platform_env = bake_platform_env ~platform in
  let build_dir =
    Filename.concat cache_root
      (Fmt.kstr (Filename.concat "build/_build") "%s.%s-bake" name version)
  in
  let b =
    inputs_of_opam_file ~platform_env ~name ~version ~build_dir ~pkg_dir
      opam_path
  in
  prepare_no_solve_build_dir ~fs ~cache_root ~cache_urls ~b ~build_dir ~version;
  let sha, final_path =
    bake_no_solve_archive ~proc_mgr ~d10 ~name ~version ~build_dir
  in
  Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / build_dir);
  Log.debug (fun m -> m "no-solve bake %s -> %s" b.pkg final_path);
  {
    path = final_path;
    sha256 = sha;
    strip_components = 0;
    subst_files = b.substs;
  }
