open Cmdliner

let log_src = Logs.Src.create "oi.cmd.docker" ~doc:"oi docker command"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat
let err_msg fmt = Fmt.kstr (fun s -> Error (`Msg s)) fmt

(* [--distro=NAME] parsed via dockerfile-opam. Defaults to debian-stable. *)
let parse_distro tag =
  match Dockerfile_opam.Distro.distro_of_tag tag with
  | Some d -> Ok d
  | None -> err_msg "unknown distro tag: %s" tag
  | exception _ -> err_msg "unknown distro tag: %s" tag

let pp_distro ppf d =
  Fmt.string ppf
    (Dockerfile_opam.Distro.tag_of_distro
       (Dockerfile_opam.Distro.resolve_alias d :> Dockerfile_opam.Distro.t))

let distro_tag d =
  Dockerfile_opam.Distro.tag_of_distro
    (Dockerfile_opam.Distro.resolve_alias d :> Dockerfile_opam.Distro.t)

(* Per-distro list driving [--all]. Sized so each release CI run produces
   the canonical registry tree. *)
let default_distros : Registry_docker.Distro.t list =
  [
    `Alpine `Latest;
    `Debian `Stable;
    `Ubuntu `V24_04;
    `Ubuntu `V26_04;
    `Fedora `Latest;
  ]

(* Project-mode emit: one Dockerfile that runs [cmd] (e.g. [oi build]
   or [oi test]) inside [distro]. Filename encodes both the action and
   the distro so multiple variants coexist in the same dir. *)
let emit_project ~cmd ~suffix ~tag_label ~generator ~cwd_s ~distro ~output =
  let tag = distro_tag distro in
  let path =
    match output with
    | Some p -> p
    | None -> cwd_s / Fmt.str "Dockerfile.oi-%s.%s" suffix tag
  in
  let df = Registry_docker.dockerfile_project ~cmd ~generator distro in
  Registry_docker.write_dockerfile path df;
  Oi.Say.step "Wrote %s" path;
  Oi.Say.info "build with: docker build -t %s -f %s %s" tag_label path cwd_s

(* Probe each target distro for overlay depexts, returning [(distro, depexts)].
   Failures degrade to empty depext lists with a warning, so emit never blocks
   on a broken probe (e.g. offline). *)
let compute_per_distro_depexts ~fs ~sys ~cache ~data_dir ~refresh ~platform =
  try
    Build.compute_overlay_depexts_per_distro ~fs ~sys ~cache ~data_dir ~refresh
      ~platform ~distros:default_distros
  with exn ->
    Log.warn (fun m ->
        m "overlay depext probe failed: %s; falling back to no depexts"
          (Printexc.to_string exn));
    List.map (fun d -> (d, [])) default_distros

(* Which artefact this invocation emits. Docker and obuilder share the
   whole solve / depext / per-distro pipeline; only the per-distro file
   (Dockerfile vs obuilder spec) and the [--all] extras differ. *)
type kind = Docker | Obuilder

let kind_name = function Docker -> "docker" | Obuilder -> "obuilder"

(* For each target distro, write the per-distro artefact for [kind] and
   return [(distro, path, n_overlay_depexts)] for the summary. *)
let write_per_distro_files ~kind ~output ~s3 ~per_distro_depexts =
  List.map
    (fun d ->
      let overlay_depexts =
        Stdlib.Option.value (List.assoc_opt d per_distro_depexts) ~default:[]
      in
      let path =
        match kind with
        | Docker ->
            let p = output / Registry_docker.one_distro_filename d in
            Registry_docker.write_dockerfile p
              (Registry_docker.dockerfile_one_distro ~s3 ~overlay_depexts d);
            p
        | Obuilder ->
            let p = output / Registry_docker.one_distro_spec_filename d in
            Registry_docker.write_file p
              (Registry_docker.obuilder_spec_one_distro ~s3 ~overlay_depexts d);
            p
      in
      (d, path, List.length overlay_depexts))
    default_distros

let print_per_distro_paths per_distro_paths =
  List.iter
    (fun (_, path, n) ->
      let suffix = if n = 0 then "" else Fmt.str "  (%d overlay depexts)" n in
      Oi.Say.info "%s%a" path Oi.Style.pp_dim_string suffix)
    per_distro_paths

(* Summary after [docker --all]: static-oi builder, per-distro
   Dockerfiles, and the docker-compose orchestration. *)
let print_docker_all_summary ~oi_path ~per_distro_paths ~compose_path =
  Oi.Say.step "Wrote";
  Oi.Say.info "%s" oi_path;
  print_per_distro_paths per_distro_paths;
  Oi.Say.info "%s" compose_path;
  Oi.Say.newline ();
  Oi.Say.step "Static oi release binary";
  Oi.Say.info "docker buildx build -f %s --output type=local,dest=./oi-bin ."
    oi_path;
  Oi.Say.step "Run the build + sync (Docker)";
  Oi.Say.info "S3_ACCESS_KEY=… S3_SECRET_KEY=… docker compose build   %a"
    Oi.Style.pp_dim_string
    "# builds each image, runs oi build --all and s3cmd put --recursive"

(* Summary after [obuilder --all]: just the per-distro specs — no
   docker-compose / static-builder image (those are Docker-only). *)
let print_obuilder_all_summary ~per_distro_paths =
  Oi.Say.step "Wrote obuilder specs";
  print_per_distro_paths per_distro_paths;
  match per_distro_paths with
  | (_, p, _) :: _ ->
      Oi.Say.newline ();
      Oi.Say.step "Build one";
      Oi.Say.info "obuilder build --store=… -f %s ." p
  | [] -> ()

(* Multi-distro registry build. Docker: [Dockerfile.oi] + per-distro
   [Dockerfile.<distro>] + [docker-compose.yml]. Obuilder: per-distro
   [<distro>.spec] only (no compose / static-builder image). *)
let emit_all ~kind ~fs ~sys ~platform ~cache ~data_dir ~refresh ~src_context ~s3
    ~output () =
  (try Unix.mkdir output 0o755 with Unix.Unix_error (EEXIST, _, _) -> ());
  Oi.Say.step "Computing overlay depexts for %d distros"
    (List.length default_distros);
  let per_distro_depexts =
    compute_per_distro_depexts ~fs ~sys ~cache ~data_dir ~refresh ~platform
  in
  let per_distro_paths =
    write_per_distro_files ~kind ~output ~s3 ~per_distro_depexts
  in
  match kind with
  | Obuilder -> print_obuilder_all_summary ~per_distro_paths
  | Docker ->
      let oi_path = output / "Dockerfile.oi" in
      Registry_docker.write_dockerfile oi_path
        (Registry_docker.dockerfile_oi ~src_context);
      let compose_path = output / "docker-compose.yml" in
      Registry_docker.write_file compose_path
        (Registry_docker.docker_compose_yaml ~s3 ~distros:default_distros ());
      print_docker_all_summary ~oi_path ~per_distro_paths ~compose_path

(* Reject unworkable flag combinations up front. [obuilder] only wires
   up [--all] and target-mode; the project / test / no-recipe flows are
   Dockerfile-only. *)
let validate_flags ~kind ~test_mode ~all ~no_recipe ~targets =
  let cmd = Fmt.str "oi dist %s" (kind_name kind) in
  if test_mode && all then
    Oi.Error.fail_config_error "%s: --test and --all are mutually exclusive" cmd;
  if targets <> [] && all then
    Oi.Error.fail_config_error
      "%s: --all and positional TARGET(s) are mutually exclusive" cmd;
  if targets <> [] && test_mode then
    Oi.Error.fail_config_error
      "%s: --test and positional TARGET(s) are mutually exclusive" cmd;
  match kind with
  | Docker -> ()
  | Obuilder ->
      if test_mode || no_recipe || (targets = [] && not all) then
        Oi.Error.fail_config_error
          "%s: only --all or positional TARGET(s) are supported \
           (--test/--no-recipe/project-mode are Docker-only)"
          cmd

(* Plain record so the [run] callback doesn't need a 16-arg positional list. *)
type run_args = {
  refresh : bool;
  registry : string;
  oi_version : string;
  no_recipe : bool;
  no_cache_mount : bool;
  test_mode : bool;
  all : bool;
  distro : Dockerfile_opam.Distro.t;
  src_context : string;
  s3 : Registry_docker.s3_config;
  output : string option;
  targets : string list;
}

(* Dispatch to the correct emit path. Order matters: [--all] wins, then
   positional TARGETs (with/without [--no-recipe]), then [--test], else
   the default project Dockerfile. The non-Docker flows are unreachable
   for [Obuilder] because {!validate_flags} rejects them first. *)
let dispatch_emit ~kind ~harness ~data_dir ~cwd_s ~args =
  let {
    refresh;
    registry;
    oi_version;
    no_recipe;
    no_cache_mount;
    test_mode;
    all;
    distro;
    src_context;
    s3;
    output;
    targets;
  } =
    args
  in
  let {
    Harness.fs;
    proc_mgr;
    clock;
    sys;
    platform;
    os_key;
    cache;
    http_session;
    _;
  } =
    harness
  in
  if all then
    let dir = Stdlib.Option.value output ~default:cwd_s in
    emit_all ~kind ~fs ~sys ~platform ~cache ~data_dir ~refresh ~src_context ~s3
      ~output:dir ()
  else if targets <> [] then
    if no_recipe then
      Docker_target.emit_no_recipe ~distro ~oi_version ~registry ~no_cache_mount
        ~output ~targets
    else
      Docker_target.emit ~fs ~proc_mgr ~clock ~sys ~os_key ~cache ~data_dir
        ~session:http_session ~platform ~refresh ~registry ~distro ~oi_version
        ~no_cache_mount ~obuilder:(kind = Obuilder) ~output ~targets
  else if test_mode then
    emit_project ~cmd:"oi test" ~suffix:"test" ~tag_label:"my-project-test"
      ~generator:"oi dist docker --test" ~cwd_s ~distro ~output
  else
    Docker_target.emit_local ~fs ~proc_mgr ~clock ~sys ~os_key ~cache ~data_dir
      ~session:http_session ~platform ~refresh ~registry ~distro ~oi_version
      ~no_cache_mount ~output ~cwd:cwd_s

(* Cmdliner terms: each one is a single argument; lifted to module level
   so the [cmd] term stays compact. *)

let test_mode_term =
  Arg.(
    value & flag
    & info
        ~doc:"Emit a Dockerfile running $(b,oi test) instead of $(b,oi build)."
        [ "test" ])

let all_term =
  Arg.(
    value & flag
    & info
        ~doc:
          "Multi-distro registry project. Docker: $(b,Dockerfile.oi), one \
           $(b,Dockerfile.<distro>) per target, and $(b,docker-compose.yml). \
           Obuilder: one $(b,<distro>.spec) per target (no compose)."
        [ "all" ])

let distro_term =
  let distro_conv =
    Cmdliner.Arg.conv ~docv:"DISTRO" (parse_distro, pp_distro)
  in
  Arg.(
    value
    & opt distro_conv (`Debian `Stable)
    & info ~docv:"DISTRO"
        ~doc:
          "Base distro tag (e.g. $(b,debian-13), $(b,alpine-3.23), \
           $(b,fedora-43)). Ignored with $(b,--all)."
        [ "distro" ])

let src_context_term =
  Arg.(
    value & opt string "."
    & info ~docv:"DIR"
        ~doc:
          "Path to the $(b,oi) source tree within the Docker build context. \
           $(b,docker --all) only."
        [ "src" ])

let output_term =
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"PATH"
        ~doc:
          "Output path. Default file: $(b,Dockerfile.oi-{build,test}.<distro>) \
           in cwd. With $(b,--all): directory for the project (default: cwd). \
           With target args: $(b,Dockerfile.oi-<slug>.<distro>) / \
           $(b,oi-<slug>.<distro>.spec) (or a directory)."
        [ "o"; "output" ])

let targets_term =
  Arg.(
    value & pos_all string []
    & info ~docv:"TARGET"
        ~doc:
          "Build target(s): a $(b,PKG) name, $(b,@HANDLE) overlay, or \
           $(b,@HANDLE/PKG). Generates a file that fetches every d10ir archive \
           in one cacheable layer and replays the plan. Mutually exclusive \
           with $(b,--all) and $(b,--test)."
        [])

let oi_version_term =
  Arg.(
    value & opt string "latest"
    & info ~docv:"TAG"
        ~doc:
          "Release tag to bake into the generated file as the default \
           $(b,OI_VERSION) ARG (resolves at build time)."
        [ "oi-version" ])

let no_recipe_term =
  Arg.(
    value & flag
    & info
        ~doc:
          "Solve and build $(i,TARGET) at $(b,docker build) time instead of \
           baking a fixed plan. Self-contained but reproducible only while the \
           release, registry and reporepo are stable. Docker-only."
        [ "no-recipe" ])

let no_cache_mount_term =
  Arg.(
    value & flag
    & info
        ~doc:
          "Don't use BuildKit cache mounts. Slower (no cross-build cache \
           reuse) but works where the cache and image layers are on different \
           filesystems."
        [ "no-cache-mount" ])

let s3_docs = "S3 / s3cmd configuration (only used with $(b,--all))"

let s3_bucket_term =
  Arg.(
    value
    & opt string Registry_docker.default_s3_config.bucket
    & info ~docv:"URI" ~docs:s3_docs
        ~doc:
          "Target URI passed to the final $(b,s3cmd put --recursive \
           --skip-existing) (e.g. $(b,s3://my-bucket/))."
        [ "s3-bucket" ])

let s3_host_base_term =
  Arg.(
    value
    & opt string Registry_docker.default_s3_config.host_base
    & info ~docv:"HOST" ~docs:s3_docs
        ~doc:
          "$(b,s3cmd) $(b,host_base) endpoint written to the generated \
           $(b,/tmp/oi-s3cfg)."
        [ "s3-host-base" ])

let s3_host_bucket_term =
  Arg.(
    value
    & opt string Registry_docker.default_s3_config.host_bucket
    & info ~docv:"HOST" ~docs:s3_docs
        ~doc:
          "$(b,s3cmd) $(b,host_bucket) endpoint (typically equal to \
           $(b,--s3-host-base) for path-style addressing)."
        [ "s3-host-bucket" ])

let s3_bucket_location_term =
  Arg.(
    value
    & opt string Registry_docker.default_s3_config.bucket_location
    & info ~docv:"LOC" ~docs:s3_docs
        ~doc:
          "$(b,s3cmd) $(b,bucket_location) hint (e.g. $(b,auto), \
           $(b,us-east-1))."
        [ "s3-bucket-location" ])

let s3_enable_multipart_term =
  Arg.(
    value & flag
    & info ~docs:s3_docs
        ~doc:
          "Set $(b,enable_multipart = True) in the generated $(b,s3cmd) config \
           (default: off, matching the $(i,oi) maintainers' $(b,~/.s3cfg))."
        [ "s3-enable-multipart" ])

let man kind =
  let artefact, all_line =
    match kind with
    | Docker ->
        ( "Dockerfiles",
          "$(b,Dockerfile.oi), per-distro Dockerfiles, and \
           $(b,docker-compose.yml)." )
    | Obuilder -> ("obuilder specs", "one $(b,<distro>.spec) per target distro.")
  in
  [
    `S Manpage.s_description;
    `P (Fmt.str "Emit %s for building or testing the current project." artefact);
    `I ("(default)", "project build (Docker-only).");
    `I ("$(b,--test)", "project test (Docker-only).");
    `I ("$(b,TARGET)", "replay a solved plan for the given package(s).");
    `I ("$(b,--all)", all_line);
    `S Manpage.s_examples;
    `Pre
      (match kind with
      | Docker ->
          "  oi dist docker\n\
          \  oi dist docker --test --distro=alpine-3.23\n\
          \  oi dist docker --all -o ./registry-build\n\
          \  cd ./registry-build && docker compose up --build"
      | Obuilder ->
          "  oi dist obuilder utop\n\
          \  oi dist obuilder --all -o ./registry-build");
  ]

(* Run the command after Cmdliner has parsed every flag. *)
let run_with ~kind ~c ~args =
  Harness.run @@ fun ~sw env ->
  let harness =
    Harness.bootstrap ~sw ~data_dir:c.Terms.data_dir ~format:c.Terms.format env
      c.Terms.cache_dir
  in
  validate_flags ~kind ~test_mode:args.test_mode ~all:args.all
    ~no_recipe:args.no_recipe ~targets:args.targets;
  let cwd_s, _ = Workspace.resolved_cwd harness.fs in
  dispatch_emit ~kind ~harness ~data_dir:c.Terms.data_dir ~cwd_s ~args

let cmd ~kind =
  let name = kind_name kind in
  let doc =
    match kind with
    | Docker -> "Generate Dockerfiles for project builds and CI."
    | Obuilder -> "Generate obuilder specs for project builds and CI."
  in
  let run c refresh registry oi_version no_recipe no_cache_mount test_mode all
      distro src_context s3_bucket s3_host_base s3_host_bucket
      s3_bucket_location s3_enable_multipart output targets =
    let s3 : Registry_docker.s3_config =
      {
        bucket = s3_bucket;
        host_base = s3_host_base;
        host_bucket = s3_host_bucket;
        bucket_location = s3_bucket_location;
        enable_multipart = s3_enable_multipart;
      }
    in
    let args =
      {
        refresh;
        registry;
        oi_version;
        no_recipe;
        no_cache_mount;
        test_mode;
        all;
        distro;
        src_context;
        s3;
        output;
        targets;
      }
    in
    run_with ~kind ~c ~args
  in
  Cmd.v
    (Cmd.info name ~doc ~man:(man kind))
    Term.(
      const run $ Terms.common $ Terms.refresh $ Terms.registry
      $ oi_version_term $ no_recipe_term $ no_cache_mount_term $ test_mode_term
      $ all_term $ distro_term $ src_context_term $ s3_bucket_term
      $ s3_host_base_term $ s3_host_bucket_term $ s3_bucket_location_term
      $ s3_enable_multipart_term $ output_term $ targets_term)

(* The [docker] and [obuilder] subcommands of [oi dist]. *)
let subcommands = [ cmd ~kind:Docker; cmd ~kind:Obuilder ]
