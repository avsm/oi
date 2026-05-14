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

(* For each target distro, write [Dockerfile.<distro>] and matching obuilder
   spec, returning the paths/depext counts used for the post-emit summary. *)
let write_per_distro_files ~output ~s3 ~per_distro_depexts =
  List.map
    (fun d ->
      let overlay_depexts =
        Stdlib.Option.value (List.assoc_opt d per_distro_depexts) ~default:[]
      in
      let df_path = output / Registry_docker.one_distro_filename d in
      let df = Registry_docker.dockerfile_one_distro ~s3 ~overlay_depexts d in
      Registry_docker.write_dockerfile df_path df;
      let spec_path = output / Registry_docker.one_distro_spec_filename d in
      let spec_body =
        Registry_docker.obuilder_spec_one_distro ~s3 ~overlay_depexts d
      in
      Registry_docker.write_file spec_path spec_body;
      (d, df_path, spec_path, List.length overlay_depexts))
    default_distros

(* Summary printed after [--all] writes everything: list of files, the static
   oi build hint, and the docker-compose invocation. *)
let print_emit_all_summary ~oi_path ~per_distro_paths ~compose_path =
  Oi.Say.step "Wrote";
  Oi.Say.info "%s" oi_path;
  List.iter
    (fun (_, df_path, spec_path, n) ->
      let suffix = if n = 0 then "" else Fmt.str "  (%d overlay depexts)" n in
      Oi.Say.info "%s%a" df_path Oi.Style.pp_dim_string suffix;
      Oi.Say.info "%s" spec_path)
    per_distro_paths;
  Oi.Say.info "%s" compose_path;
  Oi.Say.newline ();
  Oi.Say.step "Static oi release binary";
  Oi.Say.info "docker buildx build -f %s --output type=local,dest=./oi-bin ."
    oi_path;
  Oi.Say.step "Run the build + sync (Docker)";
  Oi.Say.info "S3_ACCESS_KEY=… S3_SECRET_KEY=… docker compose build   %a"
    Oi.Style.pp_dim_string
    "# builds each image, runs oi build --all and s3cmd put --recursive"

(* Multi-distro registry build project: one [Dockerfile.<distro>] per
   target distribution, plus [Dockerfile.oi] (static oi builder) and
   [docker-compose.yml]. Each service runs [oi build --all --export
   /out]. *)
let emit_all ~fs ~sys ~platform ~cache ~data_dir ~refresh ~src_context ~s3
    ~output () =
  (try Unix.mkdir output 0o755 with Unix.Unix_error (EEXIST, _, _) -> ());
  let df_oi = Registry_docker.dockerfile_oi ~src_context in
  let oi_path = output / "Dockerfile.oi" in
  Registry_docker.write_dockerfile oi_path df_oi;
  Oi.Say.step "Computing overlay depexts for %d distros"
    (List.length default_distros);
  let per_distro_depexts =
    compute_per_distro_depexts ~fs ~sys ~cache ~data_dir ~refresh ~platform
  in
  let per_distro_paths =
    write_per_distro_files ~output ~s3 ~per_distro_depexts
  in
  let compose_path = output / "docker-compose.yml" in
  let compose_yaml =
    Registry_docker.docker_compose_yaml ~s3 ~distros:default_distros ()
  in
  Registry_docker.write_file compose_path compose_yaml;
  print_emit_all_summary ~oi_path ~per_distro_paths ~compose_path

(* All run-time mode flags / arg combinations rejected at startup. Single
   place keeps the [cmd] body free of validation noise. *)
let validate_flags ~test_mode ~all ~obuilder ~no_recipe ~targets =
  if test_mode && all then
    Oi.Error.fail_config_error
      "oi docker: --test and --all are mutually exclusive";
  if targets <> [] && all then
    Oi.Error.fail_config_error
      "oi docker: --all and positional TARGET(s) are mutually exclusive";
  if targets <> [] && test_mode then
    Oi.Error.fail_config_error
      "oi docker: --test and positional TARGET(s) are mutually exclusive";
  if obuilder && (test_mode || no_recipe || (targets = [] && not all)) then
    Oi.Error.fail_config_error
      "oi docker --obuilder: supported with --all or positional TARGET(s); \
       --no-recipe/--test/project-mode aren't wired up yet.";
  if obuilder && all then
    Oi.Say.info
      "--obuilder is implied by --all (both Dockerfiles and obuilder specs are \
       emitted unconditionally)."

(* Plain record so the [run] callback doesn't need a 17-arg positional list. *)
type run_args = {
  refresh : bool;
  registry : string;
  oi_version : string;
  no_recipe : bool;
  no_cache_mount : bool;
  obuilder : bool;
  test_mode : bool;
  all : bool;
  distro : Dockerfile_opam.Distro.t;
  src_context : string;
  s3 : Registry_docker.s3_config;
  output : string option;
  targets : string list;
}

(* Dispatch to the correct emit path. Order matters: [--all] wins, then
   positional TARGETs (with/without [--no-recipe]), then [--test], else the
   default project Dockerfile. *)
let dispatch_emit ~harness ~data_dir ~cwd_s ~args =
  let {
    refresh;
    registry;
    oi_version;
    no_recipe;
    no_cache_mount;
    obuilder;
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
    emit_all ~fs ~sys ~platform ~cache ~data_dir ~refresh ~src_context ~s3
      ~output:dir ()
  else if targets <> [] then
    if no_recipe then
      Docker_target.emit_no_recipe ~distro ~oi_version ~registry ~no_cache_mount
        ~output ~targets
    else
      Docker_target.emit ~fs ~proc_mgr ~clock ~sys ~os_key ~cache ~data_dir
        ~session:http_session ~platform ~refresh ~registry ~distro ~oi_version
        ~no_cache_mount ~obuilder ~output ~targets
  else if test_mode then
    emit_project ~cmd:"oi test" ~suffix:"test" ~tag_label:"my-project-test"
      ~generator:"oi docker --test" ~cwd_s ~distro ~output
  else
    Docker_target.emit_local ~fs ~proc_mgr ~clock ~sys ~os_key ~cache ~data_dir
      ~session:http_session ~platform ~refresh ~registry ~distro ~oi_version
      ~no_cache_mount ~output ~cwd:cwd_s

(* Cmdliner terms: each one is a single argument; lifted to module level so
   the [cmd] term stays compact. *)

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
          "Emit multi-distro project: $(b,Dockerfile.oi), one \
           $(b,Dockerfile.<distro>) per target, and $(b,docker-compose.yml)."
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
           $(b,--all) only."
        [ "src" ])

let output_term =
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"PATH"
        ~doc:
          "Output path. Default file: $(b,Dockerfile.oi-{build,test}.<distro>) \
           in cwd. With $(b,--all): directory for the project (default: cwd). \
           With target args: $(b,Dockerfile.oi-<slug>.<distro>) (or \
           directory)."
        [ "o"; "output" ])

let targets_term =
  Arg.(
    value & pos_all string []
    & info ~docv:"TARGET"
        ~doc:
          "Build target(s): a $(b,PKG) name, $(b,@HANDLE) overlay, or \
           $(b,@HANDLE/PKG). Generates a Dockerfile that fetches every d10ir \
           archive in one cacheable layer and replays the plan. Mutually \
           exclusive with $(b,--all) and $(b,--test)."
        [])

let oi_version_term =
  Arg.(
    value & opt string "latest"
    & info ~docv:"TAG"
        ~doc:
          "Release tag to bake into the generated Dockerfile as the default \
           $(b,OI_VERSION) ARG (resolves at $(b,docker build) time)."
        [ "oi-version" ])

let no_recipe_term =
  Arg.(
    value & flag
    & info
        ~doc:
          "Emit a source-independent Dockerfile that solves and builds \
           $(b,TARGET) at $(b,docker build) time instead of baking a \
           pre-emitted recipe. No $(b,recipe.json) sidecar; the resulting file \
           is fully self-contained. Trade-off: build is reproducible only as \
           long as $(b,OI_VERSION), the registry, and the reporepo URL are \
           stable."
        [ "no-recipe" ])

let no_cache_mount_term =
  Arg.(
    value & flag
    & info
        ~doc:
          "Omit BuildKit $(b,--mount=type=cache) directives and use plain \
           $(b,mkdir -p) for the same target paths. The build is fully \
           self-contained (no cache reuse across builds) but works in \
           environments where the cache mount lives on a different filesystem \
           from the image overlay — in which case the hardlink pass [cp -Rfl] \
           used during layer staging would otherwise fail with $(b,EXDEV)."
        [ "no-cache-mount" ])

let obuilder_term =
  Arg.(
    value & flag
    & info
        ~doc:
          "Emit an obuilder spec (s-expression) instead of a Dockerfile, same \
           multi-stage strategy. Output filename becomes \
           $(b,oi-<slug>.<distro>.spec). For now only the target-mode flow \
           ($(i,TARGET) positional) is supported; combining with \
           $(b,--all)/$(b,--test)/$(b,--no-recipe)/project-mode errors out."
        [ "obuilder" ])

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

let cmd_info =
  Cmd.info "docker" ~doc:"Generate Dockerfiles for project builds and CI."
    ~man:
      [
        `S Manpage.s_description;
        `P "Emit Dockerfiles for building or testing the current project.";
        `I
          ( "(default)",
            "$(b,Dockerfile.oi-build.<distro>) running $(b,oi build)." );
        `I
          ( "$(b,--test)",
            "$(b,Dockerfile.oi-test.<distro>) running $(b,oi test)." );
        `I
          ( "$(b,--all)",
            "$(b,Dockerfile.oi), per-distro Dockerfiles, and \
             $(b,docker-compose.yml). Each service runs $(b,oi build --all \
             --export /out)." );
        `S Manpage.s_examples;
        `Pre
          "  oi docker\n\
          \  oi docker --test --distro=alpine-3.23\n\
          \  oi docker --all -o ./registry-build\n\
          \  cd ./registry-build && docker compose up --build";
      ]

(* Run the command after Cmdliner has parsed every flag. Args are packed into
   a record so further functions don't need to thread positional booleans. *)
let run_with ~c ~args =
  Harness.run @@ fun ~sw env ->
  let harness =
    Harness.bootstrap ~sw ~data_dir:c.Terms.data_dir ~format:c.Terms.format env
      c.Terms.cache_dir
  in
  validate_flags ~test_mode:args.test_mode ~all:args.all ~obuilder:args.obuilder
    ~no_recipe:args.no_recipe ~targets:args.targets;
  let cwd_s, _ = Workspace.resolved_cwd harness.fs in
  dispatch_emit ~harness ~data_dir:c.Terms.data_dir ~cwd_s ~args

let cmd =
  (* Cmdliner callback: each positional name is one Term.($) above; we just
     pack them into [run_args] and hand off to [run_with]. *)
  let run c refresh registry oi_version no_recipe no_cache_mount obuilder
      test_mode all distro src_context s3_bucket s3_host_base s3_host_bucket
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
        obuilder;
        test_mode;
        all;
        distro;
        src_context;
        s3;
        output;
        targets;
      }
    in
    run_with ~c ~args
  in
  Cmd.v cmd_info
    Term.(
      const run $ Terms.common $ Terms.refresh $ Terms.registry
      $ oi_version_term $ no_recipe_term $ no_cache_mount_term $ obuilder_term
      $ test_mode_term $ all_term $ distro_term $ src_context_term
      $ s3_bucket_term $ s3_host_base_term $ s3_host_bucket_term
      $ s3_bucket_location_term $ s3_enable_multipart_term $ output_term
      $ targets_term)
