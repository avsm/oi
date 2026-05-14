let ( / ) = Filename.concat

module Distro = Dockerfile_opam.Distro

(* Slug a target token for filename use: strips '@' and replaces '/' with '-'.
   "dune" -> "dune"; "@avsm/karakeep" -> "avsm-karakeep"; "@oxcaml" -> "oxcaml". *)
let slug_of_target s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '@' -> ()
      | '/' -> Buffer.add_char buf '-'
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let bp_target_of_token (token : string) : Oi.Build_pipeline.target =
  match Target.parse_build_target token with
  | Plain_target s -> Oi.Build_pipeline.Plain s
  | Overlay_pkg (h, spec) -> Oi.Build_pipeline.Overlay_pkg { handle = h; spec }
  | Overlay_all h -> Oi.Build_pipeline.Overlay_all h

let handles_of_targets tokens =
  List.filter_map
    (fun t ->
      match Target.parse_build_target t with
      | Plain_target _ -> None
      | Overlay_pkg (h, _) | Overlay_all h -> Some h)
    tokens
  |> List.sort_uniq String.compare

(* Project the merged d10ir plan's unique archive shas, in topological-ish
   order (preserves the [merged.nodes] ordering, deduped first-wins). *)
let unique_archive_shas (plan : D10ir.Plan.t) =
  let seen = Hashtbl.create 16 in
  List.filter_map
    (fun (n : D10ir.Plan.node) ->
      let s = n.archive.sha256 in
      if Hashtbl.mem seen s then None
      else begin
        Hashtbl.replace seen s ();
        Some s
      end)
    plan.nodes

(* Resolve [distro] into the canonical [(image, tag, label, mgr)] tuple a
   renderer needs, hard-erroring if the distro uses a package manager we
   don't support. [caller] is just the command label in the error string. *)
let resolve_distro_meta ~caller distro =
  let resolved = Distro.resolve_alias distro in
  let img, image_tag = Distro.base_distro_tag (resolved :> Distro.t) in
  let distro_label = Distro.tag_of_distro (resolved :> Distro.t) in
  let mgr = Distro.package_manager (resolved :> Distro.t) in
  let mgr =
    match mgr with
    | (`Apk | `Apt | `Yum) as m -> m
    | _ ->
        Oi.Error.fail_config_error
          "%s: distro %s uses an unsupported package manager" caller
          distro_label
  in
  (img, image_tag, distro_label, mgr)

(* Combine base build-depexts with extra user/project depexts, deduping. *)
let combine_depexts ~mgr ~depexts =
  let base = Registry_docker.build_depexts mgr in
  let words = String.split_on_char ' ' base |> List.filter (( <> ) "") in
  let extra_words =
    List.filter (fun p -> not (List.mem p words)) depexts
    |> List.sort_uniq String.compare
  in
  String.concat " " (words @ extra_words)

(* [RUN] prefix for the fetch stage. With BuildKit cache mounts we let
   BuildKit create the target dir implicitly; without we prepend [mkdir -p]
   so [cd] / oi steps don't crash on a missing path. *)
let fetch_run_prefix ~no_cache_mount =
  if no_cache_mount then "RUN mkdir -p /cache/d10ir/archives\nRUN "
  else
    "RUN \
     --mount=type=cache,target=/cache/d10ir/archives,id=oi-d10ir-archives,sharing=locked \
     \\\n"

(* [RUN] prefix for the build / replay stage; same trade-off as
   [fetch_run_prefix] but also mounts the layer cache. *)
let build_run_prefix ~no_cache_mount =
  if no_cache_mount then
    "RUN mkdir -p /cache/d10ir/archives /cache/layers && \\\n    "
  else
    "RUN --mount=type=cache,target=/cache/d10ir/archives,id=oi-d10ir-archives \\\n\
    \    --mount=type=cache,target=/cache/layers,id=oi-layers \\\n\
    \    "

let render_cache_doc ~no_cache_mount =
  if no_cache_mount then
    "#   RUN fetch-archives      -> downloaded into the image layer (no\n\
     #                              cache mount); rebuilds re-download.\n\
     #   RUN oi build            -> solves + builds into the image layer."
  else
    "#   RUN fetch-archives      -> ONE layer; busts only when the sha list \
     inside\n\
     #                              the heredoc changes. Archive bytes live in a\n\
     #                              BuildKit cache mount, so unchanged shas \
     aren't\n\
     #                              re-downloaded.\n\
     #   RUN oi build            -> reuses the cache mount + d10 layer cache"

(* Render the full Dockerfile. One stage for the distro base + oi binary,
   one stage that fetches every archive in one heredoc'd RUN under a BuildKit
   cache mount, then [oi build TARGET] which solves against the reporepo
   inside the container and consumes the prefetched archives.

   The earlier shape baked a [recipe.json] sidecar and ran [oi ir run /work]
   to replay it; that's gone because the prefetched archives + the reporepo
   give [oi build] everything it needs to solve and build directly. The
   image is no longer pinned to a frozen plan, so reporepo changes between
   archive bake and [docker build] are picked up — at the cost of paying
   the solve time on every build.

   [no_cache_mount=true] strips the [--mount=type=cache] directives and falls
   back to plain [mkdir -p] for the same target paths. Useful when the
   BuildKit cache mount lives on a different filesystem from the image
   overlay and downstream [cp -Rfl] hardlink passes (e.g. [Sysops.link_tree])
   fail with [EXDEV]. The trade-off is no cross-build cache reuse: every
   docker build re-downloads archives and rebuilds layers. *)
(* Format string for the target-mode Dockerfile. Lives at module level so
   [render_dockerfile] is short enough for the length check. *)
let dockerfile_template : (_, _, _, _) format4 =
  {|# syntax=docker/dockerfile:1.7
#
# Generated by `oi docker %s --distro=%s`.
# Target:    %s
# Distro:    %s
# Nodes:     %d (solver plan at generation time; container re-solves)
# Archives:  %d unique
#
# Cache layout (top to bottom = most-stable to most-volatile):
#   base + depexts          -> invalidates on distro/depext change
#   oi binary               -> invalidates on OI_VERSION change
%s

ARG OI_VERSION=%s
ARG OI_REGISTRY=%s
ARG OI_REPOREPO_URL=

# ---- 1. base distro + build depexts -----------------------------------------
FROM %s:%s AS base
ARG OI_VERSION
RUN %s

# ---- 2. install oi from the GitHub releases page ----------------------------
RUN set -eux; \
    arch=$(uname -m); \
    tag="${OI_VERSION}"; \
    if [ "$tag" = "latest" ]; then \
        tag=$(curl -fsSL https://api.github.com/repos/avsm/oi/releases/latest \
              | sed -n 's/.*"tag_name": *"\([^"]*\)".*/\1/p' | head -n1); \
    fi; \
    curl -fsSL -o /usr/local/bin/oi \
        "https://github.com/avsm/oi/releases/download/${tag}/oi-linux-${arch}"; \
    chmod 0755 /usr/local/bin/oi; \
    /usr/local/bin/oi --version
ENV OI_CACHE_DIR=/cache OI_DATA_DIR=/state CI=true

# Pre-stamp the cache + data roots with the schema [oi --version] writes
# itself. [Stamp.ensure] (called at the start of every oi invocation)
# treats "stamp present + matching schema" as a no-op; without this, the
# cache mounts below would be populated by step 3 before [oi build]
# fires, and [Stamp.check] would mistake "items present + no stamp" for a
# pre-stamp install that needs sweeping.
RUN mkdir -p /cache /state && \
    printf 'schema %%d\nwritten_at 0\n' %d > /cache/.oi-stamp && \
    printf 'schema %%d\nwritten_at 0\n' %d > /state/.oi-stamp

# ---- 3. bulk archive fetch (one layer) --------------------------------------
FROM base AS build
ARG OI_REGISTRY
ARG OI_REPOREPO_URL
# oi's [Source.Reporepo.env_url] treats an empty [OI_REPOREPO_URL] as "use
# the built-in default", so leaving the ARG empty doesn't break the build.
ENV OI_REGISTRY=${OI_REGISTRY} OI_REPOREPO_URL=${OI_REPOREPO_URL}

%s<<'BASH'
set -eu
cd /cache/d10ir/archives
cat > /tmp/shas.txt <<'SHAS'
%s
SHAS
xargs -a /tmp/shas.txt -P8 -I{} sh -c '
    sha="$1"; f="${sha}.tar.zst"
    if [ -f "$f" ] && [ "$(sha256sum "$f" | cut -d" " -f1)" = "$sha" ]; then
        exit 0
    fi
    curl -fsSL --retry 3 -o "$f.tmp" "${OI_REGISTRY}/d10ir-archives/$f"
    echo "$sha  $f.tmp" | sha256sum -c -
    mv "$f.tmp" "$f"
' _ {}
BASH

# ---- 4. solve + build against the prefetched archives -----------------------
# [--use-registry=archives] disables the layer fetch so every package is built
# from source against the prefetched archive set. The solve consults the
# reporepo (cloned on first use) for opam metadata; the archives are
# content-addressed in [/cache/d10ir/archives/<sha>.tar.zst].
# [--dist=/dist] surfaces the [bin/], [sbin/], [share/] sub-trees of every
# root layer into [/dist] so the runtime stage below can [COPY --from=build].
%soi build --registry=${OI_REGISTRY} --use-registry=archives --dist=/dist %s

# ---- 5. runtime image ------------------------------------------------------
# Same distro + depexts as the build stage so any libs the binaries dlopen at
# runtime are present, then copy the gathered binaries and shared data into
# the FHS prefix. One [COPY] of [/dist/] -> [/usr/local/] preserves the
# [bin/], [sbin/], [share/] sub-trees and tolerates any of them being absent
# (a target that ships only binaries doesn't produce [/dist/share/]). Build
# artefacts under [/cache], [/state] are left behind in the [build] stage.
FROM base AS runtime
COPY --from=build /dist/ /usr/local/
|}

let render_dockerfile ~distro ~oi_version_default ~registry_default ~depexts
    ~shas ~target_label ~recipe_node_count ~no_cache_mount =
  let img, image_tag, distro_label, mgr =
    resolve_distro_meta ~caller:"oi docker" distro
  in
  let install =
    Registry_docker.install_cmd mgr (combine_depexts ~mgr ~depexts)
  in
  let sha_block = String.concat "\n" shas in
  let fetch_prefix = fetch_run_prefix ~no_cache_mount in
  let replay_prefix = build_run_prefix ~no_cache_mount in
  let cache_doc = render_cache_doc ~no_cache_mount in
  Fmt.str dockerfile_template target_label distro_label target_label
    distro_label recipe_node_count (List.length shas) cache_doc
    oi_version_default registry_default img image_tag install
    Oi.Stamp.cache_schema Oi.Stamp.data_schema fetch_prefix sha_block
    replay_prefix target_label

(* Obuilder spec renderer — parallel to {!render_dockerfile}, same
   multi-stage shape: a [(build oi-build (...))] stage that prefetches
   archives and runs [oi build --dist=/dist TARGET], then a top-level
   stage that pulls [/dist/] into [/usr/local/].

   Differences from the Dockerfile:
   - BuildKit [--mount=type=cache] becomes obuilder [(cache (ID (target P)))].
   - [ARG] vars don't exist in obuilder; the registry / repo URL are
     baked into the [(env …)] block so swapping mirrors means regenerating
     the spec. [oi_version] is resolved to a concrete tag at gen time
     for the same reason (no [latest] indirection inside the spec). *)
let obuilder_template : (_, _, _, _) format4 =
  {|; Generated by `oi docker --obuilder %s --distro=%s`.
; Target:    %s
; Distro:    %s
; Nodes:     %d (solver plan at generation time; container re-solves)
; Archives:  %d unique

((build oi-build
   ((from "%s:%s")
    (comment "Build stage — bootstrap toolchain, archive prefetch, oi build")
    (env CI "true")
    (env OI_CACHE_DIR "/cache")
    (env OI_DATA_DIR "/state")
    (env OI_REGISTRY "%s")
    (run (network host) (shell "%s"))
    (run (network host)
         (shell "set -eux; arch=$(uname -m); tag=\"%s\"; if [ \"$tag\" = \"latest\" ]; then tag=$(curl -fsSL https://api.github.com/repos/avsm/oi/releases/latest | sed -n 's/.*\"tag_name\": *\"\\([^\"]*\\)\".*/\\1/p' | head -n1); fi; curl -fsSL -o /usr/local/bin/oi \"https://github.com/avsm/oi/releases/download/${tag}/oi-linux-${arch}\"; chmod 0755 /usr/local/bin/oi; /usr/local/bin/oi --version"))
    (run (shell "mkdir -p /cache /state && printf 'schema %%d\\nwritten_at 0\\n' %d > /cache/.oi-stamp && printf 'schema %%d\\nwritten_at 0\\n' %d > /state/.oi-stamp"))
    (run (cache (oi-d10ir-archives (target /cache/d10ir/archives)))
         (network host)
         (shell "set -eu; mkdir -p /cache/d10ir/archives; cd /cache/d10ir/archives; for sha in %s; do f=\"$sha.tar.zst\"; if [ -f \"$f\" ] && [ \"$(sha256sum \"$f\" | cut -d' ' -f1)\" = \"$sha\" ]; then continue; fi; curl -fsSL --retry 3 -o \"$f.tmp\" \"%s/d10ir-archives/$f\"; echo \"$sha  $f.tmp\" | sha256sum -c -; mv \"$f.tmp\" \"$f\"; done"))
    (run (cache (oi-d10ir-archives (target /cache/d10ir/archives))
                (oi-layers (target /cache/layers)))
         (network host)
         (shell "oi build --registry=%s --use-registry=archives --dist=/dist %s"))))
 (from "%s:%s")
 (comment "Runtime image — same depexts as build, plus the gathered /dist tree.")
 (env CI "true")
 (run (network host) (shell "%s"))
 (copy (from (build oi-build)) (src "/dist/") (dst "/usr/local/")))
|}

let render_obuilder_spec ~distro ~oi_version_default ~registry_default ~depexts
    ~shas ~target_label ~recipe_node_count =
  let img, image_tag, distro_label, mgr =
    resolve_distro_meta ~caller:"oi docker --obuilder" distro
  in
  let install =
    Registry_docker.install_cmd mgr (combine_depexts ~mgr ~depexts)
  in
  let sha_block = String.concat " " shas in
  Fmt.str obuilder_template target_label distro_label target_label distro_label
    recipe_node_count (List.length shas) img image_tag registry_default install
    oi_version_default Oi.Stamp.cache_schema Oi.Stamp.data_schema sha_block
    registry_default registry_default target_label img image_tag install

let solve_targets ~fs ~proc_mgr ~clock ~sys ~os_key ~cache ~data_dir ~session
    ~platform ~refresh targets =
  let conf =
    Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  let bp_targets = List.map bp_target_of_token targets in
  let global_handles = handles_of_targets targets in
  let pipeline_env : Oi.Build_pipeline.env =
    {
      proc_mgr;
      fs;
      clock;
      sys;
      os_key;
      cache;
      data_dir;
      http_session = session;
    }
  in
  let req : Oi.Build_pipeline.request =
    {
      targets = bp_targets;
      with_repos = global_handles;
      pins = [];
      extra_repos = [];
      constraints = OpamPackage.Name.Map.empty;
      toolchain_override = None;
      toolchain = None;
      conf;
      local_packages_dir = None;
      project_root = None;
      (* The container starts empty, so the recipe must include every node
         regardless of whether the host has a cached layer for it. Without
         this, [Plan.of_solution] marks cached packages as [Binary] and
         [Recipe_emitter] prunes them from the d10ir plan — the generated
         Dockerfile would [oi ir run] a recipe with 0 nodes. *)
      force_source = true;
      with_test = false;
      refresh;
    }
  in
  Oi.Build_pipeline.solve pipeline_env req

(* Solve the cwd project (no positional target): mirrors the surface area
   {!Sync.run} uses, trimmed to what's needed for SHA extraction. The
   merged d10ir plan it produces feeds the prefetch step in the
   generated Dockerfile, exactly like {!solve_targets} does for the
   non-local path. *)
(* Resolve toolchain + project overlays + extras for a local project. *)
let resolve_local_project_inputs ~fs ~sys ~data_dir ~conf ~project ~url_project
    =
  let candidate_overlays =
    project.Oi.Project.overlays @ url_project.Oi.Project.Url.overlays
  in
  let toolchain =
    Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf ~install:false
      ~override:None ~handles:candidate_overlays ()
  in
  let conf, _ = Oi.Pipeline.solver_inputs toolchain conf in
  let project_overlays =
    Oi.Pipeline.filter_compatible_overlays
      ~reporepo_path:(Terms.reporepo_path ()) ~toolchain candidate_overlays
  in
  let all_extras =
    Target.merge_extras
      ~cli:(Target.cli_extra_repos ~fs ~sys ?toolchain project_overlays)
      ~project:(project.extra_repos @ url_project.extra_repos)
  in
  (conf, toolchain, project_overlays, all_extras)

(* Solver root names: project deps + non-[ocaml] CLI extras + URL-project
   roots, deduplicated. *)
let local_solver_names ~project ~extra_cli ~url_project =
  let extra_names =
    List.filter_map
      (fun (d : Oi.Project.Script.dep) ->
        if OpamPackage.Name.to_string d.name = "ocaml" then None
        else Some (OpamPackage.Name.to_string d.name))
      extra_cli
  in
  project.Oi.Project.deps @ extra_names @ url_project.Oi.Project.Url.roots
  |> List.sort_uniq String.compare

let solve_local_project ~fs ~proc_mgr ~clock ~sys ~os_key ~cache ~data_dir
    ~session ~platform ~refresh ~cwd =
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
  let project = Oi.Project.load ~fs cwd in
  let extra_cli, url_project =
    Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh []
  in
  if project.deps = [] && extra_cli = [] && url_project.roots = [] then
    Oi.Error.fail_config_error "oi docker: no *.opam files in %s" cwd;
  let conf =
    Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  let conf, toolchain, project_overlays, all_extras =
    resolve_local_project_inputs ~fs ~sys ~data_dir ~conf ~project ~url_project
  in
  let names = local_solver_names ~project ~extra_cli ~url_project in
  let local_packages_dir =
    match project.packages_dir with
    | Some _ -> project.packages_dir
    | None -> url_project.packages_dir
  in
  let pipeline_env : Oi.Build_pipeline.env =
    {
      proc_mgr;
      fs;
      clock;
      sys;
      os_key;
      cache;
      data_dir;
      http_session = session;
    }
  in
  let req : Oi.Build_pipeline.request =
    {
      targets = [ Group { tokens = names; handles = [] } ];
      with_repos = project_overlays;
      pins = project.pins @ url_project.pins;
      extra_repos = all_extras;
      constraints = Oi.Project.Script.constraints extra_cli;
      toolchain_override = None;
      toolchain;
      conf;
      local_packages_dir;
      project_root = Some cwd;
      (* Same rationale as {!solve_targets}: the container starts empty, so
         [force_source] forces every dep into the d10ir plan even when the
         host has a cached layer. *)
      force_source = true;
      with_test = false;
      refresh;
    }
  in
  Oi.Build_pipeline.solve pipeline_env req

(* Render a slim, source-independent Dockerfile that doesn't bake a recipe.
   At [docker build] time, [oi] itself solves the target against the registry
   and reporepo, fetches archives, and builds. The image is reproducible
   only insofar as [OI_VERSION], the reporepo URL, and the registry index
   are stable — every other input is resolved at build time. *)
(* [RUN] prefix for the single-stage no-recipe Dockerfile. *)
let no_recipe_build_prefix ~no_cache_mount =
  if no_cache_mount then "RUN "
  else
    "RUN --mount=type=cache,target=/cache,id=oi-cache-build,sharing=locked \\\n\
    \    "

let no_recipe_template : (_, _, _, _) format4 =
  {|# syntax=docker/dockerfile:1.7
#
# Generated by `oi docker --no-recipe %s --distro=%s`.
# Target:    %s
# Distro:    %s
# Mode:      source-independent — oi solves at docker-build time, fetches
#            archives from the registry, and builds. No recipe sidecar.
#
# Caching:
#   base + depexts          -> apt/dnf/apk layer; invalidates on distro change.
#   oi/oix install          -> invalidates on OI_VERSION change.
%s

ARG OI_VERSION=%s
ARG OI_REGISTRY=%s
ARG OI_REPOREPO_URL=

FROM %s:%s AS base
ARG OI_VERSION
RUN %s
RUN set -eux; \
    arch=$(uname -m); \
    tag="${OI_VERSION}"; \
    if [ "$tag" = "latest" ]; then \
        tag=$(curl -fsSL https://api.github.com/repos/avsm/oi/releases/latest \
              | sed -n 's/.*"tag_name": *"\([^"]*\)".*/\1/p' | head -n1); \
    fi; \
    for bin in oi oix; do \
        curl -fsSL -o /usr/local/bin/$bin \
            "https://github.com/avsm/oi/releases/download/${tag}/$bin-linux-${arch}"; \
        chmod 0755 /usr/local/bin/$bin; \
    done; \
    /usr/local/bin/oi --version
ENV OI_CACHE_DIR=/cache OI_DATA_DIR=/state CI=true

# Pre-stamp cache + data roots so [Stamp.ensure] sees [Up_to_date] and
# doesn't try to "upgrade" (sweep) the BuildKit cache mount on first use.
RUN mkdir -p /cache /state && \
    printf 'schema %%d\nwritten_at 0\n' %d > /cache/.oi-stamp && \
    printf 'schema %%d\nwritten_at 0\n' %d > /state/.oi-stamp

# ---- solve + fetch + build ---------------------------------------------------
FROM base AS build
ARG OI_REGISTRY
ARG OI_REPOREPO_URL
# oi's [Source.Reporepo.env_url] treats an empty [OI_REPOREPO_URL] as "use
# the built-in default", so leaving the ARG empty doesn't break the build.
ENV OI_REGISTRY=${OI_REGISTRY} OI_REPOREPO_URL=${OI_REPOREPO_URL}
%soi build --registry=${OI_REGISTRY} %s

# Usage:
#   docker build -t %s -f %s .
#   docker run --rm %s oix %s --help
|}

let no_recipe_cache_doc ~no_cache_mount =
  if no_cache_mount then
    "#   oi build TARGET         -> writes /cache into the image layer (no cache\n\
     #                              mount); rebuilds re-fetch and re-solve."
  else
    "#   oi build TARGET         -> one cached layer per target. The BuildKit \
     cache\n\
     #                              mount keeps /cache across rebuilds, so a \
     second\n\
     #                              build with the same OI_VERSION and TARGET \
     hits\n\
     #                              local d10 layers and is mostly free."

let render_no_recipe_dockerfile ~distro ~oi_version_default ~registry_default
    ~target_label ~no_cache_mount =
  let img, image_tag, distro_label, mgr =
    resolve_distro_meta ~caller:"oi docker" distro
  in
  let install =
    Registry_docker.install_cmd mgr (Registry_docker.build_depexts mgr)
  in
  let build_prefix = no_recipe_build_prefix ~no_cache_mount in
  let cache_doc = no_recipe_cache_doc ~no_cache_mount in
  let slug = slug_of_target target_label in
  let image_name = "oi-" ^ slug in
  let dockerfile_name = Fmt.str "Dockerfile.oi-%s.%s" slug distro_label in
  Fmt.str no_recipe_template target_label distro_label target_label distro_label
    cache_doc oi_version_default registry_default img image_tag install
    Oi.Stamp.cache_schema Oi.Stamp.data_schema build_prefix target_label
    image_name dockerfile_name image_name target_label

let emit_no_recipe ~distro ~oi_version ~registry ~no_cache_mount ~output
    ~targets =
  let target_label = String.concat " " targets in
  let body =
    render_no_recipe_dockerfile ~distro ~oi_version_default:oi_version
      ~registry_default:registry ~target_label ~no_cache_mount
  in
  let slug = targets |> List.map slug_of_target |> String.concat "-" in
  let distro_tag =
    Distro.tag_of_distro (Distro.resolve_alias distro :> Distro.t)
  in
  let dockerfile_name = Fmt.str "Dockerfile.oi-%s.%s" slug distro_tag in
  let cwd = Sys.getcwd () in
  let dockerfile_path =
    match output with
    | None -> cwd / dockerfile_name
    | Some p ->
        if Sys.file_exists p && Sys.is_directory p then p / dockerfile_name
        else p
  in
  Registry_docker.write_file dockerfile_path body;
  Oi.Say.step "Wrote";
  Oi.Say.info "%s" dockerfile_path;
  Oi.Say.newline ();
  Oi.Say.step "Build with";
  Oi.Say.info "docker build -t oi-%s -f %s %s" slug dockerfile_path
    (Filename.dirname dockerfile_path)

(* Render the project-mode Dockerfile. Same multi-stage shape as
   {!render_dockerfile}, but the "target" is the cwd project itself:
   we [COPY . /work], prefetch the dep closure's archives, then
   [oi build --dist=/dist] (project mode is auto-detected from the
   *.opam files we just copied in). The runtime stage is identical. *)
let local_template : (_, _, _, _) format4 =
  {|# syntax=docker/dockerfile:1.7
#
# Generated by `oi docker --distro=%s` (project mode).
# Project:   %s
# Distro:    %s
# Nodes:     %d (dep-closure plan at generation time; container re-solves)
# Archives:  %d unique
#
# A multi-stage build that mirrors `oi docker TARGET` but starts from
# the *.opam files in the cwd. The dep closure is solved locally so the
# Dockerfile can bake the archive sha list; the container then
# [COPY]s the sources in, re-solves, and runs the project's dune build.
# Drop the [Dockerfile.oi-project.<distro>] into the project root and:
#   docker build -t <project>:<distro> -f <this-file> .

ARG OI_VERSION=%s
ARG OI_REGISTRY=%s
ARG OI_REPOREPO_URL=

# ---- 1. base distro + build depexts -----------------------------------------
FROM %s:%s AS base
ARG OI_VERSION
RUN %s

# ---- 2. install oi from the GitHub releases page ----------------------------
RUN set -eux; \
    arch=$(uname -m); \
    tag="${OI_VERSION}"; \
    if [ "$tag" = "latest" ]; then \
        tag=$(curl -fsSL https://api.github.com/repos/avsm/oi/releases/latest \
              | sed -n 's/.*"tag_name": *"\([^"]*\)".*/\1/p' | head -n1); \
    fi; \
    curl -fsSL -o /usr/local/bin/oi \
        "https://github.com/avsm/oi/releases/download/${tag}/oi-linux-${arch}"; \
    chmod 0755 /usr/local/bin/oi; \
    /usr/local/bin/oi --version
ENV OI_CACHE_DIR=/cache OI_DATA_DIR=/state CI=true

# Pre-stamp the cache + data roots; same rationale as `oi docker TARGET`.
RUN mkdir -p /cache /state && \
    printf 'schema %%d\nwritten_at 0\n' %d > /cache/.oi-stamp && \
    printf 'schema %%d\nwritten_at 0\n' %d > /state/.oi-stamp

# ---- 3. bulk archive fetch (one layer) --------------------------------------
# Source archives for the project's dep closure as it was solved at
# generation time. The container re-solves (so reporepo changes between
# bake and [docker build] are picked up), but every archive it'll touch
# is already content-addressed in [/cache/d10ir/archives/].
FROM base AS build
ARG OI_REGISTRY
ARG OI_REPOREPO_URL
ENV OI_REGISTRY=${OI_REGISTRY} OI_REPOREPO_URL=${OI_REPOREPO_URL}

%s<<'BASH'
set -eu
cd /cache/d10ir/archives
cat > /tmp/shas.txt <<'SHAS'
%s
SHAS
xargs -a /tmp/shas.txt -P8 -I{} sh -c '
    sha="$1"; f="${sha}.tar.zst"
    if [ -f "$f" ] && [ "$(sha256sum "$f" | cut -d" " -f1)" = "$sha" ]; then
        exit 0
    fi
    curl -fsSL --retry 3 -o "$f.tmp" "${OI_REGISTRY}/d10ir-archives/$f"
    echo "$sha  $f.tmp" | sha256sum -c -
    mv "$f.tmp" "$f"
' _ {}
BASH

# ---- 4. copy local sources + project build ----------------------------------
# A [.dockerignore] entry for [_build/], [dist/], [.git/] etc. keeps the
# build-context lean. Project mode is auto-detected from the *.opam files
# inside [/work]; [--dist=/dist] surfaces the produced binaries / share/
# tree for the runtime stage's [COPY --from=build].
WORKDIR /work
COPY . /work
%soi build --registry=${OI_REGISTRY} --use-registry=archives --dist=/dist

# ---- 5. runtime image ------------------------------------------------------
# Same depexts as build; copy the gathered install tree into [/usr/local/].
FROM base AS runtime
COPY --from=build /dist/ /usr/local/
|}

let render_local_dockerfile ~distro ~oi_version_default ~registry_default
    ~depexts ~shas ~project_label ~recipe_node_count ~no_cache_mount =
  let img, image_tag, distro_label, mgr =
    resolve_distro_meta ~caller:"oi docker" distro
  in
  let install =
    Registry_docker.install_cmd mgr (combine_depexts ~mgr ~depexts)
  in
  let sha_block = String.concat "\n" shas in
  let fetch_prefix = fetch_run_prefix ~no_cache_mount in
  let build_prefix = build_run_prefix ~no_cache_mount in
  Fmt.str local_template distro_label project_label distro_label
    recipe_node_count (List.length shas) oi_version_default registry_default img
    image_tag install Oi.Stamp.cache_schema Oi.Stamp.data_schema fetch_prefix
    sha_block build_prefix

(* Stringify one group_result's error in a 4-line form. *)
let group_error_message (gr : Oi.Build_pipeline.group_result) =
  match gr.error with
  | Ok () -> None
  | Error e ->
      let kind =
        match e with
        | Solve_failed { msg; _ } -> Fmt.str "solve: %s" msg
        | Cycle _ -> "cycle"
        | Empty_after_strip -> "empty"
        | Elaborate_failed { msg } -> Fmt.str "elaborate: %s" msg
        | Emit_failed { msg } -> Fmt.str "emit: %s" msg
      in
      Fmt.kstr (fun s -> Some s) "%s — %s" gr.group.label kind

(* Extract the merged plan, or fail with an aggregated per-group error
   message. [what] names the kind of input ("project" / "target") for the
   error string. *)
let merged_or_fail ~what (solved : Oi.Build_pipeline.solved) =
  match solved.merged with
  | Some m -> m
  | None ->
      let msgs = List.filter_map group_error_message solved.groups in
      Oi.Error.fail_config_error
        "oi docker: %s produced no executable plan:@\n  %s" what
        (String.concat "\n  " msgs)

(* Resolve [output] into [(dst_dir, dockerfile_path)]: if it points at an
   existing directory, append [default_name]; otherwise treat the value
   as the full path. [None] writes into [default_dir]. *)
let resolve_output_path ~output ~default_dir ~default_name =
  match output with
  | None -> (default_dir, default_dir / default_name)
  | Some p ->
      if Sys.file_exists p && Sys.is_directory p then (p, p / default_name)
      else (Filename.dirname p, p)

let print_wrote ~merged_nodes ~shas dockerfile_path =
  Oi.Say.step "Wrote";
  Fmt.kstr
    (Oi.Say.info "%s  %a" dockerfile_path Oi.Style.pp_dim_string)
    "(plan: %d nodes, %d unique archives)" merged_nodes (List.length shas)

let emit_local ~fs ~proc_mgr ~clock ~sys ~os_key ~cache ~data_dir ~session
    ~platform ~refresh ~registry ~distro ~oi_version ~no_cache_mount ~output
    ~cwd =
  let solved =
    solve_local_project ~fs ~proc_mgr ~clock ~sys ~os_key ~cache ~data_dir
      ~session ~platform ~refresh ~cwd
  in
  let merged = merged_or_fail ~what:"project" solved in
  let shas = unique_archive_shas merged in
  let project_label = Filename.basename cwd in
  let dockerfile_body =
    render_local_dockerfile ~distro ~oi_version_default:oi_version
      ~registry_default:registry ~depexts:[] ~shas ~project_label
      ~recipe_node_count:(List.length merged.nodes) ~no_cache_mount
  in
  let distro_tag =
    Distro.tag_of_distro (Distro.resolve_alias distro :> Distro.t)
  in
  let dockerfile_name = Fmt.str "Dockerfile.oi-project.%s" distro_tag in
  let dst_dir, dockerfile_path =
    resolve_output_path ~output ~default_dir:cwd ~default_name:dockerfile_name
  in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dst_dir);
  Registry_docker.write_file dockerfile_path dockerfile_body;
  print_wrote ~merged_nodes:(List.length merged.nodes) ~shas dockerfile_path;
  Oi.Say.newline ();
  Oi.Say.step "Build with";
  Oi.Say.info "docker build -t %s -f %s %s" project_label dockerfile_path
    dst_dir

(* Pick the [(body, filename)] pair for an emit target run: obuilder spec
   vs Dockerfile, with the right filename naming. *)
let target_body_and_filename ~obuilder ~distro ~oi_version ~registry ~shas
    ~target_label ~recipe_node_count ~no_cache_mount ~slug ~distro_tag =
  if obuilder then
    ( render_obuilder_spec ~distro ~oi_version_default:oi_version
        ~registry_default:registry ~depexts:[] ~shas ~target_label
        ~recipe_node_count,
      Fmt.str "oi-%s.%s.spec" slug distro_tag )
  else
    ( render_dockerfile ~distro ~oi_version_default:oi_version
        ~registry_default:registry
        ~depexts:[] (* TODO target-scoped depexts; for now base set only *)
        ~shas ~target_label ~recipe_node_count ~no_cache_mount,
      Fmt.str "Dockerfile.oi-%s.%s" slug distro_tag )

let emit ~fs ~proc_mgr ~clock ~sys ~os_key ~cache ~data_dir ~session ~platform
    ~refresh ~registry ~distro ~oi_version ~no_cache_mount ~obuilder ~output
    ~targets =
  if targets = [] then
    Oi.Error.fail_config_error
      "oi docker: no target. Pass one or more PKG / @HANDLE / @HANDLE/PKG \
       tokens, or run without arguments for project mode.";
  let solved =
    solve_targets ~fs ~proc_mgr ~clock ~sys ~os_key ~cache ~data_dir ~session
      ~platform ~refresh targets
  in
  let merged = merged_or_fail ~what:"target" solved in
  let shas = unique_archive_shas merged in
  let target_label = String.concat " " targets in
  let slug = targets |> List.map slug_of_target |> String.concat "-" in
  let distro_tag =
    Distro.tag_of_distro (Distro.resolve_alias distro :> Distro.t)
  in
  let body, filename =
    target_body_and_filename ~obuilder ~distro ~oi_version ~registry ~shas
      ~target_label ~recipe_node_count:(List.length merged.nodes)
      ~no_cache_mount ~slug ~distro_tag
  in
  let cwd_s, _ = Workspace.resolved_cwd fs in
  let dst_dir, dockerfile_path =
    resolve_output_path ~output ~default_dir:cwd_s ~default_name:filename
  in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dst_dir);
  Registry_docker.write_file dockerfile_path body;
  print_wrote ~merged_nodes:(List.length merged.nodes) ~shas dockerfile_path;
  Oi.Say.newline ();
  Oi.Say.step "Build with";
  if obuilder then Oi.Say.info "obuilder build -f %s --store=…" dockerfile_path
  else Oi.Say.info "docker build -t oi-%s -f %s %s" slug dockerfile_path dst_dir
