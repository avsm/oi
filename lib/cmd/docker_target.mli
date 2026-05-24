(** [oi docker TARGET]: emit a Dockerfile that builds a target inside a
    container.

    The Dockerfile is d10ir-aware: it walks the solved [D10ir.Plan.t], lists
    every unique source-archive sha256, and bakes them into one heredoc'd RUN
    that curls them in parallel under a BuildKit cache mount. The container then
    runs [oi build --use-registry=archives TARGET], which solves against the
    reporepo and consumes the prefetched archives — no [recipe.json] sidecar,
    and reporepo changes between bake time and [docker build] are picked up. *)

module Distro = Dockerfile_opam.Distro

val emit_local :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  proc_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  sys:D10.Sysops.t ->
  os_key:string ->
  cache:Oi.Cache.t ->
  data_dir:string ->
  session:D10.Sysops.Http.session ->
  platform:Osrel.t ->
  refresh:bool ->
  registry:string ->
  distro:Distro.t ->
  oi_version:string ->
  no_cache_mount:bool ->
  output:string option ->
  cwd:string ->
  unit
(** [emit_local …] is the project-mode counterpart to {!emit}: solves the cwd's
    [*.opam] closure to derive the archive sha list, then writes a multi-stage
    Dockerfile that [COPY]s the local sources, re-solves inside the container
    (so reporepo changes between bake and [docker build] are picked up), runs
    [oi build --dist=/dist] (which drives the project's [dune build] and gathers
    install-tree binaries / share data), and finally [COPY]s the gathered tree
    into a clean depext-equipped runtime image. Output filename is
    [Dockerfile.oi-project.<distro>]. *)

val emit_no_recipe :
  distro:Distro.t ->
  oi_version:string ->
  registry:string ->
  no_cache_mount:bool ->
  output:string option ->
  targets:string list ->
  unit
(** [emit_no_recipe ~distro ~oi_version ~registry ~no_cache_mount ~output
     ~targets] is the source-independent counterpart to {!emit}: emit a
    Dockerfile that doesn't bake a [recipe.json]. At [docker build] time, [oi]
    itself solves [targets] against the configured registry / reporepo, fetches
    archives, and builds. The image is reproducible only insofar as
    [OI_VERSION], the reporepo URL, and the registry index are stable; every
    other input is resolved at build time. Useful when you want a one-file
    Dockerfile you can hand off without a recipe sidecar, accepting that "the
    build runs at docker-build time" rather than "the build replays a
    pre-computed plan". *)

val emit :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  proc_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  sys:D10.Sysops.t ->
  os_key:string ->
  cache:Oi.Cache.t ->
  data_dir:string ->
  session:D10.Sysops.Http.session ->
  platform:Osrel.t ->
  refresh:bool ->
  registry:string ->
  distro:Distro.t ->
  oi_version:string ->
  no_cache_mount:bool ->
  obuilder:bool ->
  output:string option ->
  targets:string list ->
  unit
(** [emit ~fs ~proc_mgr ~clock ~sys ~os_key ~cache ~data_dir ~session ~platform
     ~refresh ~registry ~distro ~oi_version ~no_cache_mount ~obuilder ~output
     ~targets] solves [targets] under [distro]'s os_key via
    [Build_pipeline.solve], collects unique archive shas from the merged
    [D10ir.Plan.t], and writes a single Dockerfile to
    [<output>/Dockerfile.oi-<slug>.<distro>] (or that name in the cwd when
    [output] is [None]). The plan is only used at generation time to derive the
    sha list embedded in the fetch step; the container re-solves at build time
    via [oi build TARGET] so no recipe sidecar is emitted.

    [oi_version] is passed through as the default for the [ARG OI_VERSION] in
    the emitted Dockerfile ([latest] resolves at docker-build time via the
    GitHub releases API). [registry] becomes the default [ARG OI_REGISTRY] so
    the archive URLs point at the user's configured mirror. *)
