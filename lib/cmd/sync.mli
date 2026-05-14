(** Project sync: install [*.opam] dependencies into [_oi/prefix/], install dev
    tools into [_oi/tools/], and (optionally) write [.envrc] for direnv.

    Library-only; the user-facing entry point is [oi build --deps-only] in
    [Build] and the auto-sync codepaths in [Exec] / [Env]. *)

val needs_sync : cwd:string -> prefix:string -> bool
(** [needs_sync ~cwd ~prefix] is [true] when [prefix] is missing or any [*.opam]
    in [cwd] has been modified more recently than [prefix]. *)

val resolve_project_toolchain :
  ?refresh:bool ->
  ?skip_local:bool ->
  ?with_repos:string list ->
  ?with_deps:string list ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  cache:Oi.Cache.t ->
  data_dir:string ->
  conf:Oi.Solver.Ctx.conf ->
  install:bool ->
  override:string option ->
  cwd:string ->
  unit ->
  Oi.Toolchain.info option
(** [resolve_project_toolchain] resolves the toolchain a project-aware command
    should use, with the same handle scope [oi sync] uses: project [x-repos:],
    URL-project overlays from [--with], and [--with-repo=@h] handles. Used by
    [oi exec] / [oi env] to pick the same toolchain [oi sync] would have,
    without doing a full sync. [install] controls whether non-relocatable
    toolchains get prepared on disk. *)

type envrc_mode = [ `Skip | `Always | `Detect ]
(** Controls [.envrc] writing during {!run}. [`Detect] (the default) writes
    [.envrc] only if [direnv] is on PATH; [`Skip] never writes; [`Always] writes
    regardless. *)

val envrc_mode_arg : envrc_mode Cmdliner.Term.t
(** [envrc_mode_arg] Cmdliner term for [--envrc=skip|always|detect]. Shared
    between [oi sync] and [oi build]. *)

val run :
  ?quiet:bool ->
  ?refresh:bool ->
  ?skip_local:bool ->
  ?with_repos:string list ->
  ?with_deps:string list ->
  ?jobs:int ->
  ?toolchain:string ->
  ?envrc_mode:envrc_mode ->
  ?with_test:bool ->
  proc_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  sys:D10.Sysops.t ->
  platform:Osrel.t ->
  os_key:string ->
  cache:Oi.Cache.t ->
  data_dir:string ->
  registry:string ->
  use_registry:Oi.Use_registry.t ->
  session:D10.Sysops.Http.session ->
  cwd:string ->
  unit ->
  string * Oi.Toolchain.info option
(** [run] Run a full sync in [cwd] and return the assembled [_oi/prefix/] path
    along with the resolved toolchain so callers can reuse it (e.g. [oi exec]
    reading env vars without re-resolving). [quiet] (default [false]) routes
    narration to [Logs.info] instead of stdout.

    A unified [Progress_ui] is opened internally when on a TTY (and not
    [quiet]); otherwise narration goes to [Logs.info]. *)
