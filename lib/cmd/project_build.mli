(** [oi build] / [oi test] project mode: drive the cwd's local [*.opam] build or
    test run.

    Sync the project's dependency closure into [_oi/prefix/], then run a dune
    target with that prefix on PATH. Dune handles inter-package ordering of
    sibling [*.opam] files itself. Non-dune projects only support [`Deps_only]
    for now. *)

type action = [ `Build | `Test | `Deps_only ]
(** [`Build] runs $(b,dune build --profile=release); [`Test] runs $(b,dune
    runtest --profile=release); [`Deps_only] stops after sync. *)

val run :
  action:action ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  proc_mgr:Eio_unix.Process.mgr_ty Eio.Resource.t ->
  clock:float Eio.Time.clock_ty Eio.Resource.t ->
  sys:D10.Sysops.t ->
  platform:Osrel.t ->
  os_key:string ->
  cache:Oi.Cache.t ->
  data_dir:string ->
  registry:string ->
  use_registry:Oi.Use_registry.t ->
  session:D10.Sysops.Http.session ->
  ?refresh:bool ->
  ?with_repos:string list ->
  ?with_deps:string list ->
  ?jobs:int ->
  ?toolchain:string ->
  ?envrc_mode:Sync.envrc_mode ->
  ?dry_run:bool ->
  ?dist:string ->
  cwd:string ->
  unit ->
  int
(** [run] sync's the project then runs the action's dune target (or stops after
    sync for [`Deps_only]). Returns the dune invocation's exit code (0 on
    success). Errors out via {!Oi.Error.E} when [cwd] has no [*.opam] files or
    no [dune-project]. *)

val depexts :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  platform:Osrel.t ->
  cache:Oi.Cache.t ->
  data_dir:string ->
  ?refresh:bool ->
  ?with_repos:string list ->
  ?with_deps:string list ->
  ?toolchain:string ->
  cwd:string ->
  unit ->
  int
(** [depexts ?refresh ?skip_local ?with_repos ?with_deps ?toolchain ~cwd ()]
    solves the project's dep closure (no build), then prints the union of
    [depexts:] declared by the solved packages, one per line. Backs
    [oi build --depext] in project mode. *)
