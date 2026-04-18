[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

(** Stage-based parallel build executor.

    Executes a {!Plan.t} by processing stages sequentially. Within each stage,
    fetch and build run in parallel via Eio fibers. Install is serialised at
    stage boundaries. Uses {!Installer} to process [.install] files directly
    via the opam libraries (no external [opam-installer] binary).

    Layers are captured via {!D10.Prefix.diff} and stored in the d10 cache for
    future reuse. *)

val run :
  ?cache_urls:OpamUrl.t list ->
  proc_mgr:_ Eio.Process.mgr ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:D10.Config.clk ->
  sys:D10.Sysops.t ->
  os_key:string ->
  Plan.t ->
  unit
(** [run ~proc_mgr ~fs ~clock ~sys ~os_key plan] executes the build plan.
    Reports per-package failures to stderr and exits with code 1 if any package
    fails.

    [cache_urls] are passed through to [OpamRepository.pull_tree] /
    [pull_file] for every fetch so that opam probes local and/or remote
    source mirrors (e.g. {!Source_mirror.url}, {!Source_mirror.remote_url})
    before falling back to the upstream URL. *)
