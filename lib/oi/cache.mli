(** OI caches (runs, cleanup, build logs, freshness).

    Source tarballs are cached by opam's download cache at
    [{data_dir}/opam-root/download-cache/]. This module manages the script run
    cache, the build-logs directory, sentinel-based freshness for pin and URL
    clones, and cleanup utilities. *)

type t
(** Handle for the on-disk cache rooted at a single directory (typically
    [~/.cache/oi/]). Constructed once at startup and threaded into every
    subsystem that may write into the cache. *)

val v : root:string -> Eio.Fs.dir_ty Eio.Path.t -> t
(** [v ~root fs] builds a {!t} pinned to [root]. Does not create the directory;
    callers create subdirs lazily as they're needed. *)

val pp : t Fmt.t
(** [pp ppf t] renders the cache root path. *)

val root : t -> Eio.Fs.dir_ty Eio.Path.t
(** Eio path to the cache root. *)

val root_s : t -> string
(** String form of {!root}, for env vars and APIs that take native paths. *)

val dune_root : t -> string
(** [dune_root t] is the directory used as the dune cache root ([<cache>/dune]).
    Threaded into the [DUNE_CACHE_ROOT] env var for child dune invocations so
    layer builds share one cache across [oi] runs. *)

val fs : t -> Eio.Fs.dir_ty Eio.Path.t
(** [fs t] is the underlying filesystem capability used to construct [t];
    exposed so modules that already thread a [Cache.t] don't need to duplicate
    the [fs] argument on every internal helper. *)

(** {1 Script run cache} *)

val run_dir : t -> hash:string -> Eio.Fs.dir_ty Eio.Path.t
(** [run_dir cache ~hash] is the per-script working directory
    [<cache>/runs/<hash>/]. {!Cmd.Script_runner} uses this for the synthetic
    dune project it builds the script under, keyed by the script's content hash
    so repeated runs reuse the compiled artefact. *)

(** {1 Pin-depends cache} *)

val pins_dir : t -> string
(** [pins_dir cache] is the root directory for pin-depends caches. Contains
    [sources/] (one dir per pin URL) and [sets/] (synthesized packages/ trees
    keyed by resolved pin-set hash). *)

(** {1 Toolchain install root} *)

val toolchains_root : unit -> string
(** [toolchains_root ()] is the XDG-derived directory under which fixed-prefix
    toolchains are installed ([$XDG_CACHE_HOME/oi/toolchains]). Independent of
    the cache root so {!Toolchain} can compute install prefixes without taking a
    [t]. *)

(** {1 Sentinel-based freshness}

    Shared by pin and URL-project clones. A sentinel file proves the cache entry
    was populated cleanly (partial fetches don't leave one behind). *)

val refresh_max_age : float
(** Seconds before a sentinel-guarded clone is considered stale. Default
    [86_400.0] (24h). *)

val fresh : refresh:bool -> sentinel:string -> max_age:float -> bool
(** [fresh ~refresh ~sentinel ~max_age] is [true] when a cache entry guarded by
    [sentinel] is still fresh: the sentinel exists and is younger than [max_age]
    seconds. Returns [false] unconditionally when [refresh] is [true]. *)

(** {1 Build logs}

    Diagnostic logs ([build], [solve], [fetch], [depext]) under
    [<cache_root>/build/logs/]. All writers are best-effort: I/O failures are
    swallowed so a full disk or permission error on the logs dir can't abort a
    build report. *)

module Logs : sig
  val dir : cache_root:string -> string
  (** [<cache_root>/build/logs] — the directory all log files live in. *)

  val ensure : fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> unit
  (** Create the logs directory if absent. Silent on any filesystem error. *)

  val short_hash : string -> string
  (** First 12 chars of a hash — used as a disambiguator in log filenames. *)

  val path :
    cache_root:string -> kind:string -> name:string -> hash:string -> string
  (** Canonical log file path:
      [<cache_root>/build/logs/<kind>-<name>-<short_hash hash>.log]. *)

  val write :
    fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> string -> string -> unit
  (** [write ~fs ~cache_root path content] writes [content] to [path]
      (truncating), creating the parent logs directory if needed. *)
end

(** {1 Cleanup} *)

type item = {
  label : string;
  path : Eio.Fs.dir_ty Eio.Path.t;
  description : string;
}

val items : t -> item list
(** [items t] lists subdirs under [<cache>/]. Swept by {!Stamp.cache_schema}
    bumps. *)

val data_items : t -> data_dir:string -> item list
(** [data_items t ~data_dir] lists subdirs under [<data>/] that are rebuildable
    cache (repos, opam-root). Swept by {!Stamp.data_schema} bumps. The reporepo
    is user-authored data and is NOT included. *)

val toolchain_items : t -> item list
(** [toolchain_items t] lists installed-toolchain dirs under
    [<cache>/toolchains/]. Swept by {!Stamp.toolchains_schema} bumps. *)

val cleanable_items : t -> data_dir:string -> item list
(** [cleanable_items t ~data_dir] is {!items} ++ {!data_items} ++
    {!toolchain_items}. The [oi clean] CLI iterates this. *)

val purge_items : item list -> unit
(** [purge_items xs] removes each item in [xs]; no logging. *)

val size : sys:D10.Sysops.t -> Eio.Fs.dir_ty Eio.Path.t -> int64
(** [size ~sys path] is the recursive on-disk size of [path] in bytes, computed
    via [du -sk] (with the result multiplied by 1024). Returns [0L] when the
    path doesn't exist or the subprocess errors out. *)

val pp_size : int64 Fmt.t
(** [pp_size ppf n] formats [n] as a human-readable byte count (e.g.
    ["1.2 GB"]). *)
