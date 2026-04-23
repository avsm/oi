(** Helpers for writing build / solve / fetch diagnostic logs under
    [<cache_root>/build/logs/]. *)

val dir : cache_root:string -> string
(** [<cache_root>/build/logs] — the directory all registry-build log
    files live in. *)

val ensure : fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> unit
(** Create the logs directory if absent. No-op when it already exists;
    silent on any filesystem error. *)

val short_hash : string -> string
(** First 12 chars of a hash (hex-digest or layer-hash) — used as a
    disambiguator in log filenames. *)

val path :
  cache_root:string -> kind:string -> name:string -> hash:string -> string
(** Canonical log file path:
    [<cache_root>/build/logs/<kind>-<name>-<short_hash hash>.log].
    [kind] is a short tag (e.g. ["build"], ["fetch"], ["solve"]);
    [name] is the package or target; [hash] disambiguates multiple
    log files that share a name but come from different contexts. *)

val write :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cache_root:string ->
  string ->
  string ->
  unit
(** [write ~fs ~cache_root path content] writes [content] to [path]
    (truncating), creating the parent logs directory if needed. *)
