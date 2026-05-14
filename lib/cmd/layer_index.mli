(** Local + remote layer-index plumbing for [oi run] and [oi search].

    The local half maintains an on-disk SQLite
    [{cache_root}/layers/{os_key}/index.db] built from the layers in the local
    cache — used to answer [oi run BARE_BINARY] and [oi search] when the
    registry is unreachable.

    The remote half consults the registry's published
    [{registry}/{os_key}/index-full.json] (produced by the Clickhouse query in
    [foo.sql]); there is no remote SQLite. *)

val remote_index_max_age : float
(** [remote_index_max_age] kept for backwards-compatibility with [oi config]'s
    display; no longer drives any cache TTL. *)

val url_join : string -> string -> string
(** [url_join base rel] joins a registry base URL and a relative path with a
    single ['/'] regardless of whether [base] has a trailing slash. *)

val ensure_local :
  sys:D10.Sysops.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:D10.Config.clk ->
  cache:Oi.Cache.t ->
  os_key:string ->
  string
(** [ensure_local ~sys ~fs ~clock ~cache ~os_key] builds or refreshes the local
    SQLite index at [{cache_root}/layers/{os_key}/index.db] and returns its
    path. Cheap staleness check rebuilds when the [layers/{os_key}/] directory
    count exceeds the indexed-row count. *)

val package_of_binary :
  ?on_phase:(string -> unit) ->
  sys:D10.Sysops.t ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  clock:_ Eio.Time.clock ->
  session:D10.Sysops.Http.session ->
  cache:Oi.Cache.t ->
  os_key:string ->
  registry:string ->
  string ->
  (string * string option) option
(** [package_of_binary ?on_phase ~sys ~fs ~clock ~session ~cache ~os_key
     ~registry name] returns [Some (pkg, overlay_handle)] when the local cache
    or the registry's [index-full.json] has a layer that ships [bin/name]. Local
    hits are returned first; the registry is consulted only when there is no
    local match. [on_phase], if supplied, receives short status strings while
    the registry fetch is in flight. *)

val remote_search_binary :
  pattern:string ->
  D10.Remote_index.index_full ->
  (string * string * string * string * D10.Overlay.t option) list
(** [remote_search_binary ~pattern full] linearly scans [full] for binaries
    whose name matches [pattern] (with [*] as wildcard) and returns
    [(binary, package_name, package_ver, hash, overlay)] rows sorted by binary,
    then package, then version desc. Mirrors the shape [D10.Index.search_binary]
    returns. *)

val remote_search_package :
  pattern:string ->
  D10.Remote_index.index_full ->
  (string * string * string * D10.Overlay.t option) list
(** [remote_search_package ~pattern full] scans [full] for packages whose name
    matches [pattern] and returns [(name, ver, hash, overlay)] sorted by name,
    then version desc. Mirrors [D10.Index.search_package]. *)
