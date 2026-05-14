(** Per-invocation build manifest: one JSON document per [oi build] run.

    Stored at
    [<cache>/layers/<os_key>/builds/<YYYY>/<MM>/<YYYYMMDDTHHMMSSZ>-<ULID>.json];
    the same relative shape ships to any export / S3 prefix. *)

type reporepo = {
  url : string option;
  commit : string option;
  snapshot_key : string option;
}

type overlay_pin = {
  handle : string;
  version : string;
  commit : string option;
  url : string option;
}

type target = {
  kind : string;
  handle : string option;
  spec : string option;
  tokens : string list;
}

type solve = {
  solve_key : string option;
  schema : string option;
  from_cache : bool;
  resolved : Manifest_layer.dep list;
}

type summary = {
  ok : int;
  fail : int;
  timeout : int;
  skipped : int;
  cached : int;
  exit : int;
}

type t = {
  schema : int;
  kind : string;
  invocation_id : string;
  os_key : string;
  date : string;
  finished : string option;
  duration_s : float;
  reporepo : reporepo option;
  overlays : overlay_pin list;
  context : Audit.context;
  targets : target list;
  solve : solve option;
  events : Audit.event list;
  summary : summary;
  crashed : bool;
}

val codec : t Jsont.t
(** [codec] (de)serialises {!t} as JSON. *)

val v :
  invocation_id:string ->
  os_key:string ->
  started_at:float ->
  ?finished_at:float ->
  ?reporepo:reporepo ->
  ?overlays:overlay_pin list ->
  context:Audit.context ->
  ?targets:target list ->
  ?solve:solve ->
  events:Audit.event list ->
  ?crashed:bool ->
  ?exit_code:int ->
  unit ->
  t
(** [v ~invocation_id ~os_key ~started_at ?finished_at ?reporepo ?overlays
     ~context ?targets ?solve ~events ?crashed ?exit_code ()] constructs a build
    manifest from collected audit [events] plus identity inputs. The summary is
    derived from [events]. *)

val path_for :
  cache_root:string ->
  os_key:string ->
  ts:float ->
  invocation_id:string ->
  string
(** [path_for ~cache_root ~os_key ~ts ~invocation_id] is the on-disk path for
    the manifest at unix timestamp [ts] under [cache_root]. *)

val write : fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> t -> unit
(** [write ~fs ~cache_root m] persists [m] to [path_for ...]; creates parent
    dirs as needed. *)

val try_read : path:string -> t option
(** [try_read ~path] decodes a manifest at [path]; [None] on missing file or
    decode error. *)

val read_all_at : root:string -> t list
(** [read_all_at ~root] walks [root] recursively and decodes every [*.json]
    found. Used by the local audit reader to assemble the history of all
    invocations under a given builds-subtree. *)
