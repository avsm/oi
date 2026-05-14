(** Append-only build event log.

    Single jsonl file at [<cache>/build/audit.jsonl]. One line per terminal
    package event in any [oi build] (or [oi run]) invocation. Events carry
    caller-specific context (overlay handle, toolchain, project, trigger string)
    so the registry manifest can show the full set of callers that contributed
    to each layer rather than picking a single winner.

    Append is concurrency-safe under [O_APPEND] for line lengths well below
    [PIPE_BUF] (~4 KiB on Linux), which a single event easily fits within. *)

type context = {
  overlay : D10.Overlay.t option;
  toolchain : string option;
  trigger : string;  (** Argv summary of the [oi] invocation. *)
  project : string option;
  host : string;
}

type log_pointer = { text_path : string; tail : string option }

(** What the [layer_hash] field used to mean — disambiguated:

    - [Layer h] is the d10 layer hash this event is about. Used for every
      terminal package outcome (Ok/Cached/Restored/Build_failed/...).
    - [Solve_key h] is a stable digest of the solve-group inputs, used by
      [Solve_failed] events that have no layer to point at. The digest matches
      the failure log's filename so the UI can join them. *)
type event_target = Layer of string | Solve_key of string

type event = {
  schema : int;
  event_id : string;  (** ULID — sortable, dedupable. *)
  invocation_id : string;  (** Shared across every event from one [oi] run. *)
  ts : float;
  os_key : string;
  target : event_target;
  pkg : Identity.t;
  outcome : Outcome.t;
  duration_s : float;
  context : context;
  log : log_pointer option;  (** Present on failure events. *)
}

val hash_of_target : event_target -> string
(** [hash_of_target t] pulls the layer hash out of [t]. Returns the embedded
    string for both variants — when filtering to "real" layers, pattern-match on
    [t] directly instead. *)

(** {1 Codecs} *)

val event_codec : event Jsont.t

(** Leaf codecs are exposed so {!Manifest} can re-use them when encoding
    audit-derived shapes inside its envelope without restating the schema. *)

val log_pointer_codec : log_pointer Jsont.t

val context_codec : context Jsont.t
(** Exposed so {!Manifest_build} can embed it inside the per-invocation build
    manifest. *)

(** {1 Storage}

    Events are staged to a per-invocation JSONL file under
    [<cache>/registry-staging/<invocation_id>.events.jsonl] during the run, then
    rolled into a build manifest at
    [<cache>/registry/<os_key>/builds/<YYYY>/<MM>/<date>-<id>.json] by
    {!Manifest_build.write} when the invocation finalises. *)

val staging_path : cache_root:string -> invocation_id:string -> string
(** [staging_path ~cache_root ~invocation_id] is the per-invocation staging file
    path. *)

val append : fs:Eio.Fs.dir_ty Eio.Path.t -> cache_root:string -> event -> unit
(** [append ~fs ~cache_root e] appends [e] as a single JSON line to
    {!staging_path} (keyed by [e.invocation_id]). Errors are logged and
    swallowed so logging failure cannot abort the build. *)

val read_staged : cache_root:string -> invocation_id:string -> event list
(** [read_staged] returns the events written so far during a specific invocation
    (i.e. that haven't been rolled into a build manifest yet). Used by the
    finaliser. *)

val delete_staged : cache_root:string -> invocation_id:string -> unit
(** [delete_staged] removes the staging file for an invocation. Called after
    [Manifest_build.write] succeeds. *)

val staged_invocation_ids : cache_root:string -> string list
(** [staged_invocation_ids] lists invocation IDs whose staging files still
    exist. Used by the crash-recovery reaper. *)

(** {1 IDs and helpers} *)

val ulid : unit -> string
(** A Crockford-base32 ULID. 26 chars: 48-bit ms-since-epoch + 80-bit random. *)

val invocation_id : unit -> string
(** [invocation_id] Cached per-process ULID. Every event from a single [oi]
    invocation shares this id. *)

val default_context : unit -> context
(** [default_context] Construct a base [context] from the current process:
    [trigger] from [Sys.argv], [host] from [Unix.gethostname], and [overlay] /
    [toolchain] / [project] all left [None]. Producers fill in those three
    fields locally before calling {!append}. *)

val tail_of_file : ?lines:int -> path:string -> unit -> string option
(** [tail_of_file ?lines ~path ()] returns the last [lines] lines of [path]
    (default: 150), or [None] when the file is missing. Used to embed the
    failure tail directly in the audit event or to inline the tail of a
    failed-build log into a CI transcript. *)
