(** Cross-process advisory locking for the d10 cache.

    Backed by [Unix.lockf] (POSIX fcntl record locks). The lock is held while a
    file descriptor to the lockfile remains open; it is released on explicit
    {!release}, on the surrounding {!with_lock}'s exit (success or exception),
    and on process termination. A crashed [oi] process therefore never strands a
    held lock.

    {1 What gets locked}

    Lockfiles live next to the resource they protect:

    - [<cache>/layers/<os_key>/<hash>.lock] — protects the layer at
      [<cache>/layers/<os_key>/<hash>/] and its sidecar at
      [<cache>/layers/<os_key>/<hash>.json].
    - [<cache>/d10ir/archives/<sha>.lock] — protects
      [<cache>/d10ir/archives/<sha>.{tar.zst,json}].
    - [<cache>/layers/<os_key>/.registry.lock] — protects the schema-version
      pointer at [<cache>/layers/<os_key>/registry.json].

    The files themselves are tiny zero-byte sentinels created on first use; we
    record the holder's pid into them as a debug aid.

    {1 Modes}

    Multiple {!Shared} holders coexist. An {!Exclusive} holder excludes every
    other holder of any mode. The typical pattern is:

    - Writers ({!Layer.store}, {!Registry.pull}, [oi repo bump]) take
      {!Exclusive}.
    - Readers that need a stable view ({!Layer.export}, prefix assembly) take
      {!Shared}.
    - Pure reads (browsing layer dirs from inside [oi cache show]) do not bother
      — they accept the small race.

    {1 Limitations}

    - Advisory: code that bypasses these helpers and writes the resource
      directly can still corrupt it. Treat any direct [d10.fs] write as a bug.
    - Local-FS only. [Unix.lockf] over NFS is fragile in mixed-version clusters;
      this is an unsupported configuration.
    - Not re-entrant. A fiber that already holds [lock(R)] must not try to
      acquire it again from a nested call. *)

type mode = Shared | Exclusive

type strategy =
  | Block
      (** Wait indefinitely. Status logged every [log_interval_s] seconds via
          [on_wait]. *)
  | Block_timeout of float
      (** Wait up to N seconds; raise {!Lock_unavailable} on timeout. *)
  | No_wait
      (** Fail immediately with {!Lock_unavailable} if the lock is held. *)

type t
(** A held lock. Owned by the calling fiber until {!release} fires (manually or
    via {!with_lock}'s [Fun.protect]). *)

exception Lock_unavailable of { path : string; held_by_pid : int option }

val release : t -> unit
(** [release t] drops the lock and closes the file descriptor. Idempotent —
    calling twice is safe. *)

val path : t -> string
(** [path t] is the lockfile path the [t] is holding. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints the lockfile path. *)

val acquire :
  ?mode:mode ->
  ?strategy:strategy ->
  ?log_interval_s:float ->
  ?on_wait:(waited_s:float -> path:string -> pid:int option -> unit) ->
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  fs:_ Eio.Path.t ->
  path:string ->
  unit ->
  t
(** [acquire ~sw ~clock ~fs ~path ()] acquires the lockfile at [path] in [mode]
    (default {!Exclusive}) under [strategy] (default {!Block}) and returns the
    held lock. The lock is automatically released when [sw] ends (normal
    completion or cancellation); call {!release} to release it earlier.

    Use this when the lock's lifetime spans several function calls, when you
    need to hold multiple locks side-by-side in one scope, or when integrating
    with other Eio resources that already accept a [~sw].

    [on_wait] (default no-op) is invoked at most every [log_interval_s] seconds
    (default 5.0) while blocked, with the elapsed wait time and the pid recorded
    in the lockfile (if any).

    @raise Lock_unavailable per [strategy]. *)

val with_lock :
  ?mode:mode ->
  ?strategy:strategy ->
  ?log_interval_s:float ->
  ?on_wait:(waited_s:float -> path:string -> pid:int option -> unit) ->
  clock:_ Eio.Time.clock ->
  fs:_ Eio.Path.t ->
  path:string ->
  (t -> 'a) ->
  'a
(** [with_lock ~clock ~fs ~path f] is the block-scoped wrapper around
    {!acquire}: it opens a fresh switch, acquires, runs [f t], and releases on
    [f]'s return or exception. Equivalent to:

    {[
    Eio.Switch.run @@ fun sw ->
    let t = acquire ~sw ~clock ~fs ~path () in
    Fun.protect ~finally:(fun () -> release t) (fun () -> f t)
    ]}

    Prefer this for one-shot acquisitions; reach for {!acquire} when the lock
    needs to outlive a single block. *)

(** {1 Resource-specific helpers}

    These call {!with_lock} with the canonical paths under {!Config.t}. *)

val acquire_layer :
  Config.t ->
  sw:Eio.Switch.t ->
  hash:string ->
  ?mode:mode ->
  ?strategy:strategy ->
  ?log_interval_s:float ->
  ?on_wait:(waited_s:float -> path:string -> pid:int option -> unit) ->
  unit ->
  t
(** [acquire_layer c ~sw ~hash ()] is {!acquire} on the lockfile at
    [<cache>/layers/<os_key>/<hash>.lock]. *)

val with_layer :
  Config.t ->
  hash:string ->
  ?mode:mode ->
  ?strategy:strategy ->
  ?log_interval_s:float ->
  ?on_wait:(waited_s:float -> path:string -> pid:int option -> unit) ->
  (t -> 'a) ->
  'a
(** [with_layer c ~hash f] is the block-scoped form of {!acquire_layer}. *)

val acquire_archive :
  Config.t ->
  sw:Eio.Switch.t ->
  sha:string ->
  ?mode:mode ->
  ?strategy:strategy ->
  ?log_interval_s:float ->
  ?on_wait:(waited_s:float -> path:string -> pid:int option -> unit) ->
  unit ->
  t
(** [acquire_archive c ~sw ~sha ()] is {!acquire} on the lockfile at
    [<cache>/d10ir/archives/<sha>.lock]. *)

val with_archive :
  Config.t ->
  sha:string ->
  ?mode:mode ->
  ?strategy:strategy ->
  ?log_interval_s:float ->
  ?on_wait:(waited_s:float -> path:string -> pid:int option -> unit) ->
  (t -> 'a) ->
  'a
(** [with_archive c ~sha f] is the block-scoped form of {!acquire_archive}. *)

val acquire_registry_pointer :
  Config.t ->
  sw:Eio.Switch.t ->
  ?mode:mode ->
  ?strategy:strategy ->
  ?log_interval_s:float ->
  ?on_wait:(waited_s:float -> path:string -> pid:int option -> unit) ->
  unit ->
  t
(** [acquire_registry_pointer c ~sw ()] is {!acquire} on the lockfile at
    [<cache>/layers/<os_key>/.registry.lock]. *)

val with_registry_pointer :
  Config.t ->
  ?mode:mode ->
  ?strategy:strategy ->
  ?log_interval_s:float ->
  ?on_wait:(waited_s:float -> path:string -> pid:int option -> unit) ->
  (t -> 'a) ->
  'a
(** [with_registry_pointer c f] is the block-scoped form of
    {!acquire_registry_pointer}. *)
