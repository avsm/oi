(** Higher-level locks for [oi]-shaped resources.

    Thin wrappers over {!D10.Lock} that pick the canonical lockfile path for
    each kind of shared state and call through with the project's defaults. Each
    lock comes in two forms: [acquire_X ~sw] for switch-scoped lifetime and
    [with_X] for block-scoped use. *)

val acquire_reporepo :
  ?mode:D10.Lock.mode ->
  ?strategy:D10.Lock.strategy ->
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  data_dir:string ->
  unit ->
  D10.Lock.t
(** [acquire_reporepo ~sw ~clock ~fs ~data_dir ()] is the switch-scoped variant
    of {!with_reporepo}. The lock releases when [sw] ends. *)

val with_reporepo :
  ?mode:D10.Lock.mode ->
  ?strategy:D10.Lock.strategy ->
  clock:_ Eio.Time.clock ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  data_dir:string ->
  (D10.Lock.t -> 'a) ->
  'a
(** [with_reporepo ~clock ~fs ~data_dir f] holds the reporepo lockfile at
    [<data_dir>/.reporepo.lock] (deliberately {b outside} [<data_dir>/reporepo/]
    so a [git clean -xfd] in the working tree never sweeps it). [oi repo bump],
    [oi repo add] etc. take {!D10.Lock.Exclusive}; [oi build] takes
    {!D10.Lock.Shared} so builds can run in parallel while a bump waits for them
    all. *)

val acquire_prefix :
  ?mode:D10.Lock.mode ->
  ?strategy:D10.Lock.strategy ->
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cache_root:string ->
  prefix_hash:string ->
  unit ->
  D10.Lock.t
(** [acquire_prefix ~sw ~clock ~fs ~cache_root ~prefix_hash ()] is the
    switch-scoped variant of {!with_prefix}. *)

val with_prefix :
  ?mode:D10.Lock.mode ->
  ?strategy:D10.Lock.strategy ->
  clock:_ Eio.Time.clock ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cache_root:string ->
  prefix_hash:string ->
  (D10.Lock.t -> 'a) ->
  'a
(** [with_prefix ~clock ~fs ~cache_root ~prefix_hash f] holds the per-prefix
    lockfile at [<cache>/prefixes/<prefix_hash>.lock].
    {!Pipeline.assemble_prefix} takes {!D10.Lock.Exclusive} while hardlinking
    layers; the double-check-after-acquire pattern (re-checking the [.ready]
    marker) skips the work entirely on a contested win. *)

val acquire_pin_set :
  ?mode:D10.Lock.mode ->
  ?strategy:D10.Lock.strategy ->
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cache_root:string ->
  set_id:string ->
  unit ->
  D10.Lock.t
(** [acquire_pin_set ~sw ~clock ~fs ~cache_root ~set_id ()] is the switch-scoped
    variant of {!with_pin_set}. *)

val with_pin_set :
  ?mode:D10.Lock.mode ->
  ?strategy:D10.Lock.strategy ->
  clock:_ Eio.Time.clock ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cache_root:string ->
  set_id:string ->
  (D10.Lock.t -> 'a) ->
  'a
(** [with_pin_set ~clock ~fs ~cache_root ~set_id f] holds the per-pin-set
    lockfile at [<cache>/pins/sets/<set_id>.lock]. {!Source.Pin.materialize}
    takes {!D10.Lock.Exclusive} while synthesising opam files for the pin set.
*)
