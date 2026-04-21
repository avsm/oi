(** OS-abstracted system operations.

    Wraps external tools (tar, git) with OS-specific tool selection. Tool paths
    are resolved once at {!create} time: for example, [gtar] is preferred over
    [tar] on macOS.

    All local filesystem paths are {!Eio.Path.t} values. Source fetching,
    checksum verification, and downloads are handled by opam's repository
    libraries (see {!Oi.Fetch}). *)

(** {1 Initialisation} *)

type t
(** System operations context with pre-resolved tool paths. *)

val create :
  ?stdout:_ Eio.Flow.sink ->
  ?stderr:_ Eio.Flow.sink ->
  proc_mgr:_ Eio.Process.mgr ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  unit ->
  t
(** [create ?stdout ?stderr ~proc_mgr ~fs ()] detects tool paths (tar variant)
    by probing the system via [which]. Pass the parent's [stdout] and [stderr]
    to enable {!Cmd.run_inherit}, which streams subprocess output to the user's
    terminal. Call once at startup. *)

(** {1 File queries} *)

val file_exists : _ Eio.Path.t -> bool
(** [file_exists path] is [true] if [path] exists (follows symlinks). *)

(** {1 File copying} *)

val copy_tree : t -> src:_ Eio.Path.t -> dst:_ Eio.Path.t -> unit
(** [copy_tree t ~src ~dst] copies a directory tree. Attempts CoW clone
    ([cp -ac]) first (zero-copy on APFS), falling back to [cp -a]. *)

val link_tree : t -> src:_ Eio.Path.t -> dst:_ Eio.Path.t -> unit
(** [link_tree t ~src ~dst] hardlinks all files from [src] into [dst]
    recursively via [cp -Rfl]. Tolerates "identical file" errors. *)

(** {1 Archive operations} *)

module Tar : sig
  val extract :
    t -> archive:_ Eio.Path.t -> dst:_ Eio.Path.t -> ?strip:int -> unit -> unit
  (** [extract t ~archive ~dst ?strip ()] extracts [archive] into [dst]. Uses
      [gtar] on macOS if available. *)

  val create_zstd : t -> src:_ Eio.Path.t -> dst:_ Eio.Path.t -> unit
  (** [create_zstd t ~src ~dst] creates a zstd-compressed tar archive at [dst]
      from the contents of directory [src]. *)
end

module Curl : sig
  val fetch : t -> url:string -> dst:_ Eio.Path.t -> bool
  (** [fetch t ~url ~dst] downloads [url] to [dst]. Returns [true] on success,
      [false] on any error (404, network failure, missing curl). *)
end

(** {1 Low-level command execution} *)

module Cmd : sig
  val run : t -> string list -> unit
  (** [run t args] executes [args] as a subprocess, raising [Failure] on
      non-zero exit. Stdout and stderr are captured silently. *)

  val run_out : t -> string list -> string
  (** [run_out t args] executes [args] and returns its trimmed stdout. *)

  val run_inherit : t -> string list -> unit
  (** Like {!run} but the subprocess inherits the parent's stdout and stderr so
      any progress or error output is shown to the user as the command runs. The
      failure message is short ("git exited 128") because git's own output
      already explained the problem. Requires [t] to have been created with
      [~stdout] / [~stderr]; falls back silently to {!run} when they aren't set.
  *)
end

(** {1 Git operations} *)

module Git : sig
  val head_short : t -> dir:_ Eio.Path.t -> string
  (** [head_short t ~dir] returns the short abbreviated hash of HEAD. *)
end
