(** Locating the running [oi] binary and deciding where to install an updated
    one.

    Used by [oi self update]: probe the current executable path, check whether
    we can overwrite it, and otherwise fall back to a per-user [~/.local/bin/oi]
    install. *)

val current : unit -> string
(** [current ()] is the absolute path to the currently-running [oi] executable,
    with symlinks resolved. Backed by [/proc/self/exe] on Linux (the most
    reliable source — works even when invoked via [PATH] or via a hard / soft
    link) and by [Sys.executable_name] on macOS, which the OCaml runtime
    populates via [_NSGetExecutablePath].

    Falls through to [Sys.executable_name] when [/proc/self/exe] isn't
    available; that's set to [argv.(0)] as a last resort. *)

val is_writable : string -> bool
(** [is_writable path] is [true] when an open-for-write would succeed: either
    [path] exists and the user has write permission, or [path] is missing and
    its containing directory is writable. *)

(** {1 Install target} *)

type target =
  | In_place of string
      (** Overwrite the currently-running binary at this path. *)
  | Fallback of { current : string; install_dir : string }
      (** Current binary at [current] isn't writable; install into [install_dir]
          instead and warn the user. *)

val resolve_target : ?bin_dir:string -> unit -> target
(** [resolve_target ()] returns [In_place path] when {!current} is writable and
    [Fallback { current; install_dir }] otherwise. [bin_dir] overrides the
    default [~/.local/bin] fallback directory. *)
