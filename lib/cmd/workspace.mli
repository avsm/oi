(** Working-directory queries for CLI commands.

    Helpers a command body uses once it has the Eio capabilities from
    {!Harness.bootstrap}: existence checks, the canonical absolute cwd, and
    detection of a project-local [_oi/tools/]. *)

val ocaml_version : string
(** Project-wide default OCaml version (currently [5.4.1]). *)

val app_name : string
(** Program name ([oi]). Used by {!Terms} for env-var prefixing and by the
    cmdliner program info. *)

val path_exists : Eio.Fs.dir_ty Eio.Path.t -> string -> bool
(** [path_exists fs path] is [true] iff [path] resolves to something Eio can
    stat (follows symlinks). *)

val resolved_cwd : Eio.Fs.dir_ty Eio.Path.t -> string * Eio.Fs.dir_ty Eio.Path.t
(** [resolved_cwd fs] is the canonical absolute path of the current working
    directory. Returns the string form (for env vars and opam APIs that take
    strings) and an [Eio.Path.t] rooted at [fs] (for filesystem operations).
    [Unix.realpath] up front avoids the relative ["."] that [Eio.Stdenv.cwd]
    would yield via [Eio.Path.native_exn]. *)

val tools_dir_for : cwd:string -> string option
(** [tools_dir_for ~cwd] is [Some path] when [cwd/_oi/tools/bin/] exists, so a
    caller can prepend it to PATH without adding a dangling directory. *)
