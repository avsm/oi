[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Minimal dune-project reader/writer for [oi add] and friends. *)

type t
(** Parsed representation of a single dune-project file. *)

val load : fs:Eio.Fs.dir_ty Eio.Path.t -> cwd:string -> t
(** [load ~fs ~cwd] parses [cwd/dune-project]. Raises {!Error.config_error} when
    the file is missing or malformed. *)

val generate_opam_files : t -> bool
(** [true] iff the file contains [(generate_opam_files)] or
    [(generate_opam_files true)]. False for [(generate_opam_files false)] or an
    absent stanza. *)

val package_names : t -> string list
(** Names of every [(package (name X) …)] stanza in the file, in declaration
    order. *)

val add_dependency :
  t ->
  ?package:string ->
  name:string ->
  constraint_:(string * string) option ->
  unit ->
  t
(** [add_dependency t ?package ~name ~constraint_] appends a dep to the target
    [(package …)]'s [(depends …)] list.

    - If [package] is [Some p], the stanza with [(name p)] is used.
    - If [package] is [None] and there is exactly one [(package …)] stanza, that
      one is used.
    - If [package] is [None] and there are multiple [(package …)] stanzas,
      {!Error.config_error} is raised asking for an explicit [-p PKG].
    - If the chosen stanza has no [(depends …)] form, one is created.

    [constraint_], when provided, is an [(op, version)] pair — e.g.
    [Some (">=", "0.9")] renders as [(name (>= 0.9))]. When [None], the dep is
    appended as a bare atom.

    Idempotent on package name: if an entry for [name] is already present (with
    or without a constraint), [t] is returned unchanged. *)

val save : fs:Eio.Fs.dir_ty Eio.Path.t -> t -> unit
(** [save ~fs t] pretty-prints [t] back to disk, overwriting the file it was
    loaded from. *)
