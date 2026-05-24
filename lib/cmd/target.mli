(** Parsing of the [TARGET] argument and [--with] / [--with-repo] tokens.

    Knows about the [@handle/pkg] shortcut (an overlay-scoped package spec),
    URL-vs-handle classification of [--with-repo] tokens, and the resolution of
    overlay handles into the {!Oi.Project.extra_repo} list the solver consumes.
*)

(** {1 Build target classification} *)

(** Kinds of "$(b,@handle)-prefixed target" [oi build] accepts. [oi run] only
    accepts the first two; [oi build] additionally understands [@handle] alone
    as "every package in the overlay". *)
type build_target =
  | Plain_target of string
  | Overlay_pkg of string * string  (** ([handle], [pkg_spec]) *)
  | Overlay_all of string  (** [handle] alone, expands to every overlay pkg *)

val parse_build_target : string -> build_target
(** [parse_build_target s] classifies [s]: bare [@handle] becomes [Overlay_all],
    [@handle/pkg…] becomes [Overlay_pkg], anything else is [Plain_target]. *)

val bare_handle : string -> string option
(** [bare_handle s] is [Some h] when [s] is a bare $(b,@h) handle (no $(b,/pkg)
    suffix and a well-formed handle); [None] otherwise. *)

(** {1 Handle pins}

    A [@handle/pkg[constr]] shortcut once the handle has been routed into
    [with_repos] and the package spec is ready for the solver. Carries the
    handle alongside the package name and any user-supplied constraint so we can
    later pin the package to whatever version the named overlay ships. *)

type handle_pin = {
  handle : string;
  pkg : OpamPackage.Name.t;
  user_constr : OpamFormula.version_constraint option;
}

val split_handle_prefix : string -> (string * string) option
(** [split_handle_prefix s] is [Some (handle, pkg)] for tokens of the form
    [@handle/pkg…]; [None] otherwise. Raises {!Oi.Error.fail_config_error} when
    [@handle] is given without a package. *)

val extract_handle_pins :
  with_repos:string list ->
  string list ->
  string list * string list * handle_pin list
(** [extract_handle_pins ~with_repos toks] strips [@handle/pkg] prefixes from
    [toks]. Each hit routes [handle] into [with_repos], replaces the token with
    its stripped form, and records a {!handle_pin} so the caller can pin the
    package to the overlay's version. Returns
    [(stripped_tokens, updated_with_repos, handle_pins)]. *)

val pin_handles : handle_pin list -> string list
(** [pin_handles pins] projects [pins] down to their handle strings. Useful for
    feeding the [@h/pkg] half of [--with] / [TARGET] tokens into
    {!Oi.Pipeline.pick_toolchain}. *)

val handle_pin_constraints :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  data_dir:string ->
  refresh:bool ->
  cli_extras:Oi.Project.extra_repo list ->
  handle_pin list ->
  OpamFormula.version_constraint OpamPackage.Name.Map.t
(** [handle_pin_constraints ~fs ~data_dir ~refresh ~cli_extras pins] turns
    [pins] into an [= VERSION] constraint map suitable for merging into the
    solver's [constraints] argument. A pin without an explicit user constraint
    is pinned to the highest version the overlay ships. [cli_extras] must be
    fully materialised on disk before this runs. *)

(** {1 Extras (--with-repo + project [x-repos:])} *)

val is_url_like : string -> bool
(** [is_url_like s] is [true] when [s] has a scheme-like prefix or contains a
    path separator (treated as a URL); [false] means [s] is an overlay handle.
*)

val handles_of_tokens : string list -> string list
(** [handles_of_tokens toks] keeps only the handle-shaped entries (those for
    which {!is_url_like} returns [false]). Used by every solving command to
    derive the handle list fed into {!Oi.Pipeline.pick_toolchain}. *)

val cli_extra_repos :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  ?toolchain:Oi.Toolchain.info ->
  string list ->
  Oi.Project.extra_repo list
(** [cli_extra_repos ~fs ~sys ?toolchain toks] resolves [toks] (a mix of URLs
    and reporepo handles) into the flat extra-repo list the solver wants. Later
    handles in the input list are given highest priority.

    When [?toolchain] is set, handle resolution is direct (each handle's latest
    reporepo entry only) — the toolchain's [info.packages_dirs] already supplies
    the base overlays, so transitively closing each explicit handle would put
    two different commits of the same base repo (e.g. [default]) into the
    solver's search path and trigger [conflict-class] failures on
    [ocaml-core-compiler]. *)

val merge_extras :
  cli:Oi.Project.extra_repo list ->
  project:Oi.Project.extra_repo list ->
  Oi.Project.extra_repo list
(** [merge_extras ~cli ~project] merges CLI [--with-repo] URLs and
    project-declared extras. Project entries win on a name collision. *)

(** {1 Misc} *)

val parse_pkg_target :
  string ->
  OpamPackage.Name.t * (OpamFormula.relop * OpamPackage.Version.t) option
(** [parse_pkg_target s] parses [s] as either ["name"], ["name.version"], or an
    opam atom like ["name>=1.0"] / ["name=1.0"]. Returns the bare name and an
    optional version constraint for the solver. *)
