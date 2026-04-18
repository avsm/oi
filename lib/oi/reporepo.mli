[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** A "reporepo" is an opam-layout directory where each package
    represents an overlay: a handle (the package name) points at a
    specific commit of an upstream opam repository. Dependencies
    between overlay packages express transitive overlay composition,
    giving a lockfile-like view of the full repo set needed by a
    consumer. oi never solves or builds packages from a reporepo;
    it mines the pinned URLs out of them and threads those as
    additional opam remotes into the normal solver. *)

type entry = {
  handle : string;
      (** Opam package name for the overlay (must be opam-valid, so
          no dots). *)
  version : string;
      (** Full opam version string, e.g. [20250418.0]. *)
  url : string;
      (** Upstream git URL (without commit fragment). *)
  commit : string;  (** 40-char commit sha that [url] is pinned to. *)
  ref_ : string option;
      (** Upstream git ref this entry tracks (typically a branch like
          [refs/heads/relocatable]). Used by [refresh] and [bump] to
          pick up new commits on the same branch rather than HEAD. *)
  depends : (string * string option) list;
      (** Other overlay handles this one depends on, optionally with
          an exact version. *)
  opam_path : string;  (** Absolute path to the source opam file. *)
}

val load : path:string -> entry list
(** [load ~path] walks [path/packages/*/*/opam] and parses every
    overlay package. Entries that don't have [x-oi-overlay: true] are
    skipped. Errors out on malformed overlay files. *)

val latest : entry list -> handle:string -> entry option
(** Highest-versioned entry for a given handle, or [None] if absent. *)

val find : entry list -> handle:string -> version:string -> entry option
(** Exact lookup. *)

type root = { handle : string; version : string option }

val resolve : entry list -> roots:root list -> entry list
(** Transitive closure in dependency order (deps before dependents).
    If a root has no [version], the highest version is picked. Raises
    {!Oi.Error.config_error} for unknown handles or unsatisfiable
    version pins. *)

val materialize :
  sys:D10.Sysops.t ->
  data_dir:string ->
  ?refresh:bool ->
  entry list ->
  string list
(** For each entry, ensure its upstream opam repo is cloned at the
    pinned commit under [{data_dir}/repos/overlay-<handle>-<version>/],
    refreshing per the normal [Repo.ensure] rules. Returns the list
    of [packages/] directories in the order supplied (which callers
    use to feed the solver; later entries in the list override
    earlier on name clashes by the solver's own first-wins rule). *)

(** {1 Base overlay resolution}

    Every $(b,oi) invocation needs a baseline set of opam repositories
    to resolve the compiler and the rest of the default universe.
    That baseline is modelled as an overlay named [relocatable] that
    depends (via the reporepo's [depends:]) on [default], which in
    turn depends on nothing. Both are user-managed: $(b,oi) never
    hard-codes upstream URLs. *)

val ensure_base :
  sys:D10.Sysops.t ->
  data_dir:string ->
  ?refresh:bool ->
  unit ->
  string list
(** Resolves the [relocatable] overlay (and its transitive deps) from
    the reporepo under [$OI_REPOREPO] / the default path, clones each
    at its pinned commit under [data_dir], and returns the list of
    [packages/] directories in solver priority order (dependents
    before deps so the solver's first-wins fold favours
    $(b,relocatable) over $(b,default)). Auto-clones the reporepo
    itself from {!default_url} (overridable via [$OI_REPOREPO_URL])
    if it doesn't already exist on disk. Raises
    {!Oi.Error.config_error} if the reporepo doesn't contain a
    [relocatable] entry. *)

val base_entries : unit -> entry list
(** Reads the reporepo under [$OI_REPOREPO] / the default path and
    returns the resolved base overlays without cloning. Useful for
    display in [oi config]. Empty list when the reporepo is missing
    or has no [relocatable] entry. *)

(** {1 Writing} *)

val default_path : string
(** The default location for the reporepo clone under the XDG data
    hierarchy ([$OI_DATA_DIR/reporepo], falling back to
    [$XDG_DATA_HOME/oi/reporepo] and then [~/.local/share/oi/reporepo]).
    Overridable with [$OI_REPOREPO] or a per-command [--reporepo] flag. *)

val default_url : string
(** The upstream git URL the reporepo is cloned from when none is set
    via [$OI_REPOREPO_URL] or a per-command [--reporepo-url] flag. *)

val ensure_clone : sys:D10.Sysops.t -> path:string -> url:string -> unit
(** Ensure a git working copy of [url] exists at [path]. A no-op when
    a [.git] directory is already present; an error when [path]
    exists but isn't empty and isn't a git clone. Never pulls once
    the clone exists — the user owns the working copy. *)

val add :
  sys:D10.Sysops.t ->
  path:string ->
  handle:string ->
  url:string ->
  ?ref_:string ->
  ?depends:(string * string option) list ->
  ?synopsis:string ->
  ?display_name:string ->
  ?origin_url:string ->
  unit ->
  entry
(** Create the first package for [handle] in the reporepo at [path].

    The commit is discovered via [git ls-remote URL REF]; defaults to
    [HEAD] (with [refs/heads/main] and [refs/heads/master] as
    fallbacks). Pass [~ref_:"relocatable"] (for example) to pin a
    non-default branch. The version is [YYYYMMDD.0] for today's date.

    When [depends] is omitted and [handle] is not itself a base overlay,
    the new entry auto-depends on the latest [relocatable] and
    [default] versions currently in the reporepo — that's how user
    overlays lock themselves against a specific base set. Pass
    [~depends:[]] to produce an entry with no deps. Raises if [handle]
    already has entries in the reporepo. *)

val bump :
  sys:D10.Sysops.t ->
  path:string ->
  handle:string ->
  ?url:string ->
  ?ref_:string ->
  ?depends:(string * string option) list ->
  unit ->
  [ `Bumped of entry | `Unchanged of entry ]
(** Consider creating a new version for [handle]. Re-[ls-remote]s
    either the supplied [url] or the one currently pinned by the
    highest existing version; [~ref_] targets a specific branch or
    ref (defaults to the one the previous version tracked, or
    [HEAD]).

    When [depends] is omitted and [handle] is a non-base overlay, the
    auto-injected base pins are refreshed to the latest [relocatable]
    / [default] versions in the reporepo (re-locking the user overlay
    against the current base set). Base overlays and overlays without
    any auto-injected base pin preserve their previous [depends]
    list.

    When the resolved [(url, commit, ref_, depends)] matches the
    previous version exactly, returns [`Unchanged prev] without
    writing anything. Otherwise writes a new [YYYYMMDD.N] entry (where
    [N] is the next free sequence on today's date) and returns
    [`Bumped new_entry]. Raises if [handle] is unknown. *)

val remove :
  path:string -> handle:string -> ?version:string -> unit -> unit
(** Delete a specific overlay version, or every version of a handle
    when [version] is omitted. *)

val ls_remote_sha :
  sys:D10.Sysops.t -> ?ref_:string -> string -> string
(** [ls_remote_sha ~sys ?ref_ url] runs [git ls-remote] to resolve a
    URL plus optional git ref to a 40-char commit sha. When [ref_] is
    omitted it tries [HEAD] and falls back to [refs/heads/main],
    [refs/heads/master], or any advertised ref in that order. Raises
    [Error.config_error] on failures. *)
