(** Reporepo: an overlay-of-overlays git repo whose entries register named
    upstream opam repositories ("handles") plus toolchain definitions.

    A reporepo entry is one opam file under [v2/<handle>/packages/<handle>/]
    that records the handle's git URL, pinned commit, declared toolchain, and
    transitive [depends:] on other handles. The reporepo itself is a regular git
    repository the user clones (or oi auto-clones) once; subsequent
    [oi repo bump] commits update entries in place.

    This module exposes the entry parser, the resolution walk that produces
    [packages/] dirs in solver-priority order, and the push helpers used by
    [oi repo add] / [oi repo bump]. It is the only library code that reads or
    writes the v2 layout — every other consumer goes through the resolved
    [entry list]. Re-exported as [Source.Reporepo]. *)

type entry = {
  handle : string;
      (** Opam package name for the overlay (must be opam-valid, so no dots). *)
  version : string;  (** Full opam version string, e.g. [20250418.0]. *)
  url : string;
      (** Upstream git URL (without commit fragment). Empty for definition-only
          entries that compose existing overlays via [depends:] without their
          own clone. *)
  commit : string;
      (** 40-char commit sha that [url] is pinned to. Empty when [url] is empty.
      *)
  ref_ : string option;
      (** Upstream git ref this entry tracks (typically a branch like
          [refs/heads/relocatable]). *)
  toolchain : string option;
      (** [x-oi-toolchain]: this overlay targets the named toolchain (use-site
          relationship). *)
  toolchain_name : string option;
      (** [x-oi-toolchain-name]: when set, this entry DEFINES a toolchain with
          the given CLI name. The reporepo handle and the toolchain CLI name
          live in separate namespaces. *)
  toolchain_compiler : string option;
      (** [x-oi-toolchain-compiler]: the primary compiler package spec, e.g.
          ["ocaml-variants.5.2.0+ox"]. Only meaningful when {!toolchain_name} is
          set. *)
  relocatable : bool option;
      (** [x-oi-relocatable]: build mode for the toolchain this entry defines.
      *)
  toolchain_roots : string list list;
      (** [x-oi-toolchain-roots]: solver root specs for the toolchain. *)
  toolchain_tools : string list;
      (** [x-oi-toolchain-tools]: package names that [oi build] should always
          install when this toolchain is active (typically dev tools like
          [odoc], [merlin], [ocaml-lsp-server]). Empty for non-toolchain entries
          and toolchains that don't ship a default tool set. *)
  default_toolchain : bool;
      (** [x-oi-default-toolchain]: when [true], this toolchain is selected when
          no [--toolchain] is passed on the CLI. {!load} validates that at most
          one toolchain handle in the reporepo carries this flag. *)
  depends : (string * string option) list;
      (** Other overlay handles this one depends on, optionally with an exact
          version. *)
  root_packages : string list list;
      (** Package sets to pre-build when priming this overlay into a registry.
          Each outer entry is a solve group: a list of package specs fed to the
          solver together. *)
  opam_path : string;  (** Absolute path to the source opam file. *)
}

val load : path:string -> entry list
(** [load ~path] walks [path/packages/*/*/opam] and parses every overlay
    package. Entries that don't have [x-oi-overlay: true] are skipped. *)

val latest : entry list -> handle:string -> entry option
(** Highest-versioned entry for a given handle. *)

val default_toolchain : entry list -> entry option
(** [default_toolchain] Latest version of the toolchain definition flagged
    [x-oi-default-toolchain: true], or [None] if no entry carries the flag.
    {!load} guarantees at most one toolchain handle is so flagged. *)

type root = { handle : string; version : string option }

val resolve : entry list -> roots:root list -> entry list
(** [resolve] Transitive closure in dependency order (deps before dependents).
    If a root has no [version], the highest version is picked. *)

(** {2 v1 overlay layout}

    The reporepo's [v2/] subtree carries one fully-materialised opam repository
    per handle. Each handle's [opam] files have had every [url{}] block
    rewritten to be content-addressed (sha-pinned git URL or tarball+checksum),
    turning the reporepo into a deterministic snapshot of every overlay's
    package data. The [v1] prefix is a schema marker; a future incompatible
    change becomes [v2]. *)

val iter_opam_files :
  path:string ->
  ?include_handles:string list ->
  ?skip_handles:string list ->
  (handle:string -> pkg:string -> version:string -> opam_path:string -> unit) ->
  unit
(** [iter_opam_files] Visit every
    [<path>/v2/<handle>/packages/<pkg>/<pkg.version>/opam] in the reporepo.
    Empty [include_handles] means every overlay (including [default]);
    [skip_handles] is applied last. The meta-overlay [reporepo] is always
    skipped — it holds handle-registration entries, not archives. *)

val overlay_packages_dir : path:string -> handle:string -> string
(** [overlay_packages_dir] — directly consumable as a solver [packages_dir]. *)

val assert_overlay_dir : path:string -> handle:string -> string
(** [assert_overlay_dir] Like {!overlay_packages_dir}, but errors with a "run
    [oi repo bump]" hint when the directory is missing on disk. *)

type materialise_summary = {
  handle : string;
  total : int;  (** number of packages walked *)
  kept : int;  (** url already content-addressed *)
  rewrote : int;  (** url rewritten to a sha-pinned form *)
  unavailable : (string * string) list;
      (** [(pkg.version, reason)] for entries marked [available: false]. *)
}

val materialise_handle :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  path:string ->
  handle:string ->
  url:string ->
  commit:string ->
  materialise_summary
(** [materialise_handle] Clone [url] at [commit] into a scratch directory, walk
    every [packages/<pkg>/<pkg>.<ver>/opam] file, rewrite each [url{}] block and
    pin-depends entry to a content-addressed form, and write the result to
    [<reporepo>/v2/<handle>/]. The write is atomic from the caller's POV: a
    sibling [v2/<handle>.tmp/] is built first, then rotated into place at the
    end.

    Hard-errors on unsupported VCS backends (darcs, hg, rsync). Persistent
    network failures (git ls-remote unreachable, tarball 404 after retry) mark
    the offending package [available: false] with an [x-oi-unavailable:]
    explanation rather than failing the whole bump. *)

val try_resolve_url :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  where:string ->
  OpamUrl.t ->
  has_checksum:bool ->
  [ `Keep
  | `Replace_url of OpamUrl.t
  | `Add_checksum of OpamHash.t
  | `Failed of string ]
(** [try_resolve_url ~fs ~sys ~where url ~has_checksum] performs the same
    content-address resolution as the per-overlay materialiser does for one
    [url{}] block: applies host rewrites, returns [`Keep] when the URL is
    already pinned, [`Replace_url] for a sha-pinned/host-rewritten git URL,
    [`Add_checksum] for a tarball whose sha-256 we just computed, or [`Failed]
    when the URL cannot be content-addressed. [where] is a free-form label used
    in error messages. *)

(** {2 Base overlay resolution} *)

val ensure_base :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  data_dir:string ->
  ?refresh:bool ->
  ?reporter:Build_progress.reporter ->
  unit ->
  string list
(** [ensure_base] Resolves the [relocatable] overlay (and its transitive deps)
    from the reporepo and returns the [packages/] directories under
    [v2/<handle>/] in solver priority order. Auto-clones the reporepo itself if
    it doesn't already exist on disk. Errors when any base handle's [v2/] tree
    is missing — run [oi repo bump <handle>] to populate.

    When [refresh] is [false] (the default), the clone is auto-pulled if its
    last [git fetch]/[git pull] happened more than 1 hour ago — measured from
    the mtime of [.git/FETCH_HEAD] (falling back to [.git/HEAD] for a fresh
    clone that hasn't been fetched yet). Override the threshold via
    [OI_REPOREPO_MAX_AGE] (positive float seconds; non-positive disables). *)

val base_entries : unit -> entry list
(** [base_entries] Resolved base overlays without cloning. Useful for display in
    [oi config]. Empty list when the reporepo is missing or has no [relocatable]
    entry. *)

(** {2 Paths and bootstrapping} *)

val default_path : string
(** [default_path] , falling back to [$XDG_DATA_HOME/oi/reporepo] and then
    [~/.local/share/oi/reporepo]. *)

val env_path : unit -> string
(** [env_path] when set and non-empty, otherwise {!default_path}. *)

val default_url : string
(** [default_url] Built-in upstream reporepo URL used when [OI_REPOREPO_URL] is
    unset. *)

val env_url : unit -> string
(** [env_url] when set and non-empty, otherwise {!default_url}. *)

val ensure_clone :
  ?reporter:Build_progress.reporter ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  refresh:bool ->
  path:string ->
  url:string ->
  unit ->
  unit
(** [ensure_clone ~fs ~sys ~refresh ~path ~url ()] clones or refreshes the
    reporepo. [?reporter] receives a [Status] event when the clone or refresh
    runs. *)

val set_push_url : sys:D10.Sysops.t -> path:string -> string -> unit
(** [set_push_url ~sys ~path url] sets [git remote set-url --push origin url] on
    the reporepo clone at [path]. Used when a user clones from a read-only
    mirror but wants [oi repo push] to go to a writable upstream. *)

type push_step =
  | Step_commit of { files : string list }
  | Step_pull of { commits : int }
  | Step_push of { commits : int }

type push_outcome = push_step list

val push :
  ?on_step_start:(int -> string -> unit) ->
  sys:D10.Sysops.t ->
  path:string ->
  unit ->
  push_outcome
(** [push] stages and commits any uncommitted changes, [git pull --rebase] to
    bring in upstream history, then [git push] if local is ahead. *)

val commit_dirty :
  sys:D10.Sysops.t -> path:string -> msg:string -> unit -> string list
(** [commit_dirty] Stage and commit any uncommitted changes in the reporepo at
    [path] with commit message [msg]. Returns the list of files that were
    committed (empty when the working tree is already clean, and no commit is
    created in that case). *)

val add :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  path:string ->
  handle:string ->
  url:string ->
  ?ref_:string ->
  ?toolchain:string ->
  ?base_handles:string list ->
  ?depends:(string * string option) list ->
  ?root_packages:string list list ->
  ?synopsis:string ->
  ?force:bool ->
  unit ->
  entry
(** [add] Create a new reporepo entry for [handle] tracking the upstream [url]
    at optional [ref_]. Writes the freshly resolved opam metadata under
    [v2/<handle>/packages/<handle>/<handle>.<ver>/] and returns the parsed
    {!entry}. Errors when an entry for [handle] already exists unless
    [?force:true]; the caller is expected to follow up with {!bump} for schema
    changes rather than re-adding. *)

val bump :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  path:string ->
  handle:string ->
  ?url:string ->
  ?ref_:string ->
  ?toolchain:string ->
  ?base_handles:string list ->
  ?depends:(string * string option) list ->
  ?root_packages:string list list ->
  ?default:bool ->
  unit ->
  [ `Bumped of entry | `Unchanged of entry ]
(** [bump] flips the [x-oi-default-toolchain] flag on the bumped entry. Only
    valid for entries that already carry [x-oi-toolchain-name] (the function
    errors otherwise). Callers that want to switch the default from one
    toolchain to another are responsible for clearing the flag on the previous
    default before setting it on the new one — {!load} otherwise refuses to load
    a multi-default reporepo. *)

val remove :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  path:string ->
  handle:string ->
  ?version:string ->
  unit ->
  unit
(** [remove] Delete a reporepo entry. With [?version] only the named version is
    removed; without it every version of [handle] (and the materialised
    [v2/<handle>/]) is purged. *)

val ls_remote_sha : sys:D10.Sysops.t -> ?ref_:string -> string -> string
(** [ls_remote_sha ~sys ?ref_ url] runs [git ls-remote] against [url] and
    returns the commit sha at [ref_] (defaulting to [HEAD]). Used by
    [oi repo bump] to resolve a branch / tag to a pinned commit before
    materialising. *)

val is_sha_string : string -> bool
(** [is_sha_string s] is [true] iff [s] is a 40-character hex string (a git
    commit sha). Used by [oi repo lint] to validate pinned commits. *)
