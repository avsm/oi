(** Custom opam extension field names used by oi.

    Every [x-*] field oi reads from or writes to an opam file is defined here,
    so a future rename or audit only touches one module. The values are plain
    strings — pass them to {!OpamFile.OPAM.extensions} lookups or write them out
    via the renderers in {!Source}.

    Binding names drop the [x-]/[x-oi-] prefix so [Keys.overlay],
    [Keys.toolchain_name], [Keys.repos], etc. read naturally at call sites. *)

(** {1 Reporepo overlay markers} *)

val overlay : string
(** [x-oi-overlay: true] — marks a reporepo entry as an oi overlay. Entries
    without this flag are skipped by {!Source.Reporepo} loaders. *)

val ref : string
(** [x-oi-ref: <git-ref>] — the git ref the overlay was bumped from. *)

val toolchain : string
(** [toolchain] — use-site tag declaring that this overlay targets the named
    toolchain (e.g. ["oxcaml"]). *)

(** {1 Toolchain definitions}

    An overlay entry is a {e toolchain definition} when it carries
    {!toolchain_name}. The other fields below are only meaningful on toolchain
    definitions and are validated together by [oi repo lint]. *)

val toolchain_name : string
(** [toolchain_name] — when set, this entry DEFINES a toolchain with the given
    CLI name (e.g. ["ocaml-5.4"], ["oxcaml"]). The handle namespace and CLI-name
    namespace are separate; this field is the bridge. *)

val toolchain_compiler : string
(** [toolchain_compiler] — the primary compiler package spec (e.g.
    ["ocaml-base-compiler.5.4.1"], ["ocaml-variants.\ 5.4.0+relocatable"]).
    Required on toolchain definitions. *)

val toolchain_roots : string
(** [toolchain_roots] [x-oi-toolchain-roots: [...]] — solver root specs for the
    toolchain. Either a flat list of strings or a list of lists of strings (each
    inner group acts as an OR). *)

val toolchain_tools : string
(** [toolchain_tools] [x-oi-toolchain-tools: [...]] — package names that
    [oi build] should always install into [_oi/tools/] alongside the toolchain
    (e.g. [odoc], [merlin], [ocaml-lsp-server]). *)

val relocatable : string
(** [x-oi-relocatable: <bool>] — build mode for the toolchain this entry
    defines. *)

val default_toolchain : string
(** [default_toolchain] — when set, this toolchain is selected when no
    [--toolchain] is given. Exactly one toolchain definition in the reporepo may
    carry this flag (validated by [oi repo lint]). *)

(** {1 Project / overlay payload} *)

val root_packages : string
(** [root_packages] [x-root-packages: [...]] — solver root specs
    [oi build --all] should solve for under this overlay. Same shape as
    {!toolchain_roots}: a flat list of strings or a list of lists. *)

val repos : string
(** [repos] [x-repos: [...]] — project [*.opam] field listing extra reporepo
    handles ([@HANDLE]) or repository URLs the project depends on. *)

val reporepo_hash : string
(** [x-reporepo-hash: <sha>] — git revision of the default reporepo at the time
    a bundle ([oi dist duniverse]) was produced. Stamps the metadata snapshot
    the solve used so a downstream [oi build] can reproduce the same opam-file
    resolution by rolling the local reporepo back to that sha. Information-only
    for now; consumed by [oi build] in a future revision. *)

(** {1 Bake markers} *)

val d10_archive : string
(** [d10_archive] — sha256 of the consolidated d10ir source archive produced by
    [oi repo bake] / [oi repo bump]. Set in-place on every reporepo opam after a
    successful bake; consumers ([oi build] / [oi show]) skip the
    fetch+patch+extras+substs pipeline whenever this is present and look the
    archive up in [<cache>/d10ir/archives/<sha>.tar.zst]. Hard-error if the
    registered archive isn't available locally — the user must re-bake. *)

(** {1 Helpers} *)

val read_string_ext : string -> OpamFile.OPAM.t -> string option
(** [read_string_ext key opam] returns the bare string value of the [key]
    extension on [opam], or [None] when the field is absent or holds a
    non-string shape (list, bool, filtered list). Used to read fields like
    {!d10_archive} where only a single sha is meaningful today. *)
