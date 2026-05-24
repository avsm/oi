(** Fixed-prefix OCaml toolchains.

    A toolchain is a bundle of OCaml compiler + findlib + ocamlbuild (+ maybe
    dune) that lives at a persistent, well-known path and is shared across all
    consumer prefixes via PATH / OCAMLPATH layering rather than hardlinks.
    Needed for compilers whose [--prefix] is baked into the binary (oxcaml,
    upstream [ocaml-base-compiler]): copying such a compiler into a per-solve
    consumer prefix would break, because [Config.standard_library] points back
    at the original install path.

    Toolchains are reporepo-defined: each is a definition-only entry (no own
    [url:], just [depends:] composing existing overlays) carrying
    [x-oi-toolchain-name], [x-oi-toolchain-compiler], [x-oi-relocatable], and
    [x-oi-toolchain-roots]. The reporepo handle (e.g. [toolchain-oxcaml]) and
    the CLI toolchain name (e.g. [oxcaml] from [x-oi-toolchain-name]) live in
    separate namespaces; the latter is what [--toolchain=NAME] resolves against.

    On resolve the toolchain entry's [depends:] are walked transitively, the
    URL-bearing overlays in scope are materialised, and the
    [x-oi-toolchain-roots] are solved. A non-relocatable toolchain (oxcaml) is a
    preinstalled system package: oi probes for its fixed external prefix (from
    [x-oi-external-prefix] / [brew --prefix] / [OI_<HANDLE>_PREFIX]) and
    hard-errors with an install hint if absent — it is never built by oi.
    Relocatable toolchains have their compiler built into the consumer prefix by
    the normal solve. *)

[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

type info = {
  handle : string;  (** Toolchain handle, e.g. [oxcaml]. *)
  ocaml_version : string;
      (** Effective [ocaml-variants] / [ocaml-base-compiler] version (e.g.
          [5.2.0+ox] or [5.3.0]). Derived from the solved packages. *)
  install_prefix : string;
      (** Absolute path where a non-relocatable toolchain lives. Computed even
          for relocatable toolchains (folded into the cache key / identity), but
          the directory is never created on disk in that case. *)
  hash : string;  (** 64-hex content hash of the resolved opam files + conf. *)
  relocatable : bool;
      (** [true] when the toolchain's compiler can be installed into a per-solve
          consumer prefix — {!ensure_installed} is then a no-op and downstream
          env / [mark_installed] paths skip the fixed-prefix treatment. The
          toolchain still pins the compiler version via {!opam_ctx_of_info}.
          [false] (oxcaml) means [oi] from-source-builds {!root_names} as a unit
          into {!install_prefix} (always a user-writable [$XDG_CACHE_HOME]
          location), PATH-/[OCAMLPATH]-layers that prefix into every consumer
          build, and treats those packages as virtually-installed for solver
          purposes. *)
  preinstalled_override : bool option;
      (** When [Some b], the [ocaml:preinstalled] opam variable is forced to [b]
          inside {!Solver.Ctx.create}. [None] for normal consumer builds
          (preinstalled follows [relocatable]). {!Toolchain_install} sets
          [Some false] for the one-off toolchain build so [ocamlfind+ox]'s
          configure does not pass [-no-topfind] — the toolchain prefix is
          user-writable, so [topfind] actually gets installed there. *)
  packages : OpamPackage.Set.t;  (** Packages installed in the toolchain. *)
  compiler_name : OpamPackage.Name.t;
      (** The single compiler-package name this toolchain installs (parsed from
          [x-oi-toolchain-compiler], e.g. [ocaml-base-compiler] for upstream,
          [ocaml-variants] for relocatable / oxcaml). Replaces the hardcoded
          [["ocaml"; "ocaml-base-compiler"; "ocaml-variants"; ...]] families
          previously sprinkled across the codebase: every site that wants to ask
          "is this package the toolchain's compiler?" reads this field. *)
  root_names : OpamPackage.Name.Set.t;
      (** Names originally passed as root packages to the toolchain solver (e.g.
          [ocaml-variants], [ocamlfind]). Subset of [packages] by name. The
          consumer solver adds just these as required roots — adding the full
          [packages] set (transitive closure) pulls in oxcaml meta packages that
          trigger unsatisfiable conflicts. *)
  packages_dirs : string list;
      (** Packages directories used to resolve [packages]. Preserved for later
          solves that want to consult the toolchain's opam metadata. *)
  tools : string list;
      (** Package names from the toolchain's [x-oi-toolchain-tools] field.
          [oi build] installs these unconditionally into [_oi/tools/] when this
          toolchain is active. Empty when the toolchain ships no default tool
          set; per-project triggered tools (mdx when dune-project uses it,
          ocamlformat when .ocamlformat is present) live in code, not here. *)
  dep_handles : string list;
      (** Reporepo overlay handles the toolchain layers under (e.g.
          [["default"]]). Surfaced verbatim by [oi show]'s [Repositories] block.
      *)
}

val opam_ctx_of_info : info -> Solver.Ctx.toolchain
(** [opam_ctx_of_info i] projects [i] down to the {!Solver.Ctx.toolchain} subset
    that [Solver.Ctx.create] / [Solver.solve] / [Prefix.make_env] need. Single
    source of truth for the conversion, used by all the CLI commands that thread
    a toolchain through. *)

val apply_conf : info option -> Solver.Ctx.conf -> Solver.Ctx.conf
(** [apply_conf info conf] is [conf] with [ocaml_version] replaced by the
    toolchain's solved version when [info] is set, unchanged otherwise.
    Consumers that thread an [Solver.Ctx.conf] through to a solve / build need
    this so [ocaml:version] resolves to the toolchain's compiler. *)

val default_root : unit -> string
(** [default_root ()] is the root dir under which toolchains are installed, i.e.
    [$XDG_CACHE_HOME/oi/toolchains]. *)

val resolve :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  data_dir:string ->
  conf:Solver.Ctx.conf ->
  handle:string ->
  info
(** [resolve ~fs ~sys ~data_dir ~conf ~handle] looks up [handle] (the CLI
    toolchain name) in the reporepo: finds the latest entry whose
    [x-oi-toolchain-name] matches, resolves its [depends:] transitively,
    materialises the URL-bearing overlays, solves the [x-oi-toolchain-roots],
    computes the effective hash, and returns the {!info}. Auto-clones the
    reporepo if missing. Raises {!Error.fail_config_error} if no entry defines a
    toolchain with that name. *)

val url_of : handle:string -> string option
(** [url_of ~handle] returns the URL of the toolchain's primary source — by
    convention the URL of the first overlay listed in the toolchain entry's
    [depends:] (e.g. [oxcaml] for the oxcaml toolchain). [None] when no reporepo
    entry defines the named toolchain. Used by [oi show] to display the
    toolchain's source repo. *)

val depends_of : handle:string -> string list option
(** [depends_of ~handle] returns the reporepo overlay handles the named
    toolchain layers under (e.g. [Some ["oxcaml"; "default"]] for [oxcaml]).
    [None] when no reporepo entry defines the named toolchain. Used by
    [oi repo add]/[bump] when an overlay declares [x-oi-toolchain] so the
    auto-injected base pins match the toolchain's own base set. *)

type summary = {
  handle : string;
  url : string;
  ref_ : string option;  (** Git ref tracked, e.g. [Some "relocatable"]. *)
  relocatable : bool;
      (** [true] when the toolchain's compiler is built into the consumer
          prefix; [false] when [oi] source-builds it into a user-writable
          [$XDG_CACHE_HOME/oi/toolchains] prefix. *)
  is_default : bool;
      (** [true] when this toolchain's latest entry carries
          [x-oi-default-toolchain: true]. *)
  depends : string list;
  roots : string list;
  tools : string list;
      (** Always-on dev tools the toolchain installs into [_oi/tools/] on each
          [oi build] (from [x-oi-toolchain-tools]). *)
}

val available : unit -> summary list
(** [available ()] is a snapshot of every toolchain definition the reporepo
    currently advertises (entries carrying [x-oi-toolchain-name]). Cheap (no
    solve, no network), so safe for [oi config]. Empty when the reporepo has no
    toolchain entries — fresh machines need to clone or create them. *)

val ensure_installed : ?reporter:Build_progress.reporter -> info -> unit
(** [ensure_installed ?reporter i] ensures toolchain [i] is usable. No-op for
    relocatable toolchains (the compiler is built into the consumer prefix by
    the normal solve). For a non-relocatable toolchain it probes
    [<i.install_prefix>/bin/ocamlc]; the {e populating} of that prefix is
    {!Oi.Aux_install.ensure}'s job and runs ahead of the per-consumer build via
    the {!Build_pipeline.aux_installer} plumbing. [?reporter] receives [Status]
    events around the probe. *)

val is_ready : info -> bool
(** [is_ready info] is the general predicate for "this toolchain is usable".
    [true] for relocatable toolchains (compiler built into the consumer prefix,
    nothing to probe) and for non-relocatable ones whose
    [info.install_prefix/bin/ocamlc] exists. Callers that stage
    [info.install_prefix] into a build env should gate on this to fail fast with
    a clear error instead of letting downstream [+ox] builds crash trying to
    find a compiler that isn't there. *)
