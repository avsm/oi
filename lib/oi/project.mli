(** Project metadata: read [*.opam] files in a directory, parse script-style
    dependency annotations, edit [dune-project] files, probe for dev tools, and
    clone remote URL-projects into the pin cache.

    The top-level module reads a project directory's [*.opam] files and surfaces
    the deps, pin-depends, and [x-repos:] declarations a CLI driver needs to
    drive the solver. The submodules cover the adjacent project concerns
    ({!Url}, {!Dune}, {!Script}, {!Tool}). *)

(** {1 Project metadata from a directory} *)

type extra_repo = {
  name : string;
  url : string;
  local_packages_dir : string option;
      (** When set, the entry resolves to this on-disk [packages/] directory and
          {!Source.Repo.ensure_many} returns it without cloning. Used for
          reporepo-handle overlays already materialised under
          [<reporepo>/v2/<handle>/]. *)
}

type pin = { pkg : OpamPackage.t; url : OpamUrl.t; declared_in : string }
(** [declared_in] is the source [*.opam] filename, used in error messages. *)

type t = {
  deps : string list;
      (** Direct deps, sorted, deduplicated. Excludes local packages and
          "ocaml". *)
  local_packages : string list;
      (** Names of [*.opam] files in the dir (without the [.opam] suffix). *)
  extra_repos : extra_repo list;
      (** URL-form entries from [x-repos:] across all [*.opam]. *)
  pins : pin list;
      (** Union of [pin-depends:] entries across all [*.opam], in declared
          order. *)
  overlays : string list;
      (** Reporepo-handle entries from [x-repos:] (the [@HANDLE] form). *)
  packages_dir : string option;
      (** When the project directory contains a [repo] file (opam-repository
          marker), this is [Some "<dir>/packages"]. The caller should inject it
          as a high-priority [packages_dir] in the solver. *)
}

val load : fs:Eio.Fs.dir_ty Eio.Path.t -> string -> t
(** [load ~fs dir] reads every [*.opam] in [dir], aggregates their direct deps,
    pin-depends, and [x-repos:] declarations, and returns the merged project
    view. Returns {!empty} when [dir] has no opam files. *)

val empty : t
(** Empty project metadata: no deps, no pins, no overlays. Used by callers that
    need to bypass the cwd probe (e.g. when [--skip-local] is set). *)

val pp : t Fmt.t
(** [pp ppf t] renders the deps / local-packages / overlays / pins counts. *)

(** {1 Script dependency parser}

    OCaml scripts declare deps via [[\@\@\@opam ...]] on the first line.
    Dependency strings accept an opam package name plus an optional version
    constraint ([pkg>=1.0]) and an optional findlib sub-library ([pkg.sub] or
    [pkg.sub>=1.0]). *)

module Script : sig
  type dep = {
    name : OpamPackage.Name.t;
    findlib_name : string;
    constraint_ : OpamFormula.version_constraint option;
  }

  val parse_deps_from_file : fs:Eio.Fs.dir_ty Eio.Path.t -> string -> dep list
  (** Parse the [[\@\@\@opam …]] annotation on the first line of [path]. Returns
      [[]] when the annotation is absent. *)

  val parse_cli_dep : string -> dep
  (** CLI-style dep spec: [.] after the package name is opam's [pkg.version]
      shorthand. *)

  val name_s : dep -> string
  (** [dep.name] as a plain string. *)

  val dedup : dep list -> dep list
  (** Drop later occurrences of the same dep name, preserving input order. *)

  val script_hash : string -> dep list -> string
  (** Content-addressed key for the (script path, dep list) pair. Identifies the
      cached compiled binary the script runner produces. *)

  val constraints :
    dep list -> OpamFormula.version_constraint OpamTypes.name_map
  (** Project a [dep list] down to the constraint map the solver consumes. *)

  val generate_project : script:string -> deps:dep list -> dir:string -> unit
  (** Write a synthetic [dune-project] + [main.ml] into [dir] that links the
      script's deps and compiles its body. Used by [oi run] for [.ml] targets.
  *)

  val scaffold_for_script :
    script:string -> deps:dep list -> dir:string -> string list * string list
  (** [scaffold_for_script ~script ~deps ~dir] writes [dune-project], [dune]
      (executable named after [script]'s basename, so dune compiles the script
      in place), and [<base>.opam] (deps = the parsed script deps, so
      [oi build]'s project mode has something to solve) into [dir] so an editor
      / LSP and [oi build] both work. Unlike {!generate_project} it does {e not}
      copy the script to [main.ml]. Each file is written only when absent —
      existing files are {e never} overwritten. Returns [(created, kept)]: the
      basenames written vs. the ones left untouched. Used by
      [oi build <script>.ml]. *)
end

(** {1 URL-supplied projects}

    CLI callers pass [--with=URL] to pull a whole upstream opam project into the
    current solve. oi clones [URL] into the pin cache (sharing the
    sentinel-based freshness machinery with {!Source.Pin}), reads every [*.opam]
    at the clone's root, and synthesises one {!pin} per local package so the
    existing pin-depends pipeline can realise them through
    {!Source.Pin.materialize}. *)

module Url : sig
  type nonrec t = {
    pins : pin list;
        (** One synthetic pin per local package, plus every [pin-depends:] entry
            the URL project itself declared. *)
    roots : string list;
        (** Package names that should enter the solve as roots: the local
            packages provided by each URL project. *)
    extra_repos : extra_repo list;
        (** URL entries from the URL project's [x-repos:] field. *)
    overlays : string list;
        (** Reporepo handles from the URL project's [x-repos:] field. *)
    packages_dir : string option;
        (** When the URL project contains a [repo] file, its [packages/]
            directory. *)
  }

  type with_arg = Url of string | Dep of Script.dep

  val classify : string -> with_arg
  (** Strings whose scheme is [http(s)://], [git+…], [git@…], [git://], or
      [ssh://] become {!Url}. Everything else is parsed as an opam package spec
      via {!Script.parse_cli_dep} and returned as {!Dep}. *)

  val materialize :
    ?reporter:Build_progress.reporter ->
    fs:Eio.Fs.dir_ty Eio.Path.t ->
    sys:D10.Sysops.t ->
    cache:Cache.t ->
    ?refresh:bool ->
    string list ->
    t
  (** [?reporter] receives [Status "Cloning <url>"] events as each [--with] URL
      is fetched. *)

  val classify_all : string list -> string list * Script.dep list
  (** [classify_all tokens] partitions each token via {!classify}, returning
      [(urls, deps)] in input order so callers can fan URL fetches out in
      parallel and feed [deps] straight to the solver. *)
end

(** {1 dune-project reader/writer} *)

module Dune : sig
  type t
  (** Parsed [dune-project] stanza set. *)

  val load : fs:Eio.Fs.dir_ty Eio.Path.t -> cwd:string -> t
  (** Read [<cwd>/dune-project]; errors when the file is missing or unparseable.
  *)

  val generate_opam_files : t -> bool
  (** [true] when [dune-project] declares [(generate_opam_files true)] — i.e.
      package metadata is sourced from dune's [(package …)] stanzas rather than
      hand-edited [*.opam] files. [oi add]'s edit path only works on
      generate-opam-files projects. *)

  val package_names : t -> string list
  (** Names of every [(package …)] stanza declared in [dune-project]. *)

  val add_dependency :
    t ->
    ?package:string ->
    name:string ->
    constraint_:(string * string) option ->
    unit ->
    t
  (** Insert a [(depends …)] entry into [package] (or the single declared
      package when omitted) with optional version [constraint_]. Returns the
      updated value; nothing is written until {!save}. *)

  val save : fs:Eio.Fs.dir_ty Eio.Path.t -> t -> unit
  (** Render [t] back over [dune-project] in place, preserving comments and
      indentation as far as possible. *)
end

(** {1 Dev-tool probes}

    Tools are opam packages whose binaries are useful during development but
    whose [lib/<pkg>/] trees must not leak into the main project's OCaml search
    path. *)

module Tool : sig
  type trigger = Ocamlformat_file | Dune_project_using of string
  type spec = { name : string; binary : string; trigger : trigger }

  val specs : spec list
  (** Editor / dev tools whose installation is project-state-driven (e.g.
      ocamlformat when [.ocamlformat] is present, mdx when [dune-project] uses
      it). Always-on tools live in the active toolchain's [x-oi-toolchain-tools]
      field instead — the toolchain decides what its consumers get without a
      hardcoded list here. *)

  type result = {
    spec : spec;
    hit : bool;
    version : string option;
    detail : string;
  }

  val probe : fs:Eio.Fs.dir_ty Eio.Path.t -> string -> result list
  (** [probe ~fs cwd] runs every entry in {!specs} against [cwd] and returns one
      {!result} per spec. Includes negatives so callers can show "checked X, not
      found" in diagnostics. *)

  val hits : result list -> result list
  (** Filter to results whose [hit] is true — convenience over
      [List.filter (fun r -> r.hit)]. *)
end
