[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

(** Script runner.

    Parses OCaml scripts with [[\@\@\@opam ...]] dependency declarations,
    generates a temporary dune project, installs dependencies, builds, and runs
    the script. Built binaries are cached by script hash under {!Cache.runs}.

    Dependency strings accept an opam package name plus an optional
    version constraint ([pkg>=1.0]) and an optional findlib sub-library
    ([pkg.sub] or [pkg.sub>=1.0]). The opam solver sees the stem before
    the first dot — e.g. [ppx_deriving.show>=1.0] resolves against the
    [ppx_deriving] opam package at [>= 1.0] — while the generated dune
    stanza uses the full findlib name. *)

type dep = {
  name : OpamPackage.Name.t;
      (** Opam package name — the stem before the first [.]. *)
  findlib_name : string;
      (** Findlib library reference, including any [.sub] suffix. Equal
          to [OpamPackage.Name.to_string name] when no sub-library was
          specified. *)
  constraint_ : OpamFormula.version_constraint option;
}
(** A dependency parsed from the opam attribute or --with flag. *)

val parse_deps_from_file : string -> dep list
(** [parse_deps_from_file path] reads the first line of [path] and parses
    [[\@\@\@opam pkg1>=1.0 pkg2 ...]] into a dependency list. Returns [[]] if no
    attribute is found. *)

val parse_dep : string -> dep
(** [parse_dep s] parses ["pkg>=1.0"] into a dep using
    {!OpamFormula.atom_of_string}. *)

val name_s : dep -> string
(** [name_s d] is the package name as a string. *)

val dedup : dep list -> dep list
(** [dedup deps] drops duplicates by package name, preserving first-occurrence
    order. Use this after merging script-declared deps with CLI [--with] flags
    so the generated dune [libraries] stanza (and the solver input) doesn't
    carry duplicates. *)

val script_hash : string -> dep list -> string
(** [script_hash path deps] returns a short hash of the script contents and its
    dependency list, for caching. *)

val constraints : dep list -> OpamFormula.version_constraint OpamTypes.name_map
(** [constraints deps] extracts version constraints from the dep list into a
    name map suitable for {!Solve.solve}. *)

val generate_project : script:string -> deps:dep list -> dir:string -> unit
(** [generate_project ~script ~deps ~dir] creates a dune project in [dir] with
    [script] as the main executable and [deps] as libraries. *)
