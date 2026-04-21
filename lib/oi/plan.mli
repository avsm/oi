(** Execution plan: fully resolved build/install commands.

    Computes a complete execution plan from a dependency solution, resolving all
    opam variables, filters, and commands into concrete shell commands. The plan
    is organised into parallel stages — packages within a stage have no
    inter-dependencies and can build concurrently in separate prefixes.

    Build directories are deterministic under the oi cache root at
    [{cache_root}/build/_build/{pkg}/], with the shared prefix at
    [{cache_root}/build/prefix/]. Each package lists the dependency layers that
    must be hardlinked into the prefix before building. *)

type source_info = { url : string; checksums : string list }
type patch = { file : string; filter : string option }
type subst = string

type dep_layer = {
  pkg : string;  (** dependency package name.version *)
  hash : string;  (** d10 layer hash *)
}

type package_plan = {
  pkg : string;
  layer_hash : string;
  method_ : [ `Source | `Binary ];
  dep_layers : dep_layer list;
  source : source_info option;
  extra_sources : (string * source_info) list;
  patches : patch list;
  substs : subst list;
  subst_vars : (string * string) list;
  build_commands : string list list;
  install_commands : string list list;
  install_file : string;
  env : string array;
  build_dir : string;
  prefix : string;
  overlay_handle : string option;
      (** Reporepo overlay handle that contributed this package's opam file
          (e.g. ["default"], ["avsm"]), or [None] when the package came from a
          non-overlay source like a pin-depends tree. *)
  overlay_version : string option;
      (** Reporepo overlay version (e.g. ["20260418.6"]). Present iff
          [overlay_handle] is. *)
}

type group = { stage : int; packages : package_plan list }

type t = {
  groups : group list;
  cache_root : string;
  os_key : string;
  ocaml_version : string;
  total_packages : int;
}

val create :
  Opam_ctx.t ->
  packages_dirs:string list ->
  cache_root:string ->
  os_key:string ->
  ocaml_version:string ->
  Action.t ->
  t
(** [packages_dirs] is the same list passed to the solver, used to attribute
    each package to its source directory so the resulting plan (and any layer
    built from it) can be tagged with the overlay that contributed the opam
    file. *)

val pp : t Fmt.t
