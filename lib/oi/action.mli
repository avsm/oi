[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

(** Action graph for package installation.

    Captures the full dependency DAG with per-package build phases, preserving
    the parallel structure from the solver. When [~d10] is provided to {!plan},
    packages whose layers already exist in the cache are marked as {!Binary} and
    restored directly instead of building from source. *)

(** {1 Types} *)

(** How a package will be installed. *)
type method_ =
  | Source of { is_virtual : bool }
      (** Build from source. Virtual packages have no build/install commands. *)
  | Binary  (** Restore from the cached layer at [node.layer_hash]. *)

type node = {
  pkg : OpamPackage.t;
  opam : OpamFile.OPAM.t;
  method_ : method_;
  deps : OpamPackage.Name.t list;
      (** Direct dependencies within the plan (determines execution order). *)
  layer_hash : string;
      (** Content hash of the package plus its transitive in-plan deps. *)
}
(** A single package action in the plan. *)

type t
(** An action graph: a DAG of nodes keyed by package name, preserving parallel
    structure. *)

(** {1 Construction} *)

val plan :
  Opam_ctx.t ->
  ?d10:D10.Config.t ->
  packages_dirs:string list ->
  OpamPackage.t list ->
  t
(** [plan ctx ?d10 ~packages_dirs pkgs] builds an action graph from a
    topologically sorted package list. When [d10] is given, checks the layer
    cache and marks cached packages as [Binary]. *)

(** {1 Accessors} *)

val nodes : t -> node list
(** [nodes t] is the list of all nodes in topological order. *)

val roots : t -> node list
(** [roots t] is the list of nodes with no in-plan dependencies (can start
    immediately). *)

val dependents : t -> OpamPackage.Name.t -> node list
(** [dependents t name] is the list of nodes that directly depend on [name]. *)

val total : t -> int
(** [total t] is the number of nodes in the plan. *)

val find : t -> OpamPackage.Name.t -> node
val dep_pkgs_of : t -> node -> OpamPackage.t list
val nodes_by_name : t -> node OpamPackage.Name.Map.t
val topo_order : t -> OpamPackage.Name.t list
val parallel_groups : t -> OpamPackage.Name.t list list

(** {1 Layer hashes} *)

val layer_hash_for : t -> OpamPackage.Name.t -> string option
(** [layer_hash_for t name] returns the layer hash for a specific package, or
    [None] if the package is not in the plan. *)

val layer_hashes : t -> string list
(** [layer_hashes t] returns the layer hash for each package in the plan, in
    topological order. Used for prefix assembly. *)

(** {1 Dry-run output} *)

val pp_tree : ?remote_has:(string -> bool) -> t Fmt.t
(** [pp_tree ?remote_has] renders the action graph as a dependency tree.
    [Source] packages whose layer hash satisfies [remote_has] are shown as
    "remote" (cyan) instead of "source" (blue). *)
