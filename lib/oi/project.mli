[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Read project metadata from the *.opam files in a directory. *)

type extra_repo = { name : string; url : string }

type pin = { pkg : OpamPackage.t; url : OpamUrl.t; declared_in : string }
(** [declared_in] is the source *.opam filename, used in error messages. *)

type t = {
  deps : string list;
      (** Direct deps, sorted, deduplicated. Excludes local packages and
          "ocaml". *)
  local_packages : string list;
      (** Names of *.opam files in the dir (without the [.opam] suffix). *)
  extra_repos : extra_repo list;
      (** Union of [x-opam-repositories:] entries across all *.opam, in declared
          order (first-occurrence wins on duplicate names). *)
  pins : pin list;
      (** Union of [pin-depends:] entries across all *.opam, in declared order.
      *)
  overlays : string list;
      (** Union of [x-reporepo:] entries across all *.opam. Each element is
          a reporepo handle (e.g. ["avsm"]); {!Oi.Reporepo} resolves them
          at sync time. Deduplicated, declared-order preserved. *)
}

val load : fs:Eio.Fs.dir_ty Eio.Path.t -> string -> t
(** [load ~fs dir] scans every [*.opam] file in [dir]. Raises
    {!Error.config_error} on malformed [x-opam-repositories:] values, on
    conflicting [extra_repos] (same name, different URL), or on conflicting
    [pins] (same package name, different URL or version). *)
