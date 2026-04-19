[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** URL-supplied projects.

    CLI callers pass [--with=URL] to pull a whole upstream opam project
    into the current solve. oi clones [URL] into the pin cache (sharing
    the sentinel-based freshness machinery with {!Pin}), reads every
    [*.opam] at the clone's root via {!Project.load}, and synthesizes
    one {!Project.pin} per local package so the existing pin-depends
    pipeline can realise them through {!Pin.materialize}.

    Only [http(s)://], [git+…], [git@…], and [ssh://] URLs are treated
    as URL-projects. Everything else is left to callers to parse as an
    opam package spec via {!Script.parse_cli_dep}. *)

type t = {
  pins : Project.pin list;
      (** One synthetic pin per local package, plus every
          [pin-depends:] entry the URL project itself declared. *)
  roots : string list;
      (** Package names that should enter the solve as roots: the local
          packages provided by each URL project. Names are strings so
          callers can feed them straight into the existing [with_deps]
          plumbing. *)
  extra_repos : Project.extra_repo list;
      (** Any [x-opam-repositories:] the URL project declared. *)
  overlays : string list;
      (** Any [x-reporepo:] handles the URL project declared. *)
}

(** A single [--with=…] CLI token, classified. URLs are always tested
    first; everything else falls through to {!Script.parse_cli_dep}. *)
type with_arg = Url of string | Dep of Script.dep

val classify : string -> with_arg
(** Classify a [--with=…] token. Strings whose scheme is
    [http(s)://], [git+…], [git@…], [git://], or [ssh://] become
    {!Url}. Everything else is parsed as an opam package spec via
    {!Script.parse_cli_dep} and returned as {!Dep}. *)

val materialize :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  sys:D10.Sysops.t ->
  cache:Cache.t ->
  ?refresh:bool ->
  string list ->
  t
(** [materialize ~fs ~sys ~cache urls] clones every URL, loads each
    clone's [*.opam] files, and returns the aggregated pin +
    solver-root contribution. Empty [urls] yields the empty record.

    [refresh] flows through to the pin fetch logic and forces a
    re-clone regardless of sentinel age. *)

val classify_all : string list -> string list * Script.dep list
(** [classify_all tokens] runs {!classify} over every token and
    partitions the result into [(urls, deps)]. Convenience wrapper so
    callers get two lists in a single pass. *)
