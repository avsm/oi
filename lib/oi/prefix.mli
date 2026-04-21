[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

(** OCaml-specific prefix environment.

    Environment variables for OCaml toolchain prefixes. Layer assembly is
    handled by {!D10.Prefix}. *)

val env_vars : prefix:string -> dune_cache_root:string -> (string * string) list

val envrc_content :
  prefix:string -> ?tools:string -> dune_cache_root:string -> unit -> string
(** Generate the contents of an [.envrc] that activates [prefix]. When [tools]
    is given, its [bin/] subdirectory is prepended to [PATH] ahead of
    [prefix/bin]. The tools [lib/] is intentionally NOT wired into
    OCAMLLIB/OCAMLPATH/OCAMLFIND_DESTDIR — dev tools stay visible as binaries on
    PATH but invisible to the main project's compiler. *)

val make_env :
  prefix:string ->
  ?tools:string ->
  dune_cache_root:string ->
  unit ->
  string array
(** Like {!envrc_content} but returns an environment array suitable for
    [Eio.Process.spawn]. *)
