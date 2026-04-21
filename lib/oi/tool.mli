[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Dev-tool probes for a project directory.

    Tools are opam packages whose binaries are useful during development but
    whose [lib/<pkg>/] trees must not leak into the main project's OCaml search
    path. Each tool is solved independently against the project's current ocaml
    constraint and installed into [_oi/tools/].

    The set of tools is probe-driven and hardcoded; there is no project config
    field to add extras. *)

type trigger =
  | Always  (** Installed on every project. *)
  | Ocamlformat_file  (** [.ocamlformat] exists (and declares [version =]). *)
  | Dune_project_using of string
      (** [dune-project] contains a [(using <name> ...)] stanza. *)

type spec = {
  name : string;  (** Opam package name. *)
  binary : string;  (** Expected [bin/<binary>] name (often same as name). *)
  trigger : trigger;
}
(** A single probe rule. *)

val registry : spec list
(** The hardcoded tool registry, in display order. *)

type result = {
  spec : spec;
  hit : bool;
      (** True when the trigger condition was met — tool should be installed. *)
  version : string option;
      (** Version string derived from the probe, if any. Only
          {!Ocamlformat_file} populates this (from the [.ocamlformat] file's
          [version = X.Y.Z] line). *)
  detail : string;
      (** Human-readable explanation of why [hit] is what it is, for display in
          [oi tool list] / [oi config]. *)
}
(** Outcome of running a probe in a project directory. *)

val probe : fs:Eio.Fs.dir_ty Eio.Path.t -> string -> result list
(** [probe ~fs dir] runs every probe in {!registry} against [dir] and returns
    one {!result} per registered tool. Always returns a result for every tool,
    so callers can render misses too. *)

val hits : result list -> result list
(** Keep only the results whose [hit] is true. Convenience for callers that only
    want the install set. *)
