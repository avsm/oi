(** User-facing errors and the exit-code contract.

    Every CLI failure goes through {!t}. {!code} maps each error to a stable
    exit status, aligned with [opam] where the meaning matches:

    {table
      {tr {th [oi] } {th opam } {th meaning } }
      {tr {td [0] } {td [0] } {td success } }
      {tr {td [1] } {td [1] } {td generic / unspecified failure ({!Msg}) } }
      {tr {td [5] } {td [5] } {td system error (out of disk, permission, …) } }
      {tr {td [10] } {td [10] } {td configuration error ({!Config_error}) } }
      {tr {td [20] } {td [20] } {td no solver solution ({!No_solution}) } }
      {tr {td [30] } {td [30] } {td fetch / sync failure ({!Fetch_failed}) } }
      {tr {td [31] } {td [31] } {td package build failed ({!Build_failed}) } }
      {tr {td [50] } {td [50] } {td missing depext (system package absent) } }
      {tr
        {td [65] }
        {td [65] }
        {td not-found ({!Not_found}: package, binary, layer) }
      }
      {tr {td [99] } {td [99] } {td internal error } }
      {tr {td [124] } {td [-] } {td cmdliner argument-parse error (built-in) } }
      {tr {td [130] } {td [-] } {td interrupted (SIGINT/SIGTERM) } }
    } *)

(** Stable categorisation of every error. The constructor is what callers raise
    via the helpers below; {!kind} projects to a pure tag for routing (exit
    code, JSON serialisation, log filtering). *)
type t =
  | Not_found of { target : string; msg : string }
  | No_solution of { msg : string }
  | Config_error of { msg : string }
  | Build_failed of { pkg : string; cmd : string; output : string }
  | Fetch_failed of { url : string; msg : string }
  | Msg of string

type kind =
  | K_generic
  | K_system
  | K_config
  | K_no_solution
  | K_fetch
  | K_build
  | K_depext
  | K_not_found
  | K_internal

exception E of t
(** The single exception used for all user-facing errors. *)

val raise : t -> 'a
(** [raise e] raises [E e]. *)

val pp : t Fmt.t
(** [pp ppf e] pretty-prints [e] with ANSI colours. Intended for the human
    terminal path in {!Cmd.Harness}; the JSON path uses {!codec} instead. *)

val kind : t -> kind
(** [kind e] projects [e] to its stable categorisation tag. *)

val kind_string : kind -> string
(** [kind_string k] is the snake-case identifier (["not_found"],
    ["no_solution"], …) used as the [kind] field in the JSON envelope. *)

val code : t -> int
(** [code e] is the process exit status for [e] per the table above. *)

val codec : t Jsont.t
(** JSON envelope of an error:
    {[
      {
        "schema_version": "<version>",
        "error": {
          "code": <int>,
          "kind": "<kind_string>",
          "message": "<one-line summary>",
          "details": { ... }   // optional, shape per-kind
        }
      }
    ]}
    [details] is omitted unless the kind carries structured fields
    ({!Build_failed} carries [pkg]/[command]/[log]; {!Fetch_failed} carries
    [url]/[log]). *)

val to_json : t -> string
(** [to_json e] encodes [e] via {!codec} and returns a pretty-printed JSON
    string with a trailing newline. Falls back to a minimal ASCII envelope if
    the codec fails (which it shouldn't). *)

(** {1 Constructors} *)

val fail_not_found : string -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [fail_not_found target fmt …] raises a {!Not_found} carrying [target] and a
    [fmt]-formatted message. Exit code [65]. *)

val fail_msg : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [fail_msg fmt …] raises a generic {!Msg} with the [fmt]-formatted text. Exit
    code [1]. *)

val no_solution : string -> 'a
(** [no_solution msg] raises a {!No_solution} carrying [msg]. Exit code [20]. *)

val fail_config_error : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [fail_config_error fmt …] raises a {!Config_error}. Exit code [10]. *)

val build_failed : pkg:string -> cmd:string -> output:string -> 'a
(** [build_failed ~pkg ~cmd ~output] raises a {!Build_failed} naming the
    package, the failed command, and the captured output. Exit code [31]. *)

val fail_fetch_failed :
  url:string -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [fail_fetch_failed ~url fmt …] raises a {!Fetch_failed} for [url] with a
    [fmt]-formatted reason. Exit code [30]. *)
