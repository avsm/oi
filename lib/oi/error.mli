(** User-facing errors. *)

(** Structured error type for clean CLI output. *)
type t =
  | Not_found of { target : string; msg : string }
      (** A package, binary, or workspace was not found. *)
  | No_solution of { msg : string }
      (** The solver could not find a valid dependency solution. *)
  | Config_error of { msg : string }
      (** Invalid or missing workspace configuration. *)
  | Build_failed of { pkg : string; cmd : string; output : string }
      (** A build or install command failed. *)
  | Fetch_failed of { url : string; msg : string }
      (** A source download failed. *)
  | Msg of string  (** A generic user-facing error message. *)

exception E of t
(** The single exception used for all user-facing errors. *)

val raise : t -> 'a
(** [raise e] raises {!E}[ e]. *)

val pp : t Fmt.t
(** [pp] pretty-prints the error with ANSI colours. *)

val not_found : string -> ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [not_found target fmt ...] raises a Not_found error. *)

val msg : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [msg fmt ...] raises a Msg error. *)

val no_solution : string -> 'a
(** [no_solution diagnostic] raises a No_solution error. *)

val config_error : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [config_error fmt ...] raises a Config_error. *)

val build_failed : pkg:string -> cmd:string -> output:string -> 'a
(** [build_failed ~pkg ~cmd ~output] raises a Build_failed error. *)
