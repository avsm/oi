(** What to consult the remote registry for, on a per-fetch basis. The two
    fetches the pipeline performs (binary layers, source archives) are gated
    independently so a CI registry-build can reuse cached source archives while
    regenerating every binary from scratch. *)

type t =
  | All  (** Binary layers AND source archives. *)
  | Archives  (** Source archives only; build every layer from source. *)
  | Never  (** Skip the registry; upstream source fetches still happen. *)

val of_string : string -> (t, string) result
(** [of_string s] parses [s] as one of ["all"], ["archives"], or ["never"].
    Case-insensitive. *)

val to_string : t -> string
(** [to_string t] is the lowercase tag for [t]: ["all"], ["archives"], or
    ["never"]. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] prints the same tag as {!to_string}. *)
