(** Outcome of a single package action.

    The rich [t] carries per-failure-mode payloads (which command, which url,
    which upstream dep, etc.) and is what Audit and Execute speak in. [kind] is
    the no-payload tag, used by Manifest summaries, the [oi cache show] caller
    histogram, and the registry browser's status pills. *)

type fetch_kind =
  | Http_status of int
  | Checksum_mismatch
  | Network_timeout
  | Git_failed
  | Other of string

type t =
  | Ok
      (** Source build completed and the layer was committed. Pairs with a
          [Provenance.t] under the same layer hash. *)
  | Cached
      (** The package's layer was already in the d10 cache, so [D10ir.Direct]
          skipped its script. *)
  | Restored
      (** A Binary package was restored from a layer that was already in the
          local cache. *)
  | Build_failed of { command : string; exit_code : int option }
  | Install_failed of { command : string; exit_code : int option }
  | Dep_failed of { upstream : Identity.dep }
  | Fetch_failed of { url : string; kind : fetch_kind }
  | Depext_missing of { missing : string list; not_found : string list }
  | Solve_failed of { reason : string }
  | Skipped of { reason : string }

(** No-payload tag enum. Stable string forms used in summaries, JSON histograms,
    and the registry browser. *)
type kind =
  | K_ok
  | K_cached
  | K_restored
  | K_build_failed
  | K_install_failed
  | K_dep_failed
  | K_fetch_failed
  | K_depext_missing
  | K_solve_failed
  | K_skipped

val kind_of : t -> kind
(** [kind_of t] projects an outcome to its tag (drops the payload). *)

val string_of_kind : kind -> string

val pp : t Fmt.t
(** [pp ppf t] renders [t] via its {!kind_of} tag (no payload). *)

(** {1 Histogram helpers} *)

val bump : kind -> (kind * int) list -> (kind * int) list
(** [bump k h] increments the count for [k] in histogram [h], preserving the
    relative order of earlier entries (a new tag is appended at the end). *)

val sort_histogram : (kind * int) list -> (kind * int) list
(** [sort_histogram h] stably sorts [h]: highest count first, then alphabetic by
    [string_of_kind]. *)

(** {1 Codecs} *)

val codec : t Jsont.t
(** (de)serialises an outcome as JSON. *)

val kind_codec : kind Jsont.t
(** (de)serialises a {!kind} tag as JSON. *)
