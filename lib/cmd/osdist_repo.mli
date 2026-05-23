(** [oi dist repo]: assemble a signed apt/dnf/bin repo from the artefacts
    produced by [oi dist pkg --build]. Idempotent — re-runs merge new packages
    into the existing tree, preserving prior versions. *)

val cmd : unit Cmdliner.Cmd.t

(** {1 GPG signing-key auto-detection (exposed for tests)} *)

type signing_key = {
  fingerprint : string;
  algos : string list;
  curves : string list;
  uid : string;
}

val parse_signing_keys : string -> signing_key list
(** [parse_signing_keys output] decodes the colon-format dump produced by
    [gpg --list-secret-keys --with-colons --with-fingerprint] into one
    {!signing_key} per usable, sign-capable secret key (expired / revoked /
    invalid entries are dropped). Both the primary and any sign-capable subkeys
    contribute their algo + curve. *)

val pick_signing_key : signing_key list -> signing_key option
(** [pick_signing_key keys] returns the most preferable key for signing repo
    metadata: the first whose primary or subkeys is on the Ed25519 curve,
    otherwise the first key in [keys] (or [None] if empty). *)
