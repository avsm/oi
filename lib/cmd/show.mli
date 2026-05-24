(** [oi show]: render a solver-resolved view of the current project — root set,
    the dependency tree below each root, layer hashes, install paths, and other
    diagnostic detail. *)

val cmd : unit Cmdliner.Cmd.t

val suggest_for : pkgs_dirs:string list -> string -> string list
(** [suggest_for ~pkgs_dirs target] returns plausible package-name suggestions
    for a [target] the user typed that doesn't match any known package.
    Substring-matches in either direction across every [packages_dirs] entry,
    dedup'd and sorted. Returns [[]] for very short ([< 4] char) targets to
    avoid noise. Exported so other commands ({!Cmd.Run}'s "no package or binary"
    error) can show the same did-you-mean list. *)
