(** [oi dist duniverse TARGET -o DIR] — fetch every source artefact needed to
    build [TARGET] into [DIR], so a later [dune build] / [opam install] /
    [oi build] / [oi sync] can run offline.

    Layout produced under [DIR]:

    {v
    DIR/
      dune-project                                # (lang dune 3.20)
      root.opam                                   # external deps
      <pkg>/                                      # extracted main source
      reporepo/v2/<handle>/packages/<pkg>/<v>/    # subset of opam files
    v} *)

val cmd : unit Cmdliner.Cmd.t
