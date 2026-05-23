(** [oi dist duniverse TARGET -o DIR] — vendor a target into a self-contained
    bundle that builds with no [oi] or [opam] at run time.

    Layout produced under [DIR]:

    {v
    DIR/
      Makefile                              # builds the full plan into
                                            # src/prefix/ (same Makefile
                                            # `oi dist makefile` would emit)
      build.sh                              # exec wrapper that exports
                                            # PATH/OCAMLPATH/OCAMLFIND_CONF
                                            # for src/prefix/ and runs dune
      dune-project                          # (lang dune 3.20)
      dune                                  # (vendored_dirs vendor)
      project/<pkg>/  →  ../sources/<sha>/  # the user's requested roots
                                            # (strict dune treatment)
      vendor/<pkg>/   →  ../sources/<sha>/  # dune-buildable dependencies
                                            # (vendored_dirs, relaxed)
      sources/<sha>/                        # canonical unpacked source trees
                                            # (Makefile builds from here)
      recipes/<hash>.{sh,env,prefix,…}      # per-node opam recipes
      oi-build-node.sh, oi-install.sh       # Makefile build helpers
    v}

    Source archives are fetched from the d10ir content-addressed store (local
    cache then [--registry]); no opam URL fetcher is involved. The typical
    consumer flow is:

    {v
      $ cd DIR
      $ make           # builds compiler + non-dune deps into src/prefix/
      $ ./build.sh     # dune build over vendor/, using src/prefix/ on PATH
    v} *)

val cmd : unit Cmdliner.Cmd.t
