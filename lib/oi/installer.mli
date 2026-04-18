[@@@ai_disclosure "ai-assisted"]

(** Process an opam [.install] file without shelling out to [opam-installer].

    Parses the file via {!OpamFile.Dot_install}, resolves each source relative
    to [~build_dir] and copies it into the matching subdirectory of [~prefix],
    matching the destination and permission rules that the [opam-installer] CLI
    applies with default flags. *)

val install :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  prefix:string ->
  build_dir:string ->
  install_file:string ->
  unit
(** [install ~fs ~prefix ~build_dir ~install_file] copies every entry listed in
    [install_file] into [prefix].

    The package name is taken from the basename of [install_file] (e.g.
    {e foo.install} → {e foo}); it determines the package-scoped subdirectories
    such as [prefix/lib/foo].

    Source paths are joined with [build_dir]. An entry flagged as [{optional}]
    in the [.install] file is silently skipped if the source is missing; a
    missing mandatory source raises {!Error.build_failed}.

    The [misc:] section names absolute destinations. Entries under [prefix] are
    installed normally; entries outside [prefix] are logged and skipped, since
    anything outside [prefix] cannot be captured by the layer diff.

    Raises {!Error.build_failed} on a missing mandatory source or an I/O error
    while copying. *)
