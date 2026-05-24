(** [oi ir]: emit, inspect, and execute d10ir build recipes.

    The [emit] subcommand resolves a target through the normal solver pipeline
    but stops short of building, writing a [recipe.json] suitable for
    reproducing the build later (or on a different host). The [run] subcommand
    consumes that recipe directly. Also exports an opam-file helper used by
    [oi repo bump] for stamping [x-d10-archive] onto baked overlays. *)

val cmd : unit Cmdliner.Cmd.t

val opam_set_x_d10_archive : path:string -> sha:string -> [ `Added | `Already ]
(** [opam_set_x_d10_archive ~path ~sha] sets or replaces the [x-d10-archive]
    extension in the opam file at [path] in place. Returns [`Already] when the
    value matches the existing one (no write happens), [`Added] otherwise. Used
    by [oi repo bump] — the only place that should populate this field. *)

val opam_strip_patches_extras :
  fs:Eio.Fs.dir_ty Eio.Path.t -> opam_path:string -> [ `Stripped | `Already ]
(** [opam_strip_patches_extras ~fs ~opam_path] strips [patches:] and
    [extra-files:] from the opam file at [opam_path] in place and removes the
    sibling [files/] subdirectory holding those blobs. Intended for the
    post-bake cleanup path once [x-d10-archive] has been stamped on the opam
    file: the consolidated d10ir archive already contains the patched +
    extras-included source tree, so the reporepo's copy is dead weight. The
    [files/] dir typically dominates a baked overlay's on-disk size. Returns
    [`Already] when there was nothing to remove, [`Stripped] otherwise. *)
