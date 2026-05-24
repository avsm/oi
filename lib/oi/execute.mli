(** Source-prep helpers shared between {!Archive_builder.build} and
    {!Archive_builder.build_no_solve}.

    Builds no longer happen here — every build path in oi flows through
    {!Recipe_emitter.emit} → {!D10ir.Direct.run}. What's left in this module is
    the trio of preparation steps {!Archive_builder} runs in sequence to produce
    a consolidated source archive: fetch sources, copy opam [extra-files:] into
    the build dir, apply patches via [OpamFilename.patch]. *)

val fetch_phase :
  ?cache_urls:OpamUrl.t list ->
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  cache_root:string ->
  Plan.package_plan ->
  unit
(** [fetch_phase ?cache_urls ~fs ~cache_root p] fetches [p]'s primary source +
    extra sources into [p.build_dir]. *)

val copy_extra_files : Plan.package_plan -> unit
(** [copy_extra_files p] copies opam [extra-files:] entries into [p.build_dir].
*)

val apply_patches : Plan.package_plan -> unit
(** [apply_patches p] runs [OpamFilename.patch] on each declared patch from
    [p.build_dir]. Pure-OCaml; no shell-out to [patch(1)]. *)
