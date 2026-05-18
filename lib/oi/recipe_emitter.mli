(** Convert an oi {!Plan.t} into a {!D10ir.Plan.t}.

    For each {!Plan.package_plan} marked [Source] (i.e. needs to be built), the
    emitter:
    - Calls {!Archive_builder.build} to produce a tarball with the pre-resolved
      source tree.
    - Composes a shell script from [build_commands ++ install_commands],
      shell-escaping each argv and joining with newlines under [set -e].
    - Emits a {!D10ir.Plan.node} carrying the layer hash, dep layer hashes,
      archive ref, script, env, prefix, and overlay metadata.

    [Binary] package_plans are skipped — they're already in the d10 store;
    downstream nodes reference them via [dep_layer_hashes]. *)

val emit :
  d10:D10.Config.t ->
  ?cli_invocation:string list ->
  ?extra_owned_paths:string list ->
  toolchain_name:string ->
  toolchain_layer:string ->
  Plan.t ->
  D10ir.Plan.t
(** [emit] Uses [d10.root] to resolve the recipe's [archive_root] (i.e.
    [<d10.root>/d10ir/archives/]). The archives themselves are NOT materialised
    here — that's [oi repo bake] / [oi repo bump]'s job; the emitter reads each
    package's [x-d10-archive] sha and references the archive by content hash.

    [extra_owned_paths] are additional path prefixes treated as oi-owned when
    scrubbing each node's [PATH] (on top of [cache_root] and the toolchains
    root). Pass an external non-relocatable toolchain's install prefix here so
    its [bin/] survives the scrub; otherwise consumer [+ox] builds fall back to
    the host system compiler. *)
