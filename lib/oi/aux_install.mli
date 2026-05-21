(** Eager source install of a non-relocatable toolchain.

    For a non-relocatable toolchain (oxcaml) every package in
    [x-oi-toolchain-roots] — compiler bundle, [ocamlfind+ox], [ocamlbuild+ox] —
    is from-source-built into a single deterministic
    [$XDG_CACHE_HOME/oi/toolchains/<handle>-<ver>-<short_hash>] prefix that [oi]
    owns. The toolchain prefix is user-writable, so [ocamlfind+ox]'s configure
    runs with [ocaml:preinstalled = false] (no [-no-topfind] gate) and actually
    installs [topfind] into the prefix; later consumer builds just
    PATH-/[OCAMLPATH]-layer the prefix and treat its packages as
    virtually-installed.

    Reuses {!Build_pipeline.solve} + {!Build_pipeline.build} for the solve and
    build phases, threading [install_to = Some install_prefix] through
    {!D10ir.Direct.run} so every package writes directly into the toolchain
    prefix at install time — no staging-then-restore, no post-build
    hardlink-assemble, no cross-host path rewriting (registry-share is now
    disabled for non-relocatable builds in {!Build_pipeline.build}). *)

[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

val ensure :
  env:Build_pipeline.env ->
  ?reporter:Build_progress.reporter ->
  ?source_remote:D10.Layer.remote option ->
  Toolchain.info ->
  unit
(** [ensure ~env info] is a no-op when [info.relocatable] (compiler builds into
    each consumer prefix) or when [info.install_prefix] already holds a
    [bin/ocamlc] plus the [.oi-toolchain-ready] marker. Otherwise it solves
    [info.root_names] and drives the solve + build through {!Build_pipeline}
    with [install_to = Some info.install_prefix], so every toolchain package
    installs directly into the prefix at build time. On success it verifies
    [bin/ocamlc] is present and drops the [.oi-toolchain-ready] sentinel.

    [?source_remote] forwards to the inner {!Build_pipeline.build} so the
    toolchain build can fetch pre-baked source archives from the same registry
    the consumer build uses. Cmd-level callers typically construct it via
    {!Terms.remotes_of ~url:registry ~mode:use_registry} and partial-apply it
    when handing this function to {!Build_pipeline.solve}'s [aux_installer]
    parameter.

    Crucially the inner build is invoked with [layer_remote = None]: for
    non-relocatable compiler packages [oi] never pulls prebuilt binary layers
    from the remote registry. The compiler's [bin/ocamlc] bakes
    [--prefix=<install_prefix>/lib/ocaml] into its configure-time constants, so
    a registry layer baked on another host would point at a non-existent path.
    Source archives are content-addressed by source identity and have no
    machine-specific paths, so they're safe to pull.

    Raises {!Error.E} when the solve / build fails. Recursive calls inside the
    sub-solve are safe: it passes [request.toolchain = Some _] with empty
    [packages] / [root_names], so {!Build_pipeline.solve}'s [aux_installer] hook
    short-circuits without re-entering this function. *)
