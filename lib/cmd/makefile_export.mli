(** [oi source --makefile] backend.

    Render a unified d10ir plan ({!D10ir.Plan.t}, the same artifact [oi build]
    executes) as a portable Makefile that:

    - fetches each pre-baked source archive from
      [<registry>/d10ir-archives/<sha256>.tar.zst] into [src/<sha256>.tar.zst]
      (the [fetch] target, one stamp per archive), and
    - reproduces {!D10ir.Direct}'s per-node build (stage deps → unpack → subst →
      run script → apply .install → diff → store) into [src/layers/<hash>/].

    The compiler/toolchain layers ([plan.external_layers]) are not emitted as
    build rules — they are assumed reachable on the host [PATH], exactly as the
    d10ir executor treats a non-relocatable toolchain. *)

type local = {
  name : string;
  sha256 : string;
      (** Basename of the pre-shipped local source tarball, i.e.
          [<output>/src/<sha256>.tar.zst] — the caller writes it; there is no
          [fetch] rule for it. *)
  strip : int;
  script : string;
  env : string list;
  prefix : string;  (** Sentinel; rebased to the build prefix at make time. *)
  deps : string list;  (** Dep layer hashes to stage before the local build. *)
}
(** A non-registry "root" built from a local source snapshot rather than a
    pre-baked archive (the no-[TARGET] release-tarball flow). *)

val emit :
  D10ir.Plan.t ->
  output:string ->
  registry:string ->
  ?binaries:string list ->
  ?bin_roots:string list ->
  ?local:local ->
  unit ->
  unit
(** [emit plan ~output ~registry ?binaries ?bin_roots ()] writes, under
    [output/]: [Makefile], [oi-build-node.sh], [oi-install.sh],
    [recipes/<hash>.{sh,env,substs,substvars,prefix,name}], and [plan.json] (the
    full merged {!D10ir.Plan.t}, for debugging — [make] never consults it;
    everything [make] needs is the Makefile + recipes).

    [binaries] (the executables [oi build] reported producing, used only for the
    header comment and the [dest] summary) defaults to [[]].

    [bin_roots] are the layer hashes of the user-requested package(s) —
    [make dest] copies only those layers' [bin/]+[sbin/] into [dest/], i.e. just
    the selected package's own binaries, not the dependency closure. Defaults to
    the plan's (non-toolchain) roots if empty.

    The build/scratch tree is [src/] (deliberately not [dist/], to not collide
    with [dest/]). Per-node build output (both stdout and stderr — dune is
    stderr-only, so capturing both is what keeps the terminal quiet) goes to
    [src/log/<hash>.log]; only a one-line [BUILD <pkg>] progress line reaches
    the terminal, and on a node failure that node's log tail is pushed back to
    the terminal. [make V=1] streams full output live instead. [make dest]
    copies only the selected package(s)' [bin/]+[sbin/] into [dest/].

    Raises [Failure] (before writing anything) if any buildable node has no
    [archive.sha256] — otherwise the generated [fetch] target would 404. *)
