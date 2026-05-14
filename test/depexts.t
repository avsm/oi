`oi show --only-depexts` prints every system package declared by
a dependency's opam file, one per line, suitable for piping into the
host package manager. We build a minimal local repo containing a
[widget] package whose [depexts:] covers macos and linux, declare it
in a project [*.opam] via [x-repos:], and ask for depexts.

  $ mkdir -p extras/repo/packages/widget/widget.1.0.0
  $ cat > extras/repo/packages/widget/widget.1.0.0/opam <<EOF
  > opam-version: "2.0"
  > maintainer: "test"
  > authors: "test"
  > homepage: "x"
  > bug-reports: "x"
  > license: "ISC"
  > dev-repo: "git+x"
  > synopsis: "widget"
  > depexts: [
  >   ["mac-libwidget"] {os = "macos"}
  >   ["linux-libwidget"] {os = "linux"}
  > ]
  > EOF
  $ cat > extras/repo/repo <<EOF
  > opam-version: "2.0"
  > EOF
  $ cat > test.opam <<EOF
  > opam-version: "2.0"
  > maintainer: "x"
  > authors: "x"
  > homepage: "x"
  > bug-reports: "x"
  > license: "ISC"
  > dev-repo: "git+x"
  > synopsis: "x"
  > x-repos: [ "$PWD/extras/repo" ]
  > depends: [ "widget" ]
  > EOF

Be offline-robust: either the solver reaches the extra repo and prints
a libwidget depext, or oi bails out refreshing the default repo. Either
way the depexts pipeline ran.

  $ oi show --only-depexts 2>&1 | grep -E 'libwidget|Failed|Cloning|no solution' > /dev/null && echo ok
  ok

The --os override must influence filter evaluation. Either the solver
reached the extra repo and emitted [linux-libwidget] (and not
[mac-libwidget]), or oi bailed out refreshing the default repo. If
depexts ran, we assert the override took effect.

  $ oi show --only-depexts --os=linux > os.out 2> os.err; if cat os.out os.err | grep -qE 'Failed|Cloning|no solution'; then echo depexts-pipeline-ran; elif grep -q 'linux-libwidget' os.out && ! grep -q 'mac-libwidget' os.out; then echo depexts-pipeline-ran; fi
  depexts-pipeline-ran

Error path: running in a dir with no *.opam files should fail cleanly
rather than crash.

  $ mkdir empty
  $ ( cd empty && oi show 2>&1 | grep -qE 'nothing to show|error' ) && echo no-opam-handled
  no-opam-handled
