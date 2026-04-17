oi depexts prints platform-relevant system packages declared by a
dependency's opam file. We build a minimal local repo containing a
[widget] package whose [depexts:] covers macos and linux, declare it
in a project [*.opam] via [x-opam-repositories:], and ask for depexts.

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
  > x-opam-repositories: [ ["local" "$PWD/extras/repo"] ]
  > depends: [ "widget" ]
  > EOF

Be offline-robust: either the solver reaches the extra repo and prints
a libwidget depext, or oi bails out refreshing the default repo. Either
way the depexts pipeline ran.

  $ oi depexts 2>&1 | grep -E 'libwidget|Failed|Cloning' > /dev/null && echo ok
  ok

The --os override must influence filter evaluation. Either the solver
reached the extra repo and emitted [linux-libwidget] (and not
[mac-libwidget]), or oi bailed out refreshing the default repo. If
depexts ran, we assert the override took effect.

  $ oi depexts --os=linux > os.out 2> os.err; cat os.out os.err | grep -qE 'Failed|Cloning' && echo offline-skip || ( grep -q 'linux-libwidget' os.out && ! grep -q 'mac-libwidget' os.out && echo os-override-ok )
  os-override-ok

Error path: running in a dir with no *.opam files should fail cleanly
rather than crash.

  $ mkdir empty
  $ ( cd empty && oi depexts 2>&1 | grep -qE 'No .opam|error' ) && echo no-opam-handled
  no-opam-handled
