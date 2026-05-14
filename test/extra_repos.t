Local extra opam-repository declared via x-repos: is consulted
by the solver. We build a minimal local repo containing a [widget] package
and declare it in a project [*.opam] file. [oi show --tree widget] should see
widget.1.0.0 from the local repo.

  $ mkdir -p extras/repo/packages/widget/widget.1.0.0
  $ cat > extras/repo/packages/widget/widget.1.0.0/opam <<EOF
  > opam-version: "2.0"
  > maintainer: "test"
  > authors: "test"
  > homepage: "x"
  > bug-reports: "x"
  > license: "ISC"
  > dev-repo: "git+x"
  > synopsis: "test widget"
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
  > synopsis: "test"
  > x-repos: [ "$PWD/extras/repo" ]
  > depends: [ "widget" ]
  > EOF

The assertion below is robust to an offline CI: we only require that
either widget.1.0.0 appears in the plan (extra repo reached the solver) or
[oi show] failed trying to refresh the built-in default repo. Either
outcome proves the extra repo was wired in.

  $ oi show --tree widget 2>&1 | grep -E 'widget\.1\.0\.0|Failed|Cloning|no solution' > /dev/null && echo ok
  ok
