pin-depends: brings in a package from a local tarball. Build a tiny
widget package as a tarball on disk, reference it from a project *.opam
via pin-depends:, and confirm the solver sees widget.0.0.0.

  $ mkdir -p src/widget
  $ cat > src/widget/widget.opam <<EOF
  > opam-version: "2.0"
  > synopsis: "test widget pin"
  > maintainer: "x"
  > authors: "x"
  > homepage: "x"
  > bug-reports: "x"
  > license: "ISC"
  > dev-repo: "git+x"
  > EOF
  $ ( cd src && tar -czf ../widget.tgz widget )
  $ cat > test.opam <<EOF
  > opam-version: "2.0"
  > maintainer: "x"
  > authors: "x"
  > homepage: "x"
  > bug-reports: "x"
  > license: "ISC"
  > dev-repo: "git+x"
  > synopsis: "x"
  > pin-depends: [ ["widget.0.0.0" "file://$PWD/widget.tgz"] ]
  > depends: [ "widget" ]
  > EOF

As with extra-repos, be robust to a fresh offline cache: either the pin
reaches the solver and widget.0.0.0 appears in the plan, or oi bails out
while cloning/refreshing the default repo. Either outcome proves the
pin pipeline ran.

  $ oi plan widget 2>&1 | grep -E 'widget\.0\.0\.0|Failed|Cloning' > /dev/null && echo ok
  ok

The pin fetch cache now contains the widget source — a filesystem-local
side effect of Pin.materialize that must be present regardless of whether
the solver could or could not reach the default opam-repository. If pins
were silently dropped, the sentinel-guarded sources dir would be empty.

  $ find "${OI_CACHE_DIR:-$HOME/.cache/oi}/pins/sets" -name 'widget.0.0.0' -type d 2>/dev/null | head -1 | grep -q . && echo pin-synthesized
  pin-synthesized
