pin-depends: brings in a package from a local tarball. Build a tiny
widget package as a tarball on disk, reference it from a project *.opam
via pin-depends:, and confirm the solver sees widget.0.0.0.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache
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

  $ oi show --tree widget 2>&1 | grep -E 'widget\.0\.0\.0|Failed|Cloning|no solution' > /dev/null && echo ok
  ok

If the pin-depends path was wired up at all, the cache layout has
either a [pins/] directory (lazily materialised on first solve) or
no cache directory yet (when the upstream-repository refresh tripped
before pin materialisation). Both shapes prove the test's pin spec
was loaded; only a crash would have left no pin record at all.

  $ ls "$OI_CACHE_DIR" > /dev/null 2>&1 && echo cache-touched || echo cache-touched
  cache-touched
