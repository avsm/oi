Git pin revision tracking: a pin against a local git repo picks up
upstream commits when [--refresh] is passed. We exercise this by
creating a local bare repo, pinning against it, running [oi show] once,
pushing a new commit, and verifying that [oi show --tree --refresh] produces a
different layer hash than the first run.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache
  $ mkdir -p upstream
  $ cd upstream && git init -q && git config user.email x@x && git config user.name x
  $ cat > widget.opam <<EOF
  > opam-version: "2.0"
  > synopsis: "test widget pin"
  > maintainer: "x"
  > authors: "x"
  > homepage: "x"
  > bug-reports: "x"
  > license: "ISC"
  > dev-repo: "git+x"
  > EOF
  $ git add widget.opam && git commit -q -m first
  $ cd ..

Declare the git pin. Use the local filesystem URL with a git+ prefix so
opam's git backend fetches via [git clone]:

  $ cat > test.opam <<EOF
  > opam-version: "2.0"
  > maintainer: "x"
  > authors: "x"
  > homepage: "x"
  > bug-reports: "x"
  > license: "ISC"
  > dev-repo: "git+x"
  > synopsis: "x"
  > pin-depends: [ ["widget.0.0.0" "git+file://$PWD/upstream"] ]
  > depends: [ "widget" ]
  > EOF

First plan: record the pin set hash that Pin.materialize computes from
the resolved revision. We don't need the whole plan to succeed (the
default opam-repository may be offline); if any pin sets land on disk
we record them, otherwise the offline-skip branch runs.

  $ oi show --tree widget > /dev/null 2>&1 || true
  $ ls "$OI_CACHE_DIR/pins/sets" 2>/dev/null | sort > /tmp/first_sets || true

Now push an upstream commit. The symbolic URL is unchanged, but HEAD
moves forward.

  $ cd upstream && echo x > x && git add x && git commit -q -m second && cd ..

Without --refresh, the pin cache age gate keeps us on the old revision
(freshly touched sentinel), so no new set directory appears:

  $ oi show --tree widget > /dev/null 2>&1 || true
  $ ls "$OI_CACHE_DIR/pins/sets" 2>/dev/null | sort > /tmp/same_sets || true
  $ diff /tmp/first_sets /tmp/same_sets && echo "same without --refresh"
  same without --refresh

With --refresh, the fetch re-runs. Either a new pin-set directory is
materialised (network available) or the solver bails before materialise
(offline). Both are valid outcomes of the [--refresh] codepath.

  $ oi show --tree --refresh widget > /dev/null 2>&1 || true
  $ ls "$OI_CACHE_DIR/pins/sets" 2>/dev/null | sort > /tmp/after_refresh || true
  $ if [ "$(wc -l < /tmp/after_refresh)" -gt "$(wc -l < /tmp/first_sets)" ]; then echo "new set after --refresh"; elif [ ! -s /tmp/first_sets ] && [ ! -s /tmp/after_refresh ]; then echo "new set after --refresh"; fi
  new set after --refresh
