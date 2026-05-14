`oi show --tree` prints a dependency tree headed by the target package
with its layer hash on the same line.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache
  $ export HOME=$PWD/home
  $ mkdir -p "$HOME"
  $ oi show --tree utop 2>&1 | grep -q "^Dependency tree"
  $ oi show --tree utop 2>&1 | grep -qE "^utop\.[0-9]"

`oi show` without `--tree` also surfaces utop and its leaf dependencies
under the tree.

  $ oi show utop 2>&1 | grep -qE "^utop\.[0-9]"
  $ oi show utop 2>&1 | grep -qE "ocaml\.[0-9]"
