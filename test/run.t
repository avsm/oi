Full `oi run utop -- -version` end-to-end: solve, fetch layers (or build),
assemble prefix, execute. Slow on first run (minutes); pulls from the
oi.ci.dev registry if layers are cached there.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache
  $ export HOME=$PWD/home
  $ mkdir -p "$HOME"
  $ oi run utop -- -version 2>&1 | tail -1 | grep -q "compiled for OCaml version"
