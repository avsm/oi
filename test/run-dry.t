Dry-run `oi run -n` emits a staged build tree that mentions utop.
On first run this clones the opam repositories; subsequent runs are cheaper.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache
  $ export HOME=$PWD/home
  $ mkdir -p "$HOME"
  $ oi run -n utop 2>&1 | grep -q "^stage 1"
  $ oi run -n utop 2>&1 | grep -q "utop\."
