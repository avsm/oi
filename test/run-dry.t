Dry-run `oi run -n` emits a flattened list of package.version + layer hash
pairs, with the target package present on its own line.
On first run this clones the opam repositories; subsequent runs are cheaper.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache
  $ export HOME=$PWD/home
  $ mkdir -p "$HOME"
  $ oi run -n utop 2>&1 | grep -q "^utop\."
