`oi plan` prints a fully resolved plan header followed by per-package entries.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache
  $ export HOME=$PWD/home
  $ mkdir -p "$HOME"
  $ oi plan utop 2>&1 | grep -q "^Plan:"
  $ oi plan utop 2>&1 | grep -q "^  utop\."
  $ oi plan utop 2>&1 | grep -q "^    layer: "
