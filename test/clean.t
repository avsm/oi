With no arguments, oi clean lists cleanable items against an empty cache.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache
  $ oi clean 2>&1 | head -1
  Cleanable items:

Dry-run --all against an empty cache exits cleanly with a trailing Done.

  $ oi clean --all --dry-run 2>&1 | tail -1
  Done.
