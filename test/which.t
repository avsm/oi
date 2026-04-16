oi which looks up a well-known binary via the remote registry. Requires
network access to https://oi.ci.dev.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache
  $ oi which utop 2>&1 | grep -q "^utop "

A nonexistent pattern yields the "no binaries" fallback.

  $ oi which no-such-binary-xyzzy 2>&1 | grep -q "^No binaries"
