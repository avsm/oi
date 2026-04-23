oi search looks up a binary or package across the local cache, the
remote registry, and the overlays in the reporepo. Requires network
access to https://oi.ci.dev.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache

A known binary is reported as a `bin` row.

  $ oi search utop 2>&1 | grep -qE "^bin "

A nonexistent pattern yields the "no matches" fallback.

  $ oi search no-such-package-xyzzy 2>&1 | grep -q "^No matches"
