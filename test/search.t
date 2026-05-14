oi search looks up a binary or package across the local cache, the
remote registry, and the overlays in the reporepo. The registry side
consumes [<base>/<os_key>/index-full.json] — if it's missing
(unreachable or not yet published), only the local cache and the
declared-overlay rows surface.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache

A known package surfaces either as a remote `bin` row (registry
reachable + publishes index-full.json) or as a `pkg declared` row
from the in-repo overlays — both prove the search pipeline ran.

  $ oi search utop 2>&1 | grep -qE "utop"

A nonexistent pattern yields the "no matches" fallback.

  $ oi search no-such-package-xyzzy 2>&1 | grep -q "^No matches"
