Source mirror: an empty cache reports zero blobs, and `gc`/`verify`
succeed against the empty state without touching the filesystem.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache
  $ oi registry mirror stats 2>&1 | sed "s|$PWD|.|"
  Mirror: ./cache/mirror
    blobs:      0
    total size: 0B

`gc` on an empty mirror removes nothing and exits 0.

  $ oi registry mirror gc
  Removed 0 orphaned blob(s)

`verify` on an empty mirror reports OK.

  $ oi registry mirror verify
  All blobs verified OK

No mirror dir exists yet — it's materialised when the first successful
fetch promotes a blob into it via `Source_mirror.record`.

  $ test ! -d cache/mirror && echo no-mirror-yet
  no-mirror-yet
