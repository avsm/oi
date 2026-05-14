Source mirror: `oi config` reports the mirror directory and its blob
count in the [Source mirror] block. An empty cache reports zero blobs;
the dedicated `oi registry mirror stats/gc/verify` subcommands were
removed when the mirror's lifecycle moved fully under `oi clean`.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache
  $ oi config 2>&1 | sed -n '/^Source mirror/,/^$/p' | head -4 | sed "s|$PWD|.|"
  Source mirror
    dir:        ./cache/mirror
    blobs:      0
    total size: 0B

No mirror dir exists yet — it's materialised when the first successful
fetch promotes a blob into it via `Source_mirror.record`.

  $ test ! -d cache/mirror && echo no-mirror-yet
  no-mirror-yet

`oi clean --mirror` is the new way to drop the content-addressed
source mirror; on an empty cache it's a no-op that prints "Done".

  $ oi clean --mirror 2>&1 | tail -1
  ▸ Done
