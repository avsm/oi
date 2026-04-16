Config reflects sandboxed XDG env vars and the default registry URL.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache
  $ oi config 2>&1 | grep -q "data:       $PWD/data"
  $ oi config 2>&1 | grep -q "cache:      $PWD/cache"
  $ oi config 2>&1 | grep -q "https://oi.ci.dev"

XDG_DATA_HOME / XDG_CACHE_HOME are used as fallbacks when OI_* are unset.

  $ unset OI_DATA_DIR OI_CACHE_DIR
  $ export XDG_DATA_HOME=$PWD/xdg-data
  $ export XDG_CACHE_HOME=$PWD/xdg-cache
  $ oi config 2>&1 | grep -q "data:       $PWD/xdg-data/oi"
  $ oi config 2>&1 | grep -q "cache:      $PWD/xdg-cache/oi"
