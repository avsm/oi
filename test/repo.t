The `oi repo` subcommands manage a reporepo of overlay pins: a
parallel opam-layout directory where each package points at a
specific commit of someone else's opam-repository clone.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache
  $ export OI_REPOREPO=$PWD/repopo

An empty reporepo lists cleanly.

  $ oi repo list 2>&1 | sed "s|$PWD|.|"
  Reporepo ./repopo is empty.

`show` on a missing handle exits with an error.

  $ oi repo show avsm 2>&1 | tail -1 | sed "s|$PWD|.|"
  error: avsm: no overlay avsm in reporepo ./repopo

`remove` on a missing handle likewise.

  $ oi repo remove avsm 2>&1 | tail -1
  error: no such overlay avsm in reporepo

The `add` / `bump` / `refresh` paths shell out to `git ls-remote` and
are exercised by hand against real remotes; cram doesn't cover them
because dune's build sandbox places a stub `.git` file that git
ls-remote then trips over when it walks ancestor dirs looking for
repo context.

`@handle` prefix syntax: `@handle/pkg` pins `pkg` to the overlay's
version; `@handle` alone (for `oi registry build`) means every
package the overlay contributes. `oi run` rejects the bare `@handle`
form because there's no package to run.

  $ oi run @avsm 2>&1 | tail -1
  error: overlay handle "avsm" given without a package (use '@avsm/PKG')
