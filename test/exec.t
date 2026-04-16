`oi exec` auto-syncs _oi/prefix/ from a minimal project's opam file on
first use, writes .envrc, and then runs the given command with the
prefix environment active.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache
  $ export HOME=$PWD/home
  $ mkdir -p "$HOME"
  $ cat > test.opam <<EOF
  > opam-version: "2.0"
  > name: "test"
  > version: "0.1"
  > synopsis: "test"
  > depends: [ "cmdliner" ]
  > EOF

The command's own output is the only output (silent by default).

  $ oi exec -- ocaml -version 2>&1 | grep -q "OCaml toplevel"

Sync as a side effect created the prefix and .envrc.

  $ test -d _oi/prefix
  $ test -f .envrc

`which ocaml` inside the exec env resolves to the assembled prefix.

  $ oi exec -- sh -c 'command -v ocaml' 2>&1 | grep -q "_oi/prefix/bin/ocaml"
