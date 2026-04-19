The 'oi config' Dev tools block reflects the project state.
Probes run against [.ocamlformat] and [dune-project] to decide which
tools will be installed into [_oi/tools/] on the next 'oi sync'.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache

With neither .ocamlformat nor a dune-project, only the Always tools
hit and the optional ones miss.

  $ oi config 2>&1 | awk '/^Dev tools:/,/^$/'
  Dev tools:
    odoc               hit  always
    merlin             hit  always
    ocaml-lsp-server   hit  always
    mdx                miss dune-project: no (using mdx ...)
    ocamlformat        miss .ocamlformat: missing

Adding a dune-project with (using mdx ...) flips mdx to hit.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (using mdx 0.2)
  > EOF
  $ oi config 2>&1 | awk '/^Dev tools:/,/^$/' | grep mdx
    mdx                hit  dune-project: (using mdx ...)

A .ocamlformat with a version line flips ocamlformat to hit and
surfaces the version in the detail column.

  $ cat > .ocamlformat <<EOF
  > version = 0.28.1
  > profile = ocamlformat
  > EOF
  $ oi config 2>&1 | awk '/^Dev tools:/,/^$/' | grep ocamlformat
    ocamlformat        hit  .ocamlformat: version = 0.28.1

A .ocamlformat without a version line is a miss (ocamlformat refuses
to run without one, so installing it would be pointless).

  $ cat > .ocamlformat <<EOF
  > profile = ocamlformat
  > EOF
  $ oi config 2>&1 | awk '/^Dev tools:/,/^$/' | grep ocamlformat
    ocamlformat        miss .ocamlformat: no 'version = ...' line
