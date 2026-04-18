`oi add` pre-flight guards: we only want to exercise the checks that
fire before [do_sync] spawns the solver; a full solve would take
tens of seconds and need network. Use a subshell in a temp dir
everywhere so [cwd] behaviour is deterministic.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache

Missing dune-project exits 1 with a clear error.

  $ mkdir -p t1 && cd t1 && oi add fmt 2>&1 | tail -1; cd ..
  error: no dune-project at $TESTCASE_ROOT/t1/dune-project

`(generate_opam_files)` absent → error before any repo sync.

  $ mkdir -p t2 && cd t2 && printf '(lang dune 3.20)\n(package (name foo))\n' > dune-project && oi add fmt 2>&1 | tail -1; cd ..
  error: dune-project does not have (generate_opam_files): oi add only supports projects where dune owns the *.opam files

Multi-package without `-p` → error asks for disambiguation.

  $ mkdir -p t3 && cd t3
  $ cat > dune-project <<'EOF'
  > (lang dune 3.20)
  > (generate_opam_files)
  > (package (name foo))
  > (package (name bar))
  > EOF
  $ oi add fmt 2>&1 | tail -1
  error: multiple packages in dune-project (foo, bar); re-run with -p PKG to pick one

`-p` naming a non-existent stanza must error up-front (before the sync
would mutate `_oi/prefix`).

  $ oi add fmt -p nope 2>&1 | tail -1
  error: no (package (name nope) …) stanza in dune-project (declared: foo, bar)
  $ cd ..
