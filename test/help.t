Top-level help mentions the tool description.

  $ oi --help=plain 2>&1 | grep -q "fast, stateless OCaml package manager"

Every top-level subcommand has its own help page.

  $ for c in run add exec search plan sync env config clean registry; do
  >   oi "$c" --help=plain >/dev/null || echo "FAIL: $c"
  > done

Unknown subcommand exits non-zero.

  $ oi no-such-command >/dev/null 2>&1
  [124]
