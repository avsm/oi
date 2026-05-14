Top-level help mentions the tool description.

  $ oi --help=plain 2>&1 | grep -qi "fast, stateless OCaml package manager"

Every top-level subcommand has its own help page.

  $ for c in run add exec search show env config clean repo build install; do
  >   oi "$c" --help=plain >/dev/null || echo "FAIL: $c"
  > done

Unknown subcommand exits non-zero.

  $ oi no-such-command >/dev/null 2>&1
  [124]
