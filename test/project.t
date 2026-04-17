Project.load is invoked via 'oi config' and does not crash on
x-opam-repositories: or pin-depends: fields.

  $ cat > foo.opam <<EOF
  > opam-version: "2.0"
  > maintainer: "t"
  > authors: "t"
  > homepage: "x"
  > bug-reports: "x"
  > license: "ISC"
  > dev-repo: "git+x"
  > synopsis: "t"
  > depends: [ "fmt" ]
  > x-opam-repositories: [ ["mirage" "https://github.com/mirage/opam-overlays.git"] ]
  > pin-depends: [ ["bar.dev" "git+https://example.com/bar.git#main"] ]
  > EOF
  $ oi config 2>&1 | grep -q 'cache:' && echo ok
  ok
