(lang dune 3.20)
(name oi)
(generate_opam_files true)
(maintainers "Anil Madhavapeddy <anil@recoil.org>")
(authors "Anil Madhavapeddy" "Mark Elvers")
(source (github avsm/oi))

(package
 (name osrel)
 (synopsis "OS and platform detection via Eio")
 (depends
  (ocaml (>= 5.2.0))
  (eio (>= 1.0))
  fmt))

(package
 (name d10)
 (synopsis "Content-addressed binary layer cache (day10 format)")
 (depends
  (ocaml (>= 5.2.0))
  (eio (>= 1.0))
  (osrel (= :version))
  (opam-format (>= 2.2.0))
  (jsont (>= 0.2))
  (bytesrw (>= 0.2))
  sqlite3
  cmdliner
  (eio_main (>= 1.0))
  fmt
  logs))

(package
 (name oi)
 (synopsis "A stateless OCaml package builder")
 (depends
  (ocaml (>= 5.2.0))
  (eio_main (>= 1.0))
  (d10 (= :version))
  (osrel (= :version))
  (opam-format (>= 2.2.0))
  (opam-state (>= 2.2.0))
  (opam-client (>= 2.2.0))
  (opam-0install (>= 0.4))
  (progress (>= 0.4))
  (xdge (>= 1.0))
  cmdliner
  fmt
  logs
  jsont
  bytesrw
  tomlt
  (dockerfile (>= 8.3.0))
  (dockerfile-opam (>= 8.3.0))))
