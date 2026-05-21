(lang dune 3.20)

(name oi)

(version 0.13.4)

(generate_opam_files true)

(maintainers "Anil Madhavapeddy <anil@recoil.org>")

(authors "Anil Madhavapeddy" "Mark Elvers" "Ryan Gibb")

(source (github avsm/oi))

(package (name osrel) (synopsis "OS and platform detection via Eio")
 (depends (ocaml (>= 5.2.0)) (eio (>= 1.0)) fmt))

(package (name d10)
 (synopsis "Content-addressed binary layer cache (day10 format)")
 (depends (ocaml (>= 5.2.0)) (eio (>= 1.0)) (osrel (= :version))
  (opam-format (>= 2.2.0)) (jsont (>= 0.2)) (bytesrw (>= 0.2)) sqlite3 requests (alcotest :with-test)
  nox-crypto-rng fmt logs))

(package (name oi) (synopsis "A stateless OCaml package builder")
 (depends (ocaml (>= 5.2.0)) dune-build-info (eio_main (>= 1.0))
  (d10 (= :version))
  (osrel (= :version)) (opam-format (>= 2.2.0)) (opam-state (>= 2.2.0))
  (opam-repository (>= 2.2.0))
  (opam-0install (>= 0.4)) nox-tty nox-tty-eio nox-xdg
  cmdliner fmt logs jsont bytesrw sqlite3 parsexp sexplib0 progress
  (dockerfile (>= 8.3.9)) (dockerfile-opam (>= 8.3.9))))
