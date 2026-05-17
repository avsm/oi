(* Tier 2 coverage for [Oi.Keys.read_string_ext]:
     - reads back a string-valued [x-*] extension.
     - returns [None] for an extension that is non-string (e.g. a list
       literal).
     - returns [None] for a missing key. *)

let parse_opam s = OpamFile.OPAM.read_from_string s

let opam_with_extensions ~archive_value =
  Printf.sprintf
    {|opam-version: "2.0"
name: "foo"
version: "1.0"
x-d10-archive: %s
x-repos: ["@avsm" "@samoht"]
|}
    archive_value

let test_read_string_present () =
  let opam = parse_opam (opam_with_extensions ~archive_value:"\"abcdef\"") in
  Alcotest.(check (option string))
    "x-d10-archive is read as string" (Some "abcdef")
    (Oi.Keys.read_string_ext Oi.Keys.d10_archive opam)

let test_read_string_non_string_returns_none () =
  let opam = parse_opam (opam_with_extensions ~archive_value:"\"abc\"") in
  Alcotest.(check (option string))
    "x-repos is a list — returns None when asked for string" None
    (Oi.Keys.read_string_ext Oi.Keys.repos opam)

let test_read_missing_key () =
  let opam = parse_opam (opam_with_extensions ~archive_value:"\"abc\"") in
  Alcotest.(check (option string))
    "x-oi-toolchain is absent — returns None" None
    (Oi.Keys.read_string_ext Oi.Keys.toolchain opam)

let test_key_constants () =
  Alcotest.(check string) "d10_archive name" "x-d10-archive" Oi.Keys.d10_archive;
  Alcotest.(check string) "overlay name" "x-oi-overlay" Oi.Keys.overlay;
  Alcotest.(check string) "repos name" "x-repos" Oi.Keys.repos

let suite =
  ( "keys",
    [
      Alcotest.test_case "read_string_ext on a string extension" `Quick
        test_read_string_present;
      Alcotest.test_case "read_string_ext on a list extension returns None"
        `Quick test_read_string_non_string_returns_none;
      Alcotest.test_case "read_string_ext on a missing key returns None" `Quick
        test_read_missing_key;
      Alcotest.test_case "key constants match the opam x-* names" `Quick
        test_key_constants;
    ] )
