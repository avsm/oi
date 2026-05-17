(* Tier 1 coverage for [Oi.Provenance]:
     - codec round-trip on a fully-populated [t].
     - [hash_opam_file] on a non-existent file returns the empty string
       (the documented contract for missing inputs). *)

module P = Oi.Provenance

let example_origin : Oi.Origin.t =
  {
    kind = Oi.Origin.Reporepo;
    overlay = Some { handle = "avsm"; version = "2024-01-01" };
    path_in_repo = "v2/avsm/packages/foo/foo.1.0/opam";
  }

let example_phases : P.phases =
  { fetch = Some 0.2; build = Some 12.5; install = Some 0.7; restore = None }

let example_source : P.source_info =
  {
    url = "https://github.com/avsm/foo";
    kind = "git";
    checksums = [ "sha256=" ^ String.make 64 'a' ];
  }

let example_pkg : Oi.Identity.t = { name = "foo"; version = "1.0" }

let example_dep : Oi.Identity.dep =
  { id = example_pkg; hash = String.make 64 'd' }

let example () : P.t =
  {
    schema = 1;
    layer_hash = String.make 64 'h';
    os_key = "x86_64-linux";
    pkg = example_pkg;
    method_ = Oi.Identity.Source;
    built_at = 1_700_000_000.0;
    duration_s = 12.5;
    phases = example_phases;
    opam = { sha256 = String.make 64 'o'; origin = example_origin };
    source = Some example_source;
    deps = [ example_dep ];
    depexts_declared = [ "libssl-dev" ];
    build_env = { ocaml_version = "5.4.0" };
  }

let encode p =
  match Jsont_bytesrw.encode_string P.codec p with
  | Ok s -> s
  | Error e -> Alcotest.failf "encode: %s" e

let decode s =
  match Jsont_bytesrw.decode_string P.codec s with
  | Ok p -> p
  | Error e -> Alcotest.failf "decode: %s" e

let test_codec_roundtrip () =
  let p = example () in
  let encoded = encode p in
  let decoded = decode encoded in
  Alcotest.(check string) "layer_hash" p.layer_hash decoded.layer_hash;
  Alcotest.(check string) "pkg.name" p.pkg.name decoded.pkg.name;
  Alcotest.(check int)
    "deps count" (List.length p.deps) (List.length decoded.deps);
  Alcotest.(check string) "re-encode is stable" encoded (encode decoded)

let test_hash_opam_file_missing () =
  let p = "/definitely-not-a-real/opam.file" in
  Alcotest.(check string)
    "missing file → empty hash" "" (P.hash_opam_file ~path:p)

let suite =
  ( "provenance",
    [
      Alcotest.test_case "codec round-trip" `Quick test_codec_roundtrip;
      Alcotest.test_case "hash_opam_file on missing file" `Quick
        test_hash_opam_file_missing;
    ] )
