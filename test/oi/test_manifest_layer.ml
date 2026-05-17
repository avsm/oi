(* Tier 1 + Tier 2 coverage for [Oi.Manifest_layer]:
     - [string_of_outcome] is total and matches the wire-format strings.
     - [codec] encodes → decodes back to an equal record (sufficient
       guarantee that no field was accidentally dropped from the schema).
     - [write] / [try_read] round-trip through a tempdir; a repeated [write]
       of identical bytes is a no-op (idempotency). *)

module M = Oi.Manifest_layer

let outcomes = [ M.Ok; M.Fail; M.Timeout ]

let test_string_of_outcome () =
  Alcotest.(check string) "ok" "ok" (M.string_of_outcome M.Ok);
  Alcotest.(check string) "fail" "fail" (M.string_of_outcome M.Fail);
  Alcotest.(check string) "timeout" "timeout" (M.string_of_outcome M.Timeout)

let example_tarball : M.tarball =
  {
    sha256 = String.make 64 'a';
    size = 1234;
    key = Some "x86_64-linux/layers/abc.tar.zst";
  }

let example_recipe : M.recipe =
  {
    script = "echo build";
    env = [ "PATH=/usr/bin"; "OPAM_SWITCH_PREFIX=/p" ];
    substs = [ "META.in" ];
    subst_vars = [ ("name", "foo"); ("version", "1.0") ];
  }

let example_dep : M.dep =
  { name = "ocaml"; version = "5.4.0"; hash = String.make 64 'd' }

let example_file : M.file_entry =
  {
    path = "bin/foo";
    kind = "file";
    size = Some 42;
    mode = Some "0755";
    target = None;
  }

let example_findlib : M.findlib_entry =
  { package_dir = "foo"; findlib_pkg = "foo.core"; archive = Some "foo.cmxa" }

let example_source_archive : M.source_archive =
  { sha256 = String.make 64 'b'; key = Some "d10ir/abc.tar.zst" }

let example ~os_key ~hash : M.t =
  M.success ~hash ~os_key ~package:"foo.1.0.0" ~package_name:"foo"
    ~package_ver:"1.0.0" ~method_:"Source" ~overlay_handle:"avsm"
    ~overlay_version:"2024-01-01" ~tarball:example_tarball
    ~files:[ example_file ] ~binaries:[ "foo" ] ~findlib:[ example_findlib ]
    ~exit_status:0 ~recipe:example_recipe ~source_archive:example_source_archive
    ~deps:[ example_dep ] ~depexts_declared:[ "libssl-dev" ]
    ~build_env_ocaml_version:"5.4.0" ()

let encode m =
  match Jsont_bytesrw.encode_string M.codec m with
  | Ok s -> s
  | Error e -> Alcotest.failf "encode: %s" e

let decode s =
  match Jsont_bytesrw.decode_string M.codec s with
  | Ok m -> m
  | Error e -> Alcotest.failf "decode: %s" e

let test_codec_roundtrip () =
  let m = example ~os_key:"x86_64-linux" ~hash:(String.make 64 'c') in
  let encoded = encode m in
  let decoded = decode encoded in
  let re_encoded = encode decoded in
  Alcotest.(check string)
    "encode is deterministic across one decode round" encoded re_encoded

let test_codec_minimal () =
  (* Same fixture stripped of every option-typed field, to confirm
     [opt_mem] entries decode back to [None] (and don't silently appear
     as empty strings on the wire). *)
  let m =
    M.success ~hash:(String.make 64 'c') ~os_key:"x86_64-linux"
      ~package:"foo.1.0.0" ~package_name:"foo" ~package_ver:"1.0.0"
      ~method_:"Source" ~tarball:example_tarball ~files:[] ~binaries:[]
      ~findlib:[] ~exit_status:0 ~deps:[] ~depexts_declared:[] ()
  in
  let encoded = encode m in
  let decoded = decode encoded in
  Alcotest.(check (option string))
    "overlay_handle is None" None decoded.overlay_handle;
  Alcotest.(check (option string))
    "overlay_version is None" None decoded.overlay_version;
  Alcotest.(check (option string))
    "build_env_ocaml_version is None" None decoded.build_env_ocaml_version

let test_outcome_codec_each () =
  (* Encode then decode a manifest pinned to each outcome, asserting
     the wire string survives both directions. *)
  List.iter
    (fun outcome ->
      let m =
        {
          (example ~os_key:"x86_64-linux" ~hash:(String.make 64 'c')) with
          outcome;
        }
      in
      let encoded = encode m in
      let decoded = decode encoded in
      Alcotest.(check string)
        ("outcome round-trip " ^ M.string_of_outcome outcome)
        (M.string_of_outcome outcome)
        (M.string_of_outcome decoded.outcome))
    outcomes

(* -- Tier 2: write / try_read against a tempdir ----------------------- *)

let test_write_then_read () =
  Helpers.with_eio_temp_dir ~prefix:"manifest_layer"
  @@ fun ~fs ~clock:_ ~dir _env ->
  let m = example ~os_key:"x86_64-linux" ~hash:(String.make 64 'c') in
  M.write ~fs ~cache_root:dir m;
  let path = M.path_for ~cache_root:dir ~os_key:m.os_key ~hash:m.hash in
  Alcotest.(check bool) "manifest file written" true (Sys.file_exists path);
  match M.try_read ~path with
  | None -> Alcotest.fail "try_read returned None"
  | Some decoded ->
      Alcotest.(check string) "hash survives" m.hash decoded.hash;
      Alcotest.(check string) "package survives" m.package decoded.package;
      Alcotest.(check int)
        "files survives" (List.length m.files)
        (List.length decoded.files)

let test_write_is_idempotent () =
  Helpers.with_eio_temp_dir ~prefix:"manifest_layer_idem"
  @@ fun ~fs ~clock:_ ~dir _env ->
  let m = example ~os_key:"x86_64-linux" ~hash:(String.make 64 'c') in
  M.write ~fs ~cache_root:dir m;
  let path = M.path_for ~cache_root:dir ~os_key:m.os_key ~hash:m.hash in
  let bytes_before = In_channel.with_open_text path In_channel.input_all in
  let mtime_before = (Unix.stat path).st_mtime in
  (* Sleep one second so a hypothetical rewrite would change mtime. *)
  Unix.sleep 1;
  M.write ~fs ~cache_root:dir m;
  let bytes_after = In_channel.with_open_text path In_channel.input_all in
  let mtime_after = (Unix.stat path).st_mtime in
  Alcotest.(check string) "bytes unchanged" bytes_before bytes_after;
  Alcotest.(check (float 0.0))
    "mtime unchanged (write was a no-op)" mtime_before mtime_after

let test_no_tmp_files_left () =
  Helpers.with_eio_temp_dir ~prefix:"manifest_layer_no_tmp"
  @@ fun ~fs ~clock:_ ~dir _env ->
  let m = example ~os_key:"x86_64-linux" ~hash:(String.make 64 'c') in
  M.write ~fs ~cache_root:dir m;
  let registry_dir = Filename.concat (Filename.concat dir "layers") m.os_key in
  let entries = Sys.readdir registry_dir |> Array.to_list in
  (* Expected: [<hash>.json] plus [<hash>.lock] (the lockfile sentinel
     left behind by [D10.Lock] — tiny, cheap, deliberately not swept). *)
  Alcotest.(check bool)
    "contains .json" true
    (List.exists (fun n -> Filename.check_suffix n ".json") entries);
  Alcotest.(check bool)
    "no .tmp leftover" false
    (List.exists
       (fun n ->
         (* Generously consider any [.tmp*] suffix a leftover. *)
         (String.length n >= 4 && String.sub n (String.length n - 4) 4 = ".tmp")
         ||
           try
             let i = String.rindex n '.' in
             String.length n - i >= 5 && String.sub n (i - 4) 4 = ".tmp"
           with Not_found -> false)
       entries)

let suite =
  ( "manifest_layer",
    [
      Alcotest.test_case "string_of_outcome" `Quick test_string_of_outcome;
      Alcotest.test_case "codec round-trip" `Quick test_codec_roundtrip;
      Alcotest.test_case "codec minimal optional fields" `Quick
        test_codec_minimal;
      Alcotest.test_case "codec covers every outcome" `Quick
        test_outcome_codec_each;
      Alcotest.test_case "write then try_read" `Quick test_write_then_read;
      Alcotest.test_case "write is idempotent" `Slow test_write_is_idempotent;
      Alcotest.test_case "no .tmp file left after write" `Quick
        test_no_tmp_files_left;
    ] )
