(* Tier 1 + Tier 2 coverage for [Oi.Source_manifest]:
     - codec round-trip on a fully-populated [t] (every optional field
       set) and on a minimal one (every option absent).
     - [v] constructed from bake inputs against a tempdir build_dir.
       The [.git]-absent path returns [commit_sha = None]; an extra-file
       fixture has its sha256 + size populated.
     - [write] persists into the archives dir and is idempotent (no-op
       when the destination already exists). *)

module M = Oi.Source_manifest

let example_source : M.source =
  {
    kind = "git";
    url = Some "https://github.com/avsm/oi";
    ref_ = Some "main";
    commit_sha = Some (String.make 40 'c');
    checksums = [ "md5=00112233445566778899aabbccddeeff" ];
  }

let example_extra : M.extra_source =
  { name = "patches.tar.gz"; url = "https://example.com/p.tgz"; checksums = [] }

let example_file : M.extra_file =
  { path = "patch.diff"; sha256 = String.make 64 'a'; size = 42 }

let example_patch : M.patch =
  { file = "fix.patch"; filter = Some "{os = \"linux\"}" }

let example ~sha256 : M.t =
  {
    schema = 1;
    kind = "oi.source.v1";
    sha256;
    date = "2024-01-01T00:00:00Z";
    package_name = "foo";
    package_version = "1.0.0";
    overlay_handle = Some "avsm";
    overlay_version = Some "2024-01-01";
    source = example_source;
    extra_sources = [ example_extra ];
    extra_files = [ example_file ];
    patches = [ example_patch ];
    substs = [ "META.in" ];
    strip_components = 0;
    size = Some 1024;
  }

let example_minimal ~sha256 : M.t =
  {
    schema = 1;
    kind = "oi.source.v1";
    sha256;
    date = "2024-01-01T00:00:00Z";
    package_name = "foo";
    package_version = "1.0.0";
    overlay_handle = None;
    overlay_version = None;
    source =
      {
        kind = "none";
        url = None;
        ref_ = None;
        commit_sha = None;
        checksums = [];
      };
    extra_sources = [];
    extra_files = [];
    patches = [];
    substs = [];
    strip_components = 0;
    size = None;
  }

let encode m =
  match Jsont_bytesrw.encode_string M.codec m with
  | Ok s -> s
  | Error e -> Alcotest.failf "encode: %s" e

let decode s =
  match Jsont_bytesrw.decode_string M.codec s with
  | Ok m -> m
  | Error e -> Alcotest.failf "decode: %s" e

let test_codec_full () =
  let m = example ~sha256:(String.make 64 's') in
  let encoded = encode m in
  let decoded = decode encoded in
  Alcotest.(check string) "sha256 round-trips" m.sha256 decoded.sha256;
  Alcotest.(check int)
    "extra_files count"
    (List.length m.extra_files)
    (List.length decoded.extra_files);
  Alcotest.(check string) "re-encode is stable" encoded (encode decoded)

let test_codec_minimal () =
  let m = example_minimal ~sha256:(String.make 64 's') in
  let encoded = encode m in
  let decoded = decode encoded in
  Alcotest.(check (option string))
    "overlay_handle absent" None decoded.overlay_handle;
  Alcotest.(check (option int)) "size absent" None decoded.size;
  Alcotest.(check (list string)) "substs absent" [] decoded.substs

(* -- Tier 2: [v] from bake inputs + [write] / [try_read] -------------- *)

let test_v_no_git_returns_no_commit () =
  Helpers.with_eio_temp_dir ~prefix:"source_manifest_v"
  @@ fun ~fs:_ ~clock:_ ~dir env ->
  let proc_mgr = Eio.Stdenv.process_mgr env in
  (* Build a build_dir with no [.git] subdirectory; [resolve_git_commit]
     must short-circuit to [None] without spawning [git]. *)
  let build_dir = Filename.concat dir "build" in
  Helpers.mkdir_p build_dir;
  let m =
    M.v ~proc_mgr ~build_dir ~name:"foo" ~version:"1.0.0"
      ~sha256:(String.make 64 'a') ~size:42 ~strip_components:0
      ~source:
        (Some
           {
             url = "https://github.com/avsm/oi";
             checksums = [ "md5=00112233445566778899aabbccddeeff" ];
           })
      ~extra_sources:[] ~extra_files:[] ~patches:[] ~substs:[] ()
  in
  Alcotest.(check (option string))
    "no commit_sha for non-git build_dir" None m.source.commit_sha

let test_v_extra_file_sha () =
  Helpers.with_eio_temp_dir ~prefix:"source_manifest_xf"
  @@ fun ~fs:_ ~clock:_ ~dir env ->
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let build_dir = Filename.concat dir "build" in
  Helpers.mkdir_p build_dir;
  let payload = "hello\n" in
  let src_path = Filename.concat build_dir "fix.patch" in
  Out_channel.with_open_text src_path (fun oc ->
      Out_channel.output_string oc payload);
  let m =
    M.v ~proc_mgr ~build_dir ~name:"foo" ~version:"1.0.0"
      ~sha256:(String.make 64 'a') ~source:None ~extra_sources:[]
      ~extra_files:[ ("fix.patch", src_path) ]
      ~patches:[] ~substs:[] ()
  in
  match m.extra_files with
  | [ ef ] ->
      Alcotest.(check string) "path" "fix.patch" ef.path;
      Alcotest.(check int) "size" (String.length payload) ef.size;
      Alcotest.(check int) "sha256 length" 64 (String.length ef.sha256)
  | _ -> Alcotest.fail "expected exactly one extra_file"

let test_write_then_try_read () =
  Helpers.with_eio_temp_dir ~prefix:"source_manifest_io"
  @@ fun ~fs ~clock:_ ~dir _env ->
  let m = example ~sha256:(String.make 64 's') in
  M.write ~fs ~archives_dir:dir m;
  let path = M.path_for ~archives_dir:dir ~sha:m.sha256 in
  Alcotest.(check bool) "file present" true (Sys.file_exists path);
  match M.try_read ~path with
  | None -> Alcotest.fail "try_read returned None"
  | Some decoded ->
      Alcotest.(check string) "sha256 survives" m.sha256 decoded.sha256;
      Alcotest.(check string)
        "package_name survives" m.package_name decoded.package_name

let test_write_is_idempotent () =
  Helpers.with_eio_temp_dir ~prefix:"source_manifest_idem"
  @@ fun ~fs ~clock:_ ~dir _env ->
  let m = example ~sha256:(String.make 64 's') in
  M.write ~fs ~archives_dir:dir m;
  let path = M.path_for ~archives_dir:dir ~sha:m.sha256 in
  let mtime_before = (Unix.stat path).st_mtime in
  Unix.sleep 1;
  (* Try writing a different value to the same sha — must NOT clobber:
     [Source_manifest.write] short-circuits when the destination exists. *)
  let m' = { m with package_name = "bar" } in
  M.write ~fs ~archives_dir:dir m';
  let mtime_after = (Unix.stat path).st_mtime in
  let on_disk = M.try_read ~path in
  Alcotest.(check (float 0.0))
    "mtime preserved (second write skipped)" mtime_before mtime_after;
  match on_disk with
  | Some decoded ->
      Alcotest.(check string)
        "original package_name retained" "foo" decoded.package_name
  | None -> Alcotest.fail "decode of preserved file failed"

let suite =
  ( "source_manifest",
    [
      Alcotest.test_case "codec full round-trip" `Quick test_codec_full;
      Alcotest.test_case "codec minimal round-trip" `Quick test_codec_minimal;
      Alcotest.test_case "v returns None commit_sha when no .git" `Quick
        test_v_no_git_returns_no_commit;
      Alcotest.test_case "v populates extra_file sha + size" `Quick
        test_v_extra_file_sha;
      Alcotest.test_case "write then try_read" `Quick test_write_then_try_read;
      Alcotest.test_case "write is idempotent (no clobber)" `Slow
        test_write_is_idempotent;
    ] )
