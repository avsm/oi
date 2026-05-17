(* Tier 1 + Tier 2 coverage for [Oi.Manifest_registry]:
     - [path_for] points at [<cache>/layers/<os_key>/registry.json].
     - [ensure] writes the pointer on first call, becomes a no-op on the
       second call when the on-disk file already matches the current
       schema values. *)

module M = Oi.Manifest_registry

let test_path_for () =
  let p = M.path_for ~cache_root:"/c" ~os_key:"x86_64-linux" in
  Alcotest.(check string)
    "registry pointer path" "/c/layers/x86_64-linux/registry.json" p

let test_ensure_creates_file () =
  Helpers.with_eio_temp_dir ~prefix:"manifest_registry_create"
  @@ fun ~fs ~clock:_ ~dir _env ->
  let path = M.path_for ~cache_root:dir ~os_key:"x86_64-linux" in
  Alcotest.(check bool) "no file before ensure" false (Sys.file_exists path);
  M.ensure ~fs ~cache_root:dir ~os_key:"x86_64-linux" ~wrote_by:"oi";
  Alcotest.(check bool) "file present after ensure" true (Sys.file_exists path)

let test_ensure_is_idempotent () =
  Helpers.with_eio_temp_dir ~prefix:"manifest_registry_idem"
  @@ fun ~fs ~clock:_ ~dir _env ->
  M.ensure ~fs ~cache_root:dir ~os_key:"x86_64-linux" ~wrote_by:"oi";
  let path = M.path_for ~cache_root:dir ~os_key:"x86_64-linux" in
  let mtime_before = (Unix.stat path).st_mtime in
  let bytes_before = In_channel.with_open_text path In_channel.input_all in
  Unix.sleep 1;
  M.ensure ~fs ~cache_root:dir ~os_key:"x86_64-linux" ~wrote_by:"oi";
  let mtime_after = (Unix.stat path).st_mtime in
  let bytes_after = In_channel.with_open_text path In_channel.input_all in
  Alcotest.(check string) "bytes preserved" bytes_before bytes_after;
  Alcotest.(check (float 0.0))
    "mtime preserved (second ensure was a no-op)" mtime_before mtime_after

let test_ensure_rewrites_when_schema_old () =
  Helpers.with_eio_temp_dir ~prefix:"manifest_registry_old"
  @@ fun ~fs ~clock:_ ~dir _env ->
  let path = M.path_for ~cache_root:dir ~os_key:"x86_64-linux" in
  let layers_dir = Filename.concat dir "layers/x86_64-linux" in
  Helpers.mkdir_p (Filename.concat dir "layers");
  Helpers.mkdir_p layers_dir;
  let stale =
    {|{ "schema": 0, "kind": "oi.registry.v0", "os_key": "x86_64-linux", "layer_manifest_schema": 0, "build_manifest_schema": 0, "source_manifest_schema": 0, "wrote_by": "old", "date": "2020-01-01T00:00:00Z" }|}
  in
  Out_channel.with_open_text path (fun oc -> Out_channel.output_string oc stale);
  M.ensure ~fs ~cache_root:dir ~os_key:"x86_64-linux" ~wrote_by:"oi";
  let after = In_channel.with_open_text path In_channel.input_all in
  Alcotest.(check bool) "stale file was rewritten" true (after <> stale)

let suite =
  ( "manifest_registry",
    [
      Alcotest.test_case "path_for shape" `Quick test_path_for;
      Alcotest.test_case "ensure creates file" `Quick test_ensure_creates_file;
      Alcotest.test_case "ensure is idempotent" `Slow test_ensure_is_idempotent;
      Alcotest.test_case "ensure rewrites stale schema" `Quick
        test_ensure_rewrites_when_schema_old;
    ] )
