(* Tier 1 + Tier 2 coverage for [Oi.Manifest_build]:
     - JSON codec round-trips on a fully-populated [t].
     - [summary_of_events] correctly counts per-outcome buckets, including
       the [Restored] → [ok] folding (the manifest's [ok] bucket includes
       both fresh and restored events, while [cached] is its own bucket).
     - [write] persists into a [<YYYY>/<MM>/] tree and [read_all_at]
       recovers every entry written. *)

module M = Oi.Manifest_build
module A = Oi.Audit

let context : A.context =
  {
    overlay = None;
    toolchain = Some "ocaml-5.4";
    trigger = "oi build foo";
    project = Some "/work/foo";
    host = "ci-runner-1";
  }

let dummy_pkg name version : Oi.Identity.t = { name; version }
let dummy_dep n v hash : Oi.Identity.dep = { id = dummy_pkg n v; hash }

let event ?(outcome = Oi.Outcome.Ok) name version : A.event =
  {
    schema = 1;
    event_id = "01HQXTESTEVENT";
    invocation_id = "01HQXINVOCATION";
    ts = 1_700_000_000.0;
    os_key = "x86_64-linux";
    target = A.Layer (String.make 64 '0');
    pkg = dummy_pkg name version;
    outcome;
    duration_s = 1.5;
    context;
    log = None;
  }

let example_summary_inputs =
  [
    event "a" "1";
    event ~outcome:Oi.Outcome.Cached "b" "1";
    event ~outcome:Oi.Outcome.Restored "c" "1";
    event
      ~outcome:
        (Oi.Outcome.Build_failed { command = "make"; exit_code = Some 1 })
      "d" "1";
    event ~outcome:(Oi.Outcome.Skipped { reason = "dep failed" }) "e" "1";
  ]

let example_reporepo : M.reporepo =
  {
    url = Some "https://github.com/avsm/oi";
    commit = Some (String.make 40 'r');
    snapshot_key = None;
  }

let example_overlay : M.overlay_pin =
  { handle = "avsm"; version = "2024-01-01"; commit = None; url = None }

let example_target : M.target =
  { kind = "Plain"; handle = None; spec = Some "foo"; tokens = [ "foo" ] }

let example_solve : M.solve =
  {
    solve_key = Some "sk";
    schema = Some "v4";
    from_cache = false;
    resolved =
      [ { name = "ocaml"; version = "5.4.0"; hash = String.make 64 'd' } ];
  }

let example_full () =
  M.v ~invocation_id:"01HQXINVOCATION" ~os_key:"x86_64-linux"
    ~started_at:1_700_000_000.0 ~finished_at:1_700_000_010.0
    ~reporepo:example_reporepo ~overlays:[ example_overlay ] ~context
    ~targets:[ example_target ] ~solve:example_solve
    ~events:example_summary_inputs ()

let encode m =
  match Jsont_bytesrw.encode_string M.codec m with
  | Ok s -> s
  | Error e -> Alcotest.failf "encode: %s" e

let decode s =
  match Jsont_bytesrw.decode_string M.codec s with
  | Ok m -> m
  | Error e -> Alcotest.failf "decode: %s" e

let test_codec_roundtrip () =
  let m = example_full () in
  let encoded = encode m in
  let decoded = decode encoded in
  Alcotest.(check string) "invocation_id" m.invocation_id decoded.invocation_id;
  Alcotest.(check string) "os_key" m.os_key decoded.os_key;
  Alcotest.(check int)
    "overlays count" (List.length m.overlays)
    (List.length decoded.overlays);
  Alcotest.(check int)
    "events count" (List.length m.events)
    (List.length decoded.events);
  Alcotest.(check string) "re-encode is stable" encoded (encode decoded)

let test_summary_buckets () =
  let m = example_full () in
  Alcotest.(check int) "ok bucket folds Ok + Restored" 2 m.summary.ok;
  Alcotest.(check int) "cached bucket" 1 m.summary.cached;
  Alcotest.(check int) "fail bucket" 1 m.summary.fail;
  Alcotest.(check int) "skipped bucket" 1 m.summary.skipped;
  Alcotest.(check int) "timeout bucket empty" 0 m.summary.timeout

let contains needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec walk i =
    if i + n > h then false
    else if String.sub haystack i n = needle then true
    else walk (i + 1)
  in
  walk 0

let test_path_for_routes_by_month () =
  (* 2024-03-15 00:00:00 UTC = 1710460800. The path must include the
     UTC year and month derived from that timestamp. *)
  let path =
    M.path_for ~cache_root:"/c" ~os_key:"x86_64-linux" ~ts:1710460800.0
      ~invocation_id:"01HQ"
  in
  Alcotest.(check bool) "contains /2024/" true (contains "/2024/" path);
  Alcotest.(check bool) "contains /03/" true (contains "/03/" path)

(* -- Tier 2: write + read_all_at against a tempdir -------------------- *)

let timestamps = [ 1_700_000_000.0; 1_700_086_400.0; 1_710_000_000.0 ]

let test_write_and_read_all () =
  Helpers.with_eio_temp_dir ~prefix:"manifest_build"
  @@ fun ~fs ~clock:_ ~dir _env ->
  let manifests =
    List.mapi
      (fun i ts ->
        M.v
          ~invocation_id:(Printf.sprintf "01HQXTESTRUN%02d" i)
          ~os_key:"x86_64-linux" ~started_at:ts ~context
          ~events:[ event "a" "1" ]
          ())
      timestamps
  in
  List.iter (fun m -> M.write ~fs ~cache_root:dir m) manifests;
  let read =
    M.read_all_at ~root:(Filename.concat dir "layers/x86_64-linux/builds")
  in
  Alcotest.(check int)
    "every manifest read back" (List.length manifests) (List.length read);
  let written_ids =
    List.map (fun (m : M.t) -> m.invocation_id) manifests
    |> List.sort String.compare
  in
  let read_ids =
    List.map (fun (m : M.t) -> m.invocation_id) read |> List.sort String.compare
  in
  Alcotest.(check (list string)) "invocation_ids match" written_ids read_ids

let suite =
  ( "manifest_build",
    [
      Alcotest.test_case "codec round-trip" `Quick test_codec_roundtrip;
      Alcotest.test_case "summary_of_events buckets" `Quick test_summary_buckets;
      Alcotest.test_case "path_for routes by month" `Quick
        test_path_for_routes_by_month;
      Alcotest.test_case "write + read_all_at" `Quick test_write_and_read_all;
    ] )
