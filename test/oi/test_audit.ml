(* Tier 1 + Tier 2 coverage for [Oi.Audit]:
     - event codec round-trips through both [event_target] variants
       (Layer and Solve_key) — the tagged-object encoding is what
       makes the schema future-extensible.
     - ULID format: 26 chars, only Crockford alphabet, two adjacent
       calls share a leading prefix (because the timestamp wins).
     - [tail_of_file] returns the trailing N lines of a fixture. *)

module A = Oi.Audit

let crockford = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
let in_crockford c = String.contains crockford c
let dummy_pkg name version : Oi.Identity.t = { name; version }

let dummy_context : A.context =
  {
    overlay = None;
    toolchain = None;
    trigger = "oi build foo";
    project = None;
    host = "ci";
  }

let event ?(outcome = Oi.Outcome.Ok) ?(target = A.Layer (String.make 64 '0'))
    ?(log = None) name : A.event =
  {
    schema = 1;
    event_id = "01HQXTESTEVENT";
    invocation_id = "01HQXTESTINVO";
    ts = 1_700_000_000.0;
    os_key = "x86_64-linux";
    target;
    pkg = dummy_pkg name "1.0";
    outcome;
    duration_s = 1.0;
    context = dummy_context;
    log;
  }

let encode_event e =
  match Jsont_bytesrw.encode_string A.event_codec e with
  | Ok s -> s
  | Error e -> Alcotest.failf "encode: %s" e

let decode_event s =
  match Jsont_bytesrw.decode_string A.event_codec s with
  | Ok e -> e
  | Error e -> Alcotest.failf "decode: %s" e

let test_event_codec_layer () =
  let e = event "foo" in
  let decoded = decode_event (encode_event e) in
  Alcotest.(check string) "event_id" e.event_id decoded.event_id;
  match decoded.target with
  | A.Layer h ->
      Alcotest.(check string)
        "layer hash survives"
        (A.hash_of_target e.target)
        h
  | A.Solve_key _ -> Alcotest.fail "Layer target decoded as Solve_key"

let test_event_codec_solve_key () =
  let e = event "foo" ~target:(A.Solve_key (String.make 64 's')) in
  let decoded = decode_event (encode_event e) in
  match decoded.target with
  | A.Solve_key h ->
      Alcotest.(check string)
        "solve_key hash survives"
        (A.hash_of_target e.target)
        h
  | A.Layer _ -> Alcotest.fail "Solve_key target decoded as Layer"

let test_event_codec_with_log () =
  let log : A.log_pointer =
    { text_path = "/path/to/log"; tail = Some "tail line\nlast line" }
  in
  let e = event "foo" ~log:(Some log) in
  let decoded = decode_event (encode_event e) in
  match decoded.log with
  | Some l ->
      Alcotest.(check string) "log path" log.text_path l.text_path;
      Alcotest.(check (option string)) "log tail" log.tail l.tail
  | None -> Alcotest.fail "log dropped during round-trip"

let test_ulid_format () =
  let id = A.ulid () in
  Alcotest.(check int) "ULID length is 26" 26 (String.length id);
  String.iter
    (fun c ->
      Alcotest.(check bool)
        (Printf.sprintf "%c is in Crockford alphabet" c)
        true (in_crockford c))
    id

let test_ulid_time_prefix_orders () =
  let a = A.ulid () in
  let b = A.ulid () in
  let prefix_a = String.sub a 0 10 in
  let prefix_b = String.sub b 0 10 in
  Alcotest.(check bool)
    "second ULID has time prefix >= first" true (prefix_a <= prefix_b)

(* -- Tier 2: tail_of_file ------------------------------------------------ *)

let test_tail_of_file_truncates () =
  Helpers.with_temp_dir ~prefix:"audit_tail" @@ fun dir ->
  let path = Filename.concat dir "build.log" in
  Out_channel.with_open_text path (fun oc ->
      for i = 1 to 500 do
        Printf.fprintf oc "line %d\n" i
      done);
  match A.tail_of_file ~lines:150 ~path () with
  | None -> Alcotest.fail "tail returned None for an existing file"
  | Some s ->
      let actual =
        String.split_on_char '\n' s |> List.filter (fun l -> l <> "")
      in
      (* The exact boundary depends on whether the split's trailing
         empty string counts toward [lines]; we just assert the right
         neighbourhood (last ~150 lines, including line 500, none from
         the early part of the file). *)
      let n = List.length actual in
      Alcotest.(check bool)
        "between 149 and 150 non-empty lines" true
        (n = 149 || n = 150);
      Alcotest.(check string)
        "last line is line 500" "line 500"
        (List.nth actual (n - 1));
      Alcotest.(check bool)
        "first line is in the last 150 of the file" true
        (let first = List.hd actual in
         first = "line 351" || first = "line 352")

let test_tail_of_file_short_file () =
  Helpers.with_temp_dir ~prefix:"audit_tail_short" @@ fun dir ->
  let path = Filename.concat dir "short.log" in
  Out_channel.with_open_text path (fun oc ->
      Printf.fprintf oc "alpha\nbeta\ngamma\n");
  match A.tail_of_file ~lines:150 ~path () with
  | None -> Alcotest.fail "tail returned None"
  | Some s ->
      let lines =
        String.split_on_char '\n' s |> List.filter (fun l -> l <> "")
      in
      Alcotest.(check (list string))
        "all lines preserved when file is shorter than lines"
        [ "alpha"; "beta"; "gamma" ]
        lines

let test_tail_of_file_missing () =
  let absent =
    Filename.concat (Sys.getcwd ()) "definitely-not-a-real-log.txt"
  in
  Alcotest.(check (option string))
    "missing file returns None" None
    (A.tail_of_file ~path:absent ())

let suite =
  ( "audit",
    [
      Alcotest.test_case "event codec round-trip (Layer)" `Quick
        test_event_codec_layer;
      Alcotest.test_case "event codec round-trip (Solve_key)" `Quick
        test_event_codec_solve_key;
      Alcotest.test_case "event codec preserves log pointer" `Quick
        test_event_codec_with_log;
      Alcotest.test_case "ULID format" `Quick test_ulid_format;
      Alcotest.test_case "ULID time prefix is monotonic" `Quick
        test_ulid_time_prefix_orders;
      Alcotest.test_case "tail_of_file truncates 500-line file" `Quick
        test_tail_of_file_truncates;
      Alcotest.test_case "tail_of_file preserves short file" `Quick
        test_tail_of_file_short_file;
      Alcotest.test_case "tail_of_file returns None on missing file" `Quick
        test_tail_of_file_missing;
    ] )
