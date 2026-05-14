let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.audit"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* -- Types --------------------------------------------------------------- *)

type context = {
  overlay : D10.Overlay.t option;
  toolchain : string option;
  trigger : string;
  project : string option;
  host : string;
}

type log_pointer = { text_path : string; tail : string option }
type event_target = Layer of string | Solve_key of string

let hash_of_target = function Layer h | Solve_key h -> h

type event = {
  schema : int;
  event_id : string;
  invocation_id : string;
  ts : float;
  os_key : string;
  target : event_target;
  pkg : Identity.t;
  outcome : Outcome.t;
  duration_s : float;
  context : context;
  log : log_pointer option;
}

let n_tail_lines = 150

(* -- ULID --------------------------------------------------------------- *)

let crockford = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

(* Use [Stdlib.Random.State] seeded from the OS clock so we don't depend on
   global-state initialisation order. ULID's spec asks for crypto-strength
   randomness in the 80-bit suffix; we settle for clock+pid mixing, which is
   plenty for dedup purposes. *)
let rand_state =
  lazy
    (let s = Random.State.make_self_init () in
     (* Mix in pid so two oi processes started in the same wall-clock
        millisecond don't collide on the random suffix. *)
     for _ = 1 to Unix.getpid () mod 64 do
       ignore (Random.State.bits s)
     done;
     s)

let ulid () =
  let st = Lazy.force rand_state in
  let b = Bytes.create 26 in
  let ts_ms = Int64.of_float (Unix.gettimeofday () *. 1000.) in
  let t = ref ts_ms in
  for i = 9 downto 0 do
    Bytes.set b i crockford.[Int64.to_int (Int64.logand !t 31L)];
    t := Int64.shift_right_logical !t 5
  done;
  for i = 10 to 25 do
    Bytes.set b i crockford.[Random.State.int st 32]
  done;
  Bytes.unsafe_to_string b

let invocation_id_cell = lazy (ulid ())
let invocation_id () = Lazy.force invocation_id_cell

let default_context () =
  {
    overlay = None;
    toolchain = None;
    trigger = String.concat " " (Array.to_list Sys.argv);
    project = None;
    host = (try Unix.gethostname () with Unix.Unix_error _ -> "");
  }

(* -- Tail extraction ------------------------------------------------------ *)

let tail_of_file ?(lines = n_tail_lines) ~path () =
  if not (Sys.file_exists path) then None
  else
    try
      In_channel.with_open_text path (fun ic ->
          let len = in_channel_length ic in
          let buf_size = min len 64_000 in
          seek_in ic (len - buf_size);
          let s = really_input_string ic buf_size in
          let split = String.split_on_char '\n' s in
          let tail =
            match split with
            | [] | [ _ ] -> split
            | _ :: rest when String.length s = buf_size && len > buf_size ->
                rest
            | _ -> split
          in
          let n = List.length tail in
          let drop = max 0 (n - lines) in
          let kept = tail |> List.to_seq |> Seq.drop drop |> List.of_seq in
          Some (String.concat "\n" kept))
    with Sys_error _ -> None

(* -- Codec --------------------------------------------------------------- *)

let context_codec =
  let open Jsont in
  Object.map ~kind:"context" (fun overlay toolchain trigger project host ->
      { overlay; toolchain; trigger; project; host })
  |> Object.opt_mem "overlay" D10.Overlay.codec ~enc:(fun c -> c.overlay)
  |> Object.opt_mem "toolchain" string ~enc:(fun c -> c.toolchain)
  |> Object.mem "trigger" string ~enc:(fun c -> c.trigger)
  |> Object.opt_mem "project" string ~enc:(fun c -> c.project)
  |> Object.mem "host" string ~enc:(fun c -> c.host)
  |> Object.finish

let log_pointer_codec =
  let open Jsont in
  Object.map ~kind:"log" (fun text_path tail : log_pointer ->
      { text_path; tail })
  |> Object.mem "text_path" string ~enc:(fun l -> l.text_path)
  |> Object.opt_mem "tail" string ~enc:(fun l -> l.tail)
  |> Object.finish

(* [event_target] is encoded as a tagged object so a future variant (e.g.
   a synthetic registry export key) doesn't force a schema break:
     {"kind": "layer", "hash": "..."}
     {"kind": "solve_key", "hash": "..."} *)
let event_target_codec : event_target Jsont.t =
  let open Jsont in
  let case_layer =
    Object.Case.map "layer"
      (Object.map ~kind:"layer" (fun hash -> Layer hash)
      |> Object.mem "hash" string ~enc:(function
        | Layer h -> h
        | Solve_key h -> h)
      |> Object.finish)
      ~dec:Fun.id
  in
  let case_solve =
    Object.Case.map "solve_key"
      (Object.map ~kind:"solve_key" (fun hash -> Solve_key hash)
      |> Object.mem "hash" string ~enc:(function
        | Solve_key h -> h
        | Layer h -> h)
      |> Object.finish)
      ~dec:Fun.id
  in
  let cases = [ Object.Case.make case_layer; Object.Case.make case_solve ] in
  Object.map ~kind:"event_target" Fun.id
  |> Object.case_mem "kind" string cases ~enc:Fun.id ~enc_case:(function
    | Layer _ as t -> Object.Case.value case_layer t
    | Solve_key _ as t -> Object.Case.value case_solve t)
  |> Object.finish

let event_codec =
  let open Jsont in
  Object.map ~kind:"audit_event"
    (fun
      schema
      event_id
      invocation_id
      ts
      os_key
      target
      pkg
      outcome
      duration_s
      context
      log
    ->
      {
        schema;
        event_id;
        invocation_id;
        ts;
        os_key;
        target;
        pkg;
        outcome;
        duration_s;
        context;
        log;
      })
  |> Object.mem "schema" int ~enc:(fun e -> e.schema)
  |> Object.mem "event_id" string ~enc:(fun e -> e.event_id)
  |> Object.mem "invocation_id" string ~enc:(fun e -> e.invocation_id)
  |> Object.mem "ts" number ~enc:(fun e -> e.ts)
  |> Object.mem "os_key" string ~enc:(fun e -> e.os_key)
  |> Object.mem "target" event_target_codec ~enc:(fun e -> e.target)
  |> Object.mem "pkg" Identity.codec ~enc:(fun e -> e.pkg)
  |> Object.mem "outcome" Outcome.codec ~enc:(fun e -> e.outcome)
  |> Object.mem "duration_s" number ~enc:(fun e -> e.duration_s)
  |> Object.mem "context" context_codec ~enc:(fun e -> e.context)
  |> Object.opt_mem "log" log_pointer_codec ~enc:(fun e -> e.log)
  |> Object.finish

(* -- Storage -------------------------------------------------------------

   Per-invocation staging: each [oi …] process appends single-line JSON
   events to [<cache>/registry-staging/<invocation_id>.events.jsonl].
   On clean exit, {!finalize} rolls the staging file into a build
   manifest under [<cache>/registry/<os_key>/builds/<YYYY>/<MM>/]; on
   crash, the next invocation's {!reap_orphans} finalises any orphaned
   staging files as [crashed=true]. JSONL is the on-disk transient
   format because POSIX [O_APPEND] gives atomic concurrent appends for
   record sizes under [PIPE_BUF] (~4 KiB); the final S3-bound artifact
   is a single pretty-printed JSON. *)

let staging_dir ~cache_root = cache_root / "registry-staging"

let staging_path ~cache_root ~invocation_id =
  staging_dir ~cache_root / (invocation_id ^ ".events.jsonl")

let ensure_staging_dir ~fs ~cache_root =
  try
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
      Eio.Path.(fs / staging_dir ~cache_root)
  with Eio.Exn.Io _ -> ()

let collect_dir dir =
  try Sys.readdir dir |> Array.to_list with Sys_error _ -> []

let mtime_of path =
  try Some (Unix.stat path).Unix.st_mtime with Unix.Unix_error _ -> None

let try_remove path = try Sys.remove path with Sys_error _ -> ()
let one_hour_s = 3600.0

let reap_one ~dir ~current ~now name =
  if not (Filename.check_suffix name ".events.jsonl") then ()
  else
    let inv = Filename.chop_suffix name ".events.jsonl" in
    if inv = current then ()
    else
      let path = dir / name in
      match mtime_of path with
      | Some mtime when now -. mtime > one_hour_s -> try_remove path
      | _ -> ()

let reap_orphan_staging_files ~cache_root ~current =
  let dir = staging_dir ~cache_root in
  if not (Sys.file_exists dir) then ()
  else
    let now = Unix.gettimeofday () in
    List.iter (reap_one ~dir ~current ~now) (collect_dir dir)

let reaper_done = ref false

let maybe_reap ~cache_root =
  if not !reaper_done then begin
    reaper_done := true;
    try reap_orphan_staging_files ~cache_root ~current:(invocation_id ())
    with exn ->
      Log.debug (fun m -> m "audit reap: %s" (Printexc.to_string exn))
  end

let append ~fs ~cache_root e =
  maybe_reap ~cache_root;
  let dst = staging_path ~cache_root ~invocation_id:e.invocation_id in
  match Jsont_bytesrw.encode_string event_codec e with
  | Ok line -> (
      ensure_staging_dir ~fs ~cache_root;
      let body = line ^ "\n" in
      try
        Eio.Path.save ~append:true ~create:(`If_missing 0o644)
          Eio.Path.(fs / dst)
          body
      with exn ->
        Log.warn (fun m -> m "audit append %s: %s" dst (Printexc.to_string exn))
      )
  | Error msg -> Log.warn (fun m -> m "audit encode: %s" msg)

(* -- Read --------------------------------------------------------------

   Only the per-invocation staging file is read locally — it's consumed
   by [Build_pipeline.finalize_build_manifest] when a run wraps up and
   then unlinked. Persistent history lives in the per-month build
   manifests under [<cache>/layers/<os_key>/builds/<YYYY>/<MM>/...json],
   read by [Manifest_build.read_all_at] (and queried at the analytics
   layer via the S3+Clickhouse pipeline). *)

let split_lines s =
  String.split_on_char '\n' s |> List.filter (fun l -> l <> "")

let decode_event ~file line =
  match Jsont_bytesrw.decode_string ~locs:false ~file event_codec line with
  | Ok e -> Some e
  | Error msg ->
      Log.debug (fun m -> m "audit bad line: %s" msg);
      None

(* Read all events written during this invocation (i.e. that still live
   in the staging file). Used by {!finalize}. *)
let read_staged ~cache_root ~invocation_id : event list =
  let p = staging_path ~cache_root ~invocation_id in
  if not (Sys.file_exists p) then []
  else
    try
      let content = In_channel.with_open_text p In_channel.input_all in
      split_lines content |> List.filter_map (decode_event ~file:p)
    with exn ->
      Log.debug (fun m ->
          m "audit staged read %s: %s" p (Printexc.to_string exn));
      []

let delete_staged ~cache_root ~invocation_id =
  try Sys.remove (staging_path ~cache_root ~invocation_id)
  with Sys_error _ -> ()

(* List orphan staging files (i.e. invocations from prior runs that
   didn't get finalised cleanly). Caller passes them to [finalize] with
   [crashed=true]. *)
let staged_invocation_ids ~cache_root =
  let dir = staging_dir ~cache_root in
  if not (Sys.file_exists dir) then []
  else
    collect_dir dir
    |> List.filter_map (fun name ->
        if Filename.check_suffix name ".events.jsonl" then
          Some (Filename.chop_suffix name ".events.jsonl")
        else None)
