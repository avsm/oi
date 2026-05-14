(* Build manifest — one JSON object per [oi build] / [oi install] / [oi run]
   invocation. Replaces [audit.jsonl] entirely.

   Stored at [<cache>/registry/<os_key>/builds/<YYYY>/<MM>/<date>-<invocation_id>.json]
   locally and uploaded under the same key on S3. Aggregates one
   invocation's worth of audit events with the universe pin
   (reporepo commit + overlay commits + full resolved package set)
   so a downstream Clickhouse / UI can render a complete picture
   without consulting any other artifact. *)

let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.manifest_build"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* -- Types -------------------------------------------------------------- *)

type reporepo = {
  url : string option;
  commit : string option;
  snapshot_key : string option;
}

type overlay_pin = {
  handle : string;
  version : string;
  commit : string option;
  url : string option;
}

type target = {
  kind : string;  (** "Plain" | "Overlay_pkg" | "Overlay_all" | "Group" *)
  handle : string option;
  spec : string option;
  tokens : string list;
}

type solve = {
  solve_key : string option;
  schema : string option;
  from_cache : bool;
  resolved : Manifest_layer.dep list;
}

type summary = {
  ok : int;
  fail : int;
  timeout : int;
  skipped : int;
  cached : int;
  exit : int;
}

type t = {
  schema : int;
  kind : string;
  invocation_id : string;
  os_key : string;
  date : string;
  finished : string option;
  duration_s : float;
  reporepo : reporepo option;
  overlays : overlay_pin list;
  context : Audit.context;
  targets : target list;
  solve : solve option;
  events : Audit.event list;
  summary : summary;
  crashed : bool;
}

let empty_summary =
  { ok = 0; fail = 0; timeout = 0; skipped = 0; cached = 0; exit = 0 }

(* -- Helpers ------------------------------------------------------------ *)

let rfc3339_of_unix ts =
  let tm = Unix.gmtime ts in
  Fmt.str "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let compact_basic_of_unix ts =
  let tm = Unix.gmtime ts in
  Fmt.str "%04d%02d%02dT%02d%02d%02dZ" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let summary_of_events (es : Audit.event list) =
  List.fold_left
    (fun acc (e : Audit.event) ->
      match Outcome.kind_of e.outcome with
      | K_ok | K_restored -> { acc with ok = acc.ok + 1 }
      | K_cached -> { acc with cached = acc.cached + 1 }
      | K_build_failed | K_install_failed | K_dep_failed | K_fetch_failed
      | K_depext_missing | K_solve_failed ->
          { acc with fail = acc.fail + 1 }
      | K_skipped -> { acc with skipped = acc.skipped + 1 })
    empty_summary es

(* -- Codecs ------------------------------------------------------------- *)

let reporepo_codec =
  let open Jsont in
  Object.map ~kind:"reporepo" (fun url commit snapshot_key ->
      { url; commit; snapshot_key })
  |> Object.opt_mem "url" string ~enc:(fun (r : reporepo) -> r.url)
  |> Object.opt_mem "commit" string ~enc:(fun (r : reporepo) -> r.commit)
  |> Object.opt_mem "snapshot_key" string ~enc:(fun (r : reporepo) ->
      r.snapshot_key)
  |> Object.finish

let overlay_pin_codec =
  let open Jsont in
  Object.map ~kind:"overlay_pin" (fun handle version commit url ->
      { handle; version; commit; url })
  |> Object.mem "handle" string ~enc:(fun (o : overlay_pin) -> o.handle)
  |> Object.mem "version" string ~enc:(fun (o : overlay_pin) -> o.version)
  |> Object.opt_mem "commit" string ~enc:(fun (o : overlay_pin) -> o.commit)
  |> Object.opt_mem "url" string ~enc:(fun (o : overlay_pin) -> o.url)
  |> Object.finish

let target_codec =
  let open Jsont in
  Object.map ~kind:"target" (fun kind handle spec tokens ->
      { kind; handle; spec; tokens })
  |> Object.mem "kind" string ~enc:(fun (t : target) -> t.kind)
  |> Object.opt_mem "handle" string ~enc:(fun (t : target) -> t.handle)
  |> Object.opt_mem "spec" string ~enc:(fun (t : target) -> t.spec)
  |> Object.mem "tokens" (list string) ~dec_absent:[]
       ~enc:(fun (t : target) -> t.tokens)
       ~enc_omit:(( = ) [])
  |> Object.finish

let dep_codec = Manifest_layer.dep_codec

let solve_codec =
  let open Jsont in
  Object.map ~kind:"solve" (fun solve_key schema from_cache resolved ->
      { solve_key; schema; from_cache; resolved })
  |> Object.opt_mem "solve_key" string ~enc:(fun (s : solve) -> s.solve_key)
  |> Object.opt_mem "schema" string ~enc:(fun (s : solve) -> s.schema)
  |> Object.mem "from_cache" bool ~dec_absent:false ~enc:(fun (s : solve) ->
      s.from_cache)
  |> Object.mem "resolved" (list dep_codec) ~dec_absent:[]
       ~enc:(fun (s : solve) -> s.resolved)
       ~enc_omit:(( = ) [])
  |> Object.finish

let summary_codec =
  let open Jsont in
  Object.map ~kind:"summary" (fun ok fail timeout skipped cached exit ->
      { ok; fail; timeout; skipped; cached; exit })
  |> Object.mem "ok" int ~dec_absent:0 ~enc:(fun (s : summary) -> s.ok)
  |> Object.mem "fail" int ~dec_absent:0 ~enc:(fun (s : summary) -> s.fail)
  |> Object.mem "timeout" int ~dec_absent:0 ~enc:(fun (s : summary) ->
      s.timeout)
  |> Object.mem "skipped" int ~dec_absent:0 ~enc:(fun (s : summary) ->
      s.skipped)
  |> Object.mem "cached" int ~dec_absent:0 ~enc:(fun (s : summary) -> s.cached)
  |> Object.mem "exit" int ~dec_absent:0 ~enc:(fun (s : summary) -> s.exit)
  |> Object.finish

let codec : t Jsont.t =
  let open Jsont in
  Object.map ~kind:"oi.build.v1"
    (fun
      schema
      kind
      invocation_id
      os_key
      date
      finished
      duration_s
      reporepo
      overlays
      context
      targets
      solve
      events
      summary
      crashed
    ->
      {
        schema;
        kind;
        invocation_id;
        os_key;
        date;
        finished;
        duration_s;
        reporepo;
        overlays;
        context;
        targets;
        solve;
        events;
        summary;
        crashed;
      })
  |> Object.mem "schema" int ~enc:(fun r -> r.schema)
  |> Object.mem "kind" string ~enc:(fun r -> r.kind)
  |> Object.mem "invocation_id" string ~enc:(fun r -> r.invocation_id)
  |> Object.mem "os_key" string ~enc:(fun r -> r.os_key)
  |> Object.mem "date" string ~enc:(fun r -> r.date)
  |> Object.opt_mem "finished" string ~enc:(fun r -> r.finished)
  |> Object.mem "duration_s" number ~enc:(fun r -> r.duration_s)
  |> Object.opt_mem "reporepo" reporepo_codec ~enc:(fun r -> r.reporepo)
  |> Object.mem "overlays" (list overlay_pin_codec) ~dec_absent:[]
       ~enc:(fun r -> r.overlays)
       ~enc_omit:(( = ) [])
  |> Object.mem "context" Audit.context_codec ~enc:(fun r -> r.context)
  |> Object.mem "targets" (list target_codec) ~dec_absent:[]
       ~enc:(fun r -> r.targets)
       ~enc_omit:(( = ) [])
  |> Object.opt_mem "solve" solve_codec ~enc:(fun r -> r.solve)
  |> Object.mem "events" (list Audit.event_codec) ~dec_absent:[]
       ~enc:(fun r -> r.events)
       ~enc_omit:(( = ) [])
  |> Object.mem "summary" summary_codec ~enc:(fun r -> r.summary)
  |> Object.mem "crashed" bool ~dec_absent:false ~enc:(fun r -> r.crashed)
  |> Object.finish

(* -- Storage ------------------------------------------------------------ *)

let builds_dir ~cache_root ~os_key ts =
  let tm = Unix.gmtime ts in
  let yyyy = Fmt.str "%04d" (tm.tm_year + 1900) in
  let mm = Fmt.str "%02d" (tm.tm_mon + 1) in
  cache_root / "layers" / os_key / "builds" / yyyy / mm

let filename_for ~ts ~invocation_id =
  compact_basic_of_unix ts ^ "-" ^ invocation_id ^ ".json"

let path_for ~cache_root ~os_key ~ts ~invocation_id =
  builds_dir ~cache_root ~os_key ts / filename_for ~ts ~invocation_id

let write ~fs ~cache_root m =
  let ts =
    match m.events with [] -> Unix.gettimeofday () | first :: _ -> first.ts
  in
  let dir = builds_dir ~cache_root ~os_key:m.os_key ts in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dir);
  let dst =
    path_for ~cache_root ~os_key:m.os_key ~ts ~invocation_id:m.invocation_id
  in
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent codec m with
  | Error e -> Log.warn (fun mlog -> mlog "build manifest encode %s: %s" dst e)
  | Ok s -> (
      let tmp = dst ^ ".tmp" in
      try
        Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / tmp) s;
        Sys.rename tmp dst
      with exn ->
        Log.warn (fun mlog ->
            mlog "build manifest write %s: %s" dst (Printexc.to_string exn)))

let try_read ~path : t option =
  if not (Sys.file_exists path) then None
  else
    try
      let s = In_channel.with_open_text path In_channel.input_all in
      match Jsont_bytesrw.decode_string ~locs:true ~file:path codec s with
      | Stdlib.Ok r -> Some r
      | Error e ->
          Log.debug (fun m -> m "build manifest bad %s: %s" path e);
          None
    with exn ->
      Log.debug (fun m ->
          m "build manifest read %s: %s" path (Printexc.to_string exn));
      None

(* Walks <root>/*/*/*.json and returns every build manifest under that
   tree. [root] should be a [builds/] directory — typically
   [<cache>/registry/<os_key>/builds] (the local writer's target) or
   [<output_dir>/<os_key>/builds] (a registry-export consumer). *)
let read_all_at ~root : t list =
  if not (Sys.file_exists root) then []
  else
    let collect dir =
      try Sys.readdir dir |> Array.to_list with Sys_error _ -> []
    in
    let read_one dir entry =
      let path = dir / entry in
      if Filename.check_suffix entry ".json" then try_read ~path else None
    in
    collect root
    |> List.concat_map (fun y ->
        collect (root / y)
        |> List.concat_map (fun m ->
            collect (root / y / m) |> List.filter_map (read_one (root / y / m))))

(* -- Construction ------------------------------------------------------- *)

let v ~invocation_id ~os_key ~started_at ?finished_at ?reporepo ?(overlays = [])
    ~context ?(targets = []) ?solve ~(events : Audit.event list)
    ?(crashed = false) ?(exit_code = 0) () =
  let summary =
    let base = summary_of_events events in
    { base with exit = exit_code }
  in
  let duration_s =
    match finished_at with Some f -> f -. started_at | None -> 0.0
  in
  {
    schema = 1;
    kind = "oi.build.v1";
    invocation_id;
    os_key;
    date = rfc3339_of_unix started_at;
    finished = Option.map rfc3339_of_unix finished_at;
    duration_s;
    reporepo;
    overlays;
    context;
    targets;
    solve;
    events;
    summary;
    crashed;
  }
