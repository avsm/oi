[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

let log_src = Logs.Src.create "oi.retry"

module Log = (val Logs.src_log log_src : Logs.LOG)

let append_to_log path line =
  try
    let oc =
      open_out_gen [ Open_append; Open_creat; Open_wronly ] 0o644 path
    in
    output_string oc line;
    output_char oc '\n';
    close_out oc
  with Sys_error _ -> ()

let with_attempts ~label ?(max_attempts = 3) ?(initial_delay_s = 1.0)
    ?error_log_path f =
  let note_failure attempt delay exn =
    let line =
      Fmt.str "%s failed (attempt %d/%d): %s — retrying in %.1fs" label
        attempt max_attempts (Printexc.to_string exn) delay
    in
    match error_log_path with
    | Some path -> append_to_log path line
    | None -> Log.warn (fun m -> m "%s" line)
  in
  let note_success attempt =
    let line =
      Fmt.str "%s succeeded on attempt %d/%d" label attempt max_attempts
    in
    match error_log_path with
    | Some path -> append_to_log path line
    | None -> Log.app (fun m -> m "%s" line)
  in
  let rec loop attempt delay =
    match f () with
    | result ->
        if attempt > 1 then note_success attempt;
        result
    | exception exn when attempt < max_attempts ->
        note_failure attempt delay exn;
        Unix.sleepf delay;
        loop (attempt + 1) (delay *. 2.0)
  in
  loop 1 initial_delay_s
