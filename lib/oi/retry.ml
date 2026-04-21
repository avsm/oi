[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

let log_src = Logs.Src.create "oi.retry"

module Log = (val Logs.src_log log_src : Logs.LOG)

let with_attempts ~label ?(max_attempts = 3) ?(initial_delay_s = 1.0) f =
  let rec loop attempt delay =
    match f () with
    | result ->
        if attempt > 1 then
          Log.app (fun m ->
              m "%s succeeded on attempt %d/%d" label attempt max_attempts);
        result
    | exception exn when attempt < max_attempts ->
        Log.warn (fun m ->
            m "%s failed (attempt %d/%d): %s — retrying in %.1fs" label attempt
              max_attempts (Printexc.to_string exn) delay);
        Unix.sleepf delay;
        loop (attempt + 1) (delay *. 2.0)
  in
  loop 1 initial_delay_s
