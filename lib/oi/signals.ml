[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Signal handling: SIGINT → Eio switch cancellation, second SIGINT
    force-exits. *)

exception Interrupted

(* Process-global state. The signal handler runs in the OCaml runtime's
   context (not necessarily any fiber) and broadcasts the condition; a
   daemon fiber installed in the root switch waits on the condition and
   performs the actual cancel. This avoids calling scheduler primitives
   from the signal handler itself. *)

let cond = Eio.Condition.create ()

(* Async-signal-safe counter via [Atomic]. First signal → broadcast +
   graceful cancel; second signal → [exit 130], bypassing cleanup. *)
let hits = Atomic.make 0

(* Once-flag so repeated calls to [install] don't stack up daemons. *)
let installed = Atomic.make false

let handler _signo =
  let n = Atomic.fetch_and_add hits 1 + 1 in
  if n >= 2 then begin
    prerr_string "\n[oi] force-exit on second Ctrl-C\n";
    Stdlib.exit 130
  end;
  Eio.Condition.broadcast cond

let install ~sw =
  if Atomic.exchange installed true then ()
  else begin
    (* Take SIGINT away from opam's [Sys.catch_break true] — otherwise
       SIGINT raises [Sys.Break] inside [OpamProcess.Job.run] and our
       cancel path never sees it. *)
    (try Sys.catch_break false with _ -> ());
    (try Sys.set_signal Sys.sigpipe Sys.Signal_ignore
     with Invalid_argument _ -> ());
    Sys.set_signal Sys.sigint (Sys.Signal_handle handler);
    (try Sys.set_signal Sys.sigterm (Sys.Signal_handle handler)
     with Invalid_argument _ -> ());
    (* Daemon fiber that waits for the signal and propagates cancel.
       Lives as long as [sw] lives; disappears naturally when [sw]
       finishes (daemons don't block switch release). *)
    Eio.Fiber.fork_daemon ~sw (fun () ->
        Eio.Condition.await_no_mutex cond;
        prerr_string "\n[oi] interrupted; cleaning up (Ctrl-C again to force)\n";
        (try flush stderr with _ -> ());
        Eio.Switch.fail sw Interrupted;
        `Stop_daemon)
  end
