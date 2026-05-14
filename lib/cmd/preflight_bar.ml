[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(* Overall-progress UI used by [oi run]. See preflight_bar.mli. *)

let row_label_width = 40
let row_bar_width = 24

module Theme = struct
  let camel = Progress.Color.hex "#EF7D00"
  let dim_camel = Progress.Color.hex "#5C4632"

  let bar_style =
    let open Progress.Line.Bar_style in
    utf8 |> with_color camel |> with_empty_color dim_camel

  let bar ~width total =
    Progress.Line.bar ~style:(`Custom bar_style) ~width total

  let spinner () = Progress.Line.spinner ~color:camel ()

  (* [(/)] is shadowed at the top of [pipeline.ml] / [execute.ml] as
     [Filename.concat] so any percent math there would have to reach
     for [Stdlib.(/)]; defining this helper here avoids that. *)
  let pct_pp ~(total : int) : int Progress.Printer.t =
    let render (n : int) : string =
      let pct : int =
        if total <= 0 then 0
        else
          let p = n * 100 / total in
          if p < 0 then 0 else if p > 100 then 100 else p
      in
      let body = Fmt.str "(%d%%)" pct in
      let pad = max 0 (6 - String.length body) in
      String.make pad ' ' ^ body
    in
    Progress.Printer.create ~string_len:6 ~to_string:render ()
end

module Preflight = struct
  (* See [Theme.bar] above for the camel-orange filled / dim-camel
     empty palette. [Progress.Line.string] intentionally writes
     [width - 1] cells (see Progress's source), so [rpad N string]
     only renders [N - 1] visible chars. We bump the [rpad] target
     by one to land the overall row on the same column as a per-row
     [pkg fit_str 32] prefix. *)
  let phase_col_width = row_label_width - 2 + 1

  let overall_line ~total =
    let open Progress.Line in
    let spin = Theme.spinner () in
    let phase_seg = using (fun (_, s) -> s) (rpad phase_col_width string) in
    let bar_seg =
      using (fun (i, _) -> i) (Theme.bar ~width:(`Fixed row_bar_width) total)
    in
    let count_seg = using (fun (i, _) -> i) (count_to total) in
    let pct_seg =
      using (fun (i, _) -> i) (sum ~pp:(Theme.pct_pp ~total) ~width:6 ())
    in
    list ~sep:(const " ") [ spin; phase_seg; bar_seg; count_seg; pct_seg ]

  let with_bar_noop ~stopped f =
    let on_phase _ = () in
    let on_text _ = () in
    let preflight_done () = stopped := true in
    Fun.protect
      ~finally:(fun () -> stopped := true)
      (fun () -> f ~on_phase ~on_text ~preflight_done ~shared_display:None)

  let make_display () =
    let cfg =
      Progress.Config.v ~ppf:Format.err_formatter ~persistent:false ()
    in
    (* Open with [Multi.blank] (no built-in reporters) and attach the
       overall bar via [add_line]. Keeping the [Display.t] typed as
       [(unit, unit) Display.t] regardless of what's added later lets
       subsystems receive the same display via
       [?shared_display:(unit, unit) Display.t] without OCaml's
       inference unifying the parameter to whatever the
       OWNED-display path uses. *)
    let display : (unit, unit) Progress.Display.t =
      Logs_progress.start_display_compact ~ppf:Format.err_formatter ~config:cfg
    in
    Logs_progress.set_active display;
    display

  let finalise_quietly display =
    try Progress.Display.finalise display with Sys_error _ | Failure _ -> ()

  let stop_display ~stopped display =
    if not !stopped then begin
      stopped := true;
      Logs_progress.clear_active ();
      finalise_quietly display
    end

  let tick_quietly display =
    try Progress.Display.tick display with Sys_error _ | Failure _ -> ()

  let rec tick_loop ~clock ~stopped display =
    Eio.Time.sleep clock 0.1;
    if !stopped then `Stop_daemon
    else begin
      tick_quietly display;
      tick_loop ~clock ~stopped display
    end

  let spawn_tick_daemon ~sw ~stopped ~clock display =
    Eio.Fiber.fork_daemon ~sw (fun () -> tick_loop ~clock ~stopped display)

  let make_push ~stopped overall_h msg =
   fun ~advance ->
    if not !stopped then
      let delta = if advance then 1 else 0 in
      try Progress.Reporter.report overall_h (delta, !msg)
      with Sys_error _ | Failure _ -> ()

  let with_bar_tty ~sw ~clock ~total f =
    let stopped = ref false in
    let display = make_display () in
    let overall_h = Progress.Display.add_line display (overall_line ~total) in
    let msg = ref "Preparing" in
    let stepped = ref false in
    let push = make_push ~stopped overall_h msg in
    push ~advance:false;
    spawn_tick_daemon ~sw ~stopped ~clock display;
    let on_phase m =
      if not !stopped then begin
        msg := m;
        push ~advance:!stepped;
        stepped := true
      end
    in
    let on_text m =
      if not !stopped then begin
        msg := m;
        push ~advance:false
      end
    in
    let preflight_done () = stop_display ~stopped display in
    Fun.protect
      ~finally:(fun () -> stop_display ~stopped display)
      (fun () ->
        f ~on_phase ~on_text ~preflight_done ~shared_display:(Some display))

  let with_bar ~clock ?total_steps f =
    if not (Tty.is_tty ()) then with_bar_noop ~stopped:(ref false) f
    else
      let total = match total_steps with Some n when n > 0 -> n | _ -> 12 in
      Eio.Switch.run @@ fun sw -> with_bar_tty ~sw ~clock ~total f
end
