(** Coordinate the {!Logs} reporter with active {!Progress.Display} bars so log
    output doesn't tear the bar.

    {!Progress} draws bars by writing to stderr and rewriting the same lines on
    each redraw. {!Logs} also writes to stderr (via the reporter installed in
    {!Cmd.Terms}). With both running, log lines land mid-bar and corrupt the
    rendering — the bar's redraw then leaves stale fragments above it.

    The fix used here: the bar registers its display with this module while
    alive, and the Logs reporter is wrapped to [Display.pause] /
    [Display.resume] around every emission. The display's last-rendered block is
    cleared, the log line prints cleanly, then the bar redraws.

    Registration is single-slot — there's no use case in oi today for two
    simultaneous {!Progress.Display.t} instances. (The fetch bar and d10ir bar
    share one display via [shared_display].) *)

val set_active : (unit, unit) Progress.Display.t -> unit
(** [set_active d] registers [d] as the currently-active display. Subsequent log
    emissions will pause [d] before writing. *)

val clear_active : unit -> unit
(** [clear_active ()] unregisters whatever display was set. Call from the bar's
    cleanup path. Idempotent. *)

val interject : (unit -> 'a) -> 'a
(** [interject f] runs [f] with the active display paused. If no display is
    registered, just calls [f ()]. *)

val wrap_reporter : Logs.reporter -> Logs.reporter
(** [wrap_reporter r] returns a reporter that calls [r]'s emission inside
    {!interject} so log lines land cleanly even when a bar is active. *)

val start_display_compact :
  ppf:Format.formatter ->
  config:Progress.Config.t ->
  (unit, unit) Progress.Display.t
(** [start_display_compact ~ppf ~config] is
    [Progress.Display.start ~config Progress.Multi.blank].

    [ppf] is accepted for symmetry with the {!Progress.Config.v} the caller
    built around the same formatter, but currently isn't used — the bar simply
    paints below the user's command line, leaving the prompt+command intact when
    the bar is finalised. *)
