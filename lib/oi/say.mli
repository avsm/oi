(** Consistent narration primitives for CLI commands.

    Every command's "I'm doing X" output should go through one of these helpers
    so colour, indentation, and the [▸] / [✓] / [⚠] / [✗] markers stay
    consistent across the codebase. Plain [Fmt.pr] calls in command bodies tend
    to drift in style (different prefixes, ad-hoc colours, inconsistent
    capitalization); routing through [Say.*] keeps them uniform.

    Output goes to stdout for {!step}, {!info}, {!field}, {!header}, {!ok}, and
    to stderr for {!warn} and {!error}. None of these helpers interact with the
    progress bar — callers inside an {!Ui.run} body should use {!Ui.log} or
    {!Ui.suspend} instead. *)

val step : ('a, Format.formatter, unit, unit) format4 -> 'a
(** [step fmt] prints ["▸ <msg>"] in accent style — used for top-level action
    steps ("Solving", "Assembling prefix", "Wrote .envrc"). *)

val info : ('a, Format.formatter, unit, unit) format4 -> 'a
(** [info fmt] prints ["  <msg>"] in default style — secondary lines indented
    under a {!step}. *)

val field : string -> ('a, Format.formatter, unit, unit) format4 -> 'a
(** [field label fmt] prints ["  <label>: <value>"] with the label dim and a
    fixed alignment column for the value. *)

val field_list : ?sep:string -> string -> string list -> unit
(** [field_list ?sep "deps" items] prints a field whose value is a list of
    items, joined with [sep] (default [", "]) and word-wrapped to the terminal
    width. Continuation lines are indented to line up with the value column.
    No-op when [items = []]. *)

val progress : string -> unit
(** [progress msg] writes [msg] in dim style on the current line, replacing
    whatever was there (via the ANSI CR + clear-to-eol escape) without emitting
    a newline. Used for in-place high-frequency status updates (e.g. "Fetching
    layers (12/47, 34MB)" during a registry pull). The next non-progress write —
    typically a {!step} or {!progress_clear} — replaces or overwrites the line.
    No-op on non-TTY. *)

val progress_clear : unit -> unit
(** [progress_clear ()] erases the current in-place {!progress} line without
    emitting a newline. Use before a final {!step} so the summary lands cleanly
    on the same row. No-op on non-TTY. *)

val header : ('a, Format.formatter, unit, unit) format4 -> 'a
(** [header fmt] prints the message in bold — used for section headers in
    multi-block reports ([oi show], [oi config]). *)

val ok : ('a, Format.formatter, unit, unit) format4 -> 'a
(** [ok fmt] prints ["  ✓ <msg>"] in success-green. *)

val warn : ('a, Format.formatter, unit, unit) format4 -> 'a
(** [warn fmt] prints ["warning: <msg>"] in yellow on stderr, then flushes —
    protects against the message being eaten by a subsequent progress bar
    redraw. *)

val error : ('a, Format.formatter, unit, unit) format4 -> 'a
(** [error fmt] prints ["error: <msg>"] in red on stderr, then flushes. *)

val newline : unit -> unit
(** [newline ()] emits a blank line on stdout — used to vertically separate
    logical sections. *)

val set_around_emit : ((unit -> unit) -> unit) -> unit
(** [set_around_emit hook] installs a hook that wraps every Say emission. Used
    by the cmdliner layer to pause the active progress bar(s) while Say writes
    (so log lines don't tear the bar) and resume after. Default is identity
    ([fun f -> f ()]); calling it twice replaces the previous hook. *)
