(** Progress events emitted by [lib/oi] subsystems.

    The library never opens or drives a progress bar itself. Functions that have
    something to report take an optional [reporter] and emit typed [event]s; the
    caller (the [oi] cmdliner layer) constructs whatever UI it wants on top.

    The same reporter type covers every phase of an [oi build] / [oi run]:
    solve, registry fetch, source fetch, archive bake, layer build, prefix
    assembly. The cmdliner layer translates these into a single multi-line bar;
    non-TTY callers can use [null] to silence everything without changing call
    shapes. *)

(** Major phases of a [Build_pipeline.build] / [Archive_builder] run. The bar UI
    typically renders at most one of these as the "current phase" header at any
    time. *)
type phase =
  | Solving  (** [Solver.solve] running. *)
  | Fetching  (** Registry layer prefetch + per-source fetch. *)
  | Baking  (** [Archive_builder] inline-bake of a missing source archive. *)
  | Building  (** [D10ir.Direct.run] executing the layer plan. *)
  | Assembling  (** Prefix assembly post-build. *)

val string_of_phase : phase -> string
(** [string_of_phase p] is a lowercase kebab-case label for [p], e.g.
    ["solving"], ["building"]. *)

(** The kind of asset being fetched. Affects how the UI labels per-row progress
    (binary layer vs. raw source archive). *)
type fetch_kind = Layer | Source

(** Library-emitted progress events. *)
type event =
  | Phase_started of { phase : phase; label : string }
      (** A new phase has begun. [label] is human-readable, often identifies the
          unit being processed (overlay, package). *)
  | Phase_done of phase
      (** A phase ended. The UI may collapse / reset its per-phase rows here. *)
  | Status of string
      (** Free-form one-line status text; the UI may render in the aggregate
          row's message column or ignore. *)
  | Aggregate of { phase : phase; total : int; current : int }
      (** Cumulative count for a phase. Sent on each meaningful advance so the
          UI can drive a bar / fraction. *)
  | Fetch_started of {
      kind : fetch_kind;
      key : string;  (** opaque identifier for this fetch (sha, url, …). *)
      pkg : string;  (** package label for the row, may be empty. *)
      size : int64;  (** known total size in bytes, [0L] if unknown. *)
    }
  | Fetch_progress of {
      kind : fetch_kind;
      key : string;
      bytes : int64;  (** absolute bytes transferred so far. *)
      total : int64;
          (** Declared total bytes if known (e.g. HTTP Content-Length) else
              [0L]. Lets the UI refine the per-fetch denominator when
              [Fetch_started] couldn't supply a size. *)
    }
  | Fetch_finished of { kind : fetch_kind; key : string }
  | Solve_started of { label : string }
      (** A new solve group is starting. [label] is the group's human-readable
          target list (e.g. ["@avsm/atp-identity"]). The cmdliner layer renders
          this as a per-task row beneath the agg row, similar to how
          [Fetch_started] / [Node_started] rows are rendered. *)
  | Solve_finished of { label : string }
      (** The named solve group has completed (successfully or failed). Drops
          the corresponding per-task row. *)
  | Plan_ready of D10ir.Plan.t
      (** Fired once the recipe is built, before [D10ir.Direct.run] starts
          firing per-node events. The UI uses this to materialise its
          per-package rows / dep-tree rendering. *)
  | Total_estimate of { fetches : int; builds : int; fetch_bytes : int64 }
      (** Fired by the cmdliner layer once it knows the final total number of
          fetch and build tasks across all solve groups ([oi build @overlay] /
          [oi build --all]). Locks the agg bar's denominator so it stays static
          through the run rather than dynamically incrementing as new groups are
          discovered. After this event, per-task completion events
          ([Fetch_finished], [Node_built / Cached / Failed / Skipped]) increment
          the done count.

          [fetch_bytes] is the upfront sum of binary-layer sizes across all
          groups (looked up in the registry index during the pre-pass). [0L]
          when no registry is configured or no source-method layers will fetch.
          The bar's byte denominator pins to this value so it doesn't grow as
          each group's fetch phase emits its own [Fetch_started] events. *)
  | Build of D10ir.Direct.event
      (** Per-layer build events propagated from [D10ir.Direct]. The UI
          typically renders these as a per-package row stack. *)
  | Build_summary of D10ir.Direct.result
      (** Final result of the [Building] phase. Lets the UI swap to an ok/failed
          status without polling. *)

type reporter = { event : event -> unit }

val null : reporter
(** No-op reporter: drops every event. Default for callers that don't want a UI.
*)
