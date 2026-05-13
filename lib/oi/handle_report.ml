let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.handle_report"

module Log = (val Logs.src_log log_src : Logs.LOG)

type slice = {
  os_key : string;
  manifest : Manifest.t;
  events : Audit.event list;
}

(* -- Reading slices ------------------------------------------------------ *)

let load_text fs path =
  try Some (Eio.Path.load Eio.Path.(fs / path)) with Eio.Exn.Io _ -> None

let split_lines s =
  String.split_on_char '\n' s |> List.filter (fun l -> l <> "")

let load_manifest ~fs ~output_dir ~os_key =
  let path = output_dir / os_key / "logs" / "manifest.json" in
  match load_text fs path with
  | None -> None
  | Some content -> (
      match
        Jsont_bytesrw.decode_string ~locs:false ~file:path Manifest.codec
          content
      with
      | Ok m -> Some m
      | Error msg ->
          Log.debug (fun m -> m "manifest decode %s: %s" path msg);
          None)

let decode_audit_line ~path line =
  match
    Jsont_bytesrw.decode_string ~locs:false ~file:path Audit.event_codec line
  with
  | Ok e -> Some e
  | Error msg ->
      Log.debug (fun m -> m "audit bad line %s: %s" path msg);
      None

let load_audit_events ~fs ~output_dir ~os_key =
  let path = Audit.exported_log_path ~output_dir ~os_key in
  match load_text fs path with
  | None -> []
  | Some content ->
      split_lines content |> List.filter_map (decode_audit_line ~path)

let read_slice ~fs ~output_dir ~os_key =
  match load_manifest ~fs ~output_dir ~os_key with
  | None -> None
  | Some manifest ->
      let events = load_audit_events ~fs ~output_dir ~os_key in
      Some { os_key; manifest; events }

let list_subdirs ~fs path =
  try
    Eio.Path.read_dir Eio.Path.(fs / path)
    |> List.filter (fun name ->
        name <> ""
        && name.[0] <> '.'
        && Eio.Path.is_directory Eio.Path.(fs / path / name))
  with Eio.Exn.Io _ -> []

let read_all_slices ~fs ~output_dir =
  list_subdirs ~fs output_dir
  |> List.filter_map (fun os_key -> read_slice ~fs ~output_dir ~os_key)
  |> List.sort (fun a b -> String.compare a.os_key b.os_key)

(* -- Handle enumeration -------------------------------------------------- *)

module String_set = Set.Make (String)

let handles slices =
  List.fold_left
    (fun acc s ->
      List.fold_left
        (fun acc (e : Audit.event) ->
          match e.context.overlay with
          | Some o -> String_set.add o.handle acc
          | None -> acc)
        acc s.events)
    String_set.empty slices
  |> String_set.elements

(* -- Markdown helpers ---------------------------------------------------- *)

let is_failure_kind = function
  | Outcome.K_ok | K_cached | K_restored | K_skipped -> false
  | K_build_failed | K_install_failed | K_dep_failed | K_fetch_failed
  | K_depext_missing | K_solve_failed ->
      true

let outcome_to_kind_string o = Outcome.string_of_kind (Outcome.kind_of o)

let pp_outcome_detail buf (o : Outcome.t) =
  let p fmt = Fmt.kstr (Buffer.add_string buf) fmt in
  match o with
  | Build_failed { command; exit_code } ->
      p "- Command: `%s`\n" command;
      Stdlib.Option.iter (fun c -> p "- Exit code: `%d`\n" c) exit_code
  | Install_failed { command; exit_code } ->
      p "- Command: `%s`\n" command;
      Stdlib.Option.iter (fun c -> p "- Exit code: `%d`\n" c) exit_code
  | Fetch_failed { url; kind } ->
      p "- URL: `%s`\n" url;
      let k =
        match kind with
        | Http_status n -> Fmt.str "HTTP %d" n
        | Checksum_mismatch -> "checksum mismatch"
        | Network_timeout -> "network timeout"
        | Git_failed -> "git failed"
        | Other s -> s
      in
      p "- Kind: %s\n" k
  | Dep_failed { upstream } ->
      p "- Failing upstream dep: `%s`\n" (Identity.to_string upstream.id);
      p "- Upstream layer: `%s`\n" upstream.hash
  | Depext_missing { missing; not_found } ->
      if missing <> [] then
        p "- System packages missing: %s\n"
          (String.concat ", " (List.map (Fmt.str "`%s`") missing));
      if not_found <> [] then
        p "- System packages with no manager mapping: %s\n"
          (String.concat ", " (List.map (Fmt.str "`%s`") not_found))
  | Solve_failed { reason } -> p "- Reason: %s\n" reason
  | Skipped { reason } -> p "- Reason: %s\n" reason
  | Ok | Cached | Restored -> ()

let format_iso_utc ts =
  let tm = Unix.gmtime ts in
  Fmt.str "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let log_relative_path ~os_key (lp : Audit.log_pointer) =
  (* The audit log_pointer's text_path is the original local cache path, e.g.
     [<cache>/build/logs/build-foo.1.0-abc.log]. The registry only ships the
     filename's logs/ trailing component conceptually, so we emit a hint
     pointing at the conventional "../<os_key>/logs/<basename>" location an
     agent can join with the registry root. The file may or may not be
     published — the embedded tail is the authoritative copy. *)
  let basename = Filename.basename lp.text_path in
  "../" ^ os_key ^ "/logs/" ^ basename

let trim_tail ?(max_lines = 60) text =
  let lines = String.split_on_char '\n' text in
  let n = List.length lines in
  if n <= max_lines then text
  else
    let drop = n - max_lines in
    let rec skip k = function
      | [] -> []
      | _ :: rest when k > 0 -> skip (k - 1) rest
      | xs -> xs
    in
    String.concat "\n" (skip drop lines)

(* -- Filtering & grouping ------------------------------------------------ *)

let event_handle (e : Audit.event) =
  Stdlib.Option.map (fun (o : D10.Overlay.t) -> o.handle) e.context.overlay

let failures_for_handle ~handle slices =
  List.concat_map
    (fun s ->
      List.filter_map
        (fun (e : Audit.event) ->
          if event_handle e <> Some handle then None
          else if not (is_failure_kind (Outcome.kind_of e.outcome)) then None
          else Some (s.os_key, e))
        s.events)
    slices

(* Group failures by Identity.t (preserving the kind histogram). *)
module Pkg_map = Map.Make (struct
  type t = Identity.t

  let compare (a : Identity.t) (b : Identity.t) =
    match String.compare a.Identity.name b.Identity.name with
    | 0 -> String.compare a.Identity.version b.Identity.version
    | n -> n
end)

let group_by_pkg pairs =
  List.fold_left
    (fun acc (os_key, ev) ->
      let pkg = (ev : Audit.event).pkg in
      let prev = try Pkg_map.find pkg acc with Not_found -> [] in
      Pkg_map.add pkg ((os_key, ev) :: prev) acc)
    Pkg_map.empty pairs
  |> Pkg_map.bindings
  |> List.map (fun (pkg, evs) ->
      let evs =
        List.sort (fun (a_os, _) (b_os, _) -> String.compare a_os b_os) evs
      in
      (pkg, evs))

(* Find a per-package source URL across slices, falling back across [os_key]s
   so the agent gets some upstream pointer even if the failing distro's
   manifest doesn't have provenance for that package (typical: build never
   committed). *)
let url_for_entry ~pkg (e : Manifest.entry) =
  if e.pkg <> pkg then None
  else
    match e.source with
    | Some src when src.url <> "" -> Some src.url
    | _ -> None

let source_for_pkg ~pkg slices =
  List.find_map
    (fun s -> List.find_map (url_for_entry ~pkg) s.manifest.results)
    slices

(* -- Markdown rendering -------------------------------------------------- *)

let buf_add buf s = Buffer.add_string buf s
let buf_addf buf fmt = Fmt.kstr (fun s -> Buffer.add_string buf s) fmt

let summarize_kinds events =
  List.fold_left
    (fun acc (_os_key, (e : Audit.event)) ->
      Outcome.bump (Outcome.kind_of e.outcome) acc)
    [] events
  |> Outcome.sort_histogram

let buf_add_header buf ~handle ~generated_at =
  buf_addf buf "# Failure report: @%s\n\n" handle;
  buf_addf buf "_Generated %s — for LLM-agent consumption._\n\n"
    (format_iso_utc generated_at);
  buf_add buf "This file lists every package that failed to build under the `@";
  buf_add buf handle;
  buf_add buf
    "` overlay handle, joined across every distro currently published to this \
     registry. Each section gives the failing outcome, an embedded tail of the \
     build log, and a one-liner you can paste to reproduce locally.\n\n"

let buf_add_summary buf ~n_pkgs ~n_events ~distros ~kinds =
  buf_add buf "## Summary\n\n";
  buf_addf buf "- %d failing package(s), %d failure event(s)\n" n_pkgs n_events;
  buf_addf buf "- Distros with at least one failure: %s\n"
    (String.concat ", " (List.map (fun s -> "`" ^ s ^ "`") distros));
  buf_add buf "- Outcome mix: ";
  (match kinds with
  | [] -> buf_add buf "_(none)_"
  | _ ->
      buf_add buf
        (String.concat ", "
           (List.map
              (fun (k, n) -> Fmt.str "%d %s" n (Outcome.string_of_kind k))
              kinds)));
  buf_add buf "\n\n"

let buf_add_reproduction buf ~handle =
  buf_add buf "## Reproduction\n\n";
  buf_add buf
    "Build every package that participates in this overlay (host distro):\n\n";
  buf_addf buf "```sh\noi build @%s\n```\n\n" handle;
  buf_add buf "Build a single failing package:\n\n";
  buf_add buf "```sh\n";
  buf_addf buf "oi build @%s/<pkg>\n" handle;
  buf_add buf "```\n\n";
  buf_add buf
    "To rebuild on a specific distro, run inside the matching container image \
     (e.g. `docker run --rm -it oi:fedora-43`).\n\n"

let buf_add_log_tail buf (e : Audit.event) =
  match Stdlib.Option.bind e.log (fun lp -> lp.tail) with
  | Some tail when String.trim tail <> "" ->
      buf_add buf "<details><summary>Log tail</summary>\n\n";
      buf_add buf "```\n";
      buf_add buf (trim_tail tail);
      if not (String.length tail > 0 && tail.[String.length tail - 1] = '\n')
      then buf_add buf "\n";
      buf_add buf "```\n\n";
      buf_add buf "</details>\n\n"
  | _ -> ()

let buf_add_event_block buf (os_key, (e : Audit.event)) =
  buf_addf buf "#### %s — %s\n\n" os_key (outcome_to_kind_string e.outcome);
  buf_addf buf "- When: %s\n" (format_iso_utc e.ts);
  (match e.target with
  | Layer h -> buf_addf buf "- Layer hash: `%s`\n" h
  | Solve_key h -> buf_addf buf "- Solve key: `%s`\n" h);
  pp_outcome_detail buf e.outcome;
  (match e.log with
  | Some lp ->
      buf_addf buf "- Log (registry-relative): `%s`\n"
        (log_relative_path ~os_key lp)
  | None -> ());
  buf_add buf "\n";
  buf_add_log_tail buf e

let buf_add_pkg_section buf ~handle ~slices (pkg, evs) =
  let pkg_label = Identity.to_string pkg in
  let kinds_for_pkg =
    List.map (fun (_, e) -> Outcome.kind_of (e : Audit.event).outcome) evs
    |> List.sort_uniq compare
    |> List.map Outcome.string_of_kind
    |> String.concat ", "
  in
  buf_addf buf "### `%s` — %s\n\n" pkg_label kinds_for_pkg;
  buf_addf buf "Reproduce: `oi build @%s/%s`\n\n" handle pkg_label;
  (match source_for_pkg ~pkg slices with
  | Some url -> buf_addf buf "Source: %s\n\n" url
  | None -> ());
  let by_os = List.sort (fun (a, _) (b, _) -> String.compare a b) evs in
  List.iter (buf_add_event_block buf) by_os;
  buf_add buf "---\n\n"

let markdown ~handle ~generated_at slices =
  let failures = failures_for_handle ~handle slices in
  if failures = [] then ""
  else
    let buf = Buffer.create 4096 in
    let pkg_groups = group_by_pkg failures in
    let distros =
      List.fold_left
        (fun acc (os_key, _) -> String_set.add os_key acc)
        String_set.empty failures
      |> String_set.elements
    in
    buf_add_header buf ~handle ~generated_at;
    buf_add_summary buf ~n_pkgs:(List.length pkg_groups)
      ~n_events:(List.length failures) ~distros
      ~kinds:(summarize_kinds failures);
    buf_add_reproduction buf ~handle;
    buf_add buf "## Failures\n\n";
    List.iter (buf_add_pkg_section buf ~handle ~slices) pkg_groups;
    Buffer.contents buf

(* -- write_all ----------------------------------------------------------- *)

let write_all ~fs ~output_dir ~generated_at =
  let slices = read_all_slices ~fs ~output_dir in
  let hs = handles slices in
  let dir = output_dir / "handles" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dir);
  List.filter_map
    (fun handle ->
      let body = markdown ~handle ~generated_at slices in
      if body = "" then None
      else
        let path = dir / (handle ^ ".md") in
        Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / path) body;
        Some handle)
    hs
