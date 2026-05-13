(** [oi cache]: inspect the d10 content-addressed layer cache.

    Subsumes the old standalone [d10] CLI: every layer the build pipeline has
    materialised under [<cache>/layers/<os_key>/] is queryable here, plus the
    sqlite index that powers binary lookup. *)

open Cmdliner

let ( / ) = Filename.concat
let short_hash h = if String.length h > 12 then String.sub h 0 12 else h

(* Build a [D10.Config.t] from the shared harness env. The d10 view is a
   read-only handle to [<cache>/layers/<os_key>/]; nothing writes through
   it from these inspection commands except [oi cache index]. *)
let with_d10 (env : Harness.env) f =
  let d10 : D10.Config.t =
    {
      sys = env.sys;
      fs = env.fs;
      clock = (env.clock :> D10.Config.clk);
      root = Eio.Path.(env.fs / Oi.Cache.root_s env.cache);
      os_key = env.os_key;
    }
  in
  f d10

let layers_root_s d10 =
  Eio.Path.native_exn d10.D10.Config.root / "layers" / d10.os_key

let index_path_s d10 =
  Eio.Path.native_exn d10.D10.Config.root / "layers" / "index.db"

(* -- helpers for status rendering --------------------------------------- *)

let status_span = function
  | 0 -> Tty.Span.styled Oi.Style.ok "ok"
  | n -> Fmt.kstr (Tty.Span.styled Oi.Style.error) "fail %d" n

let pp_time t =
  let t = Unix.gmtime t in
  Fmt.str "%04d-%02d-%02d %02d:%02d UTC" (t.Unix.tm_year + 1900) (t.tm_mon + 1)
    t.tm_mday t.tm_hour t.tm_min

let count_entry ~scan acc dir name =
  let path = dir / name in
  if Sys.is_directory path then scan acc path else acc + 1

let count_files dir =
  if not (Sys.file_exists dir) then 0
  else
    let rec scan acc dir =
      Array.fold_left (fun a name -> count_entry ~scan a dir name) acc
        (Sys.readdir dir)
    in
    scan 0 dir

(* -- list ---------------------------------------------------------------- *)

type layer_summary = {
  layer_hash : string;
  package : string option;
  exit_status : int option;
}

let layer_summary_codec =
  let open Jsont in
  Object.map ~kind:"layer_summary" (fun layer_hash package exit_status ->
      { layer_hash; package; exit_status })
  |> Object.mem "layer_hash" string ~enc:(fun l -> l.layer_hash)
  |> Object.opt_mem "package" string ~enc:(fun l -> l.package)
  |> Object.opt_mem "exit_status" int ~enc:(fun l -> l.exit_status)
  |> Object.finish

let list_envelope_codec =
  let open Jsont in
  Object.map ~kind:"oi_cache_list" (fun _schema_version os_key layers ->
      (os_key, layers))
  |> Object.mem "schema_version" string ~enc:(fun _ ->
      Oi.Stamp.json_schema_version)
  |> Object.mem "os_key" string ~enc:(fun (k, _) -> k)
  |> Object.mem "layers" (list layer_summary_codec) ~enc:(fun (_, ls) -> ls)
  |> Object.finish

let read_layer_summaries ~layers_dir ~root ~os_key =
  if not (Sys.file_exists layers_dir) then []
  else
    Sys.readdir layers_dir |> Array.to_list |> List.sort String.compare
    |> List.map (fun hash ->
        let json = Eio.Path.(root / "layers" / os_key / hash / "layer.json") in
        match D10.Layer.load_meta json with
        | Some m ->
            {
              layer_hash = hash;
              package = Some m.package;
              exit_status = Some m.exit_status;
            }
        | None -> { layer_hash = hash; package = None; exit_status = None })

let list_summary_row l =
  match (l.package, l.exit_status) with
  | Some pkg, Some st ->
      [
        Tty.Span.styled Oi.Style.dim (short_hash l.layer_hash);
        status_span st;
        Tty.Span.text pkg;
      ]
  | _ ->
      [
        Tty.Span.styled Oi.Style.dim (short_hash l.layer_hash);
        Tty.Span.styled Oi.Style.warn "no metadata";
        Tty.Span.text "—";
      ]

let print_list_text ~os_key summaries =
  Fmt.pr "%a %s@.@." Oi.Style.pp_header_string "Layers" os_key;
  if summaries = [] then Fmt.pr "  (empty)@."
  else begin
    let total = List.length summaries in
    let rows = List.map list_summary_row summaries in
    let table =
      Tty.Table.of_rows ~header_style:Oi.Style.header
        [
          Tty.Table.column "HASH";
          Tty.Table.column "STATUS";
          Tty.Table.column "PACKAGE";
        ]
        rows
    in
    Oi.Style.pp_table Fmt.stdout table;
    Fmt.pr "@.%a %d layer(s)@." Oi.Style.pp_header_string "Total:" total
  end

let print_list_json ~os_key summaries =
  match
    Jsont_bytesrw.encode_string ~format:Jsont.Indent list_envelope_codec
      (os_key, summaries)
  with
  | Ok s ->
      print_string s;
      print_newline ()
  | Error e -> Oi.Error.fail_config_error "json encode failed: %s" e

let run_list (c : Terms.common) =
  Harness.run @@ fun ~sw env ->
  let h =
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  with_d10 h @@ fun d10 ->
  let layers_dir = layers_root_s d10 in
  let summaries =
    read_layer_summaries ~layers_dir ~root:d10.root ~os_key:d10.os_key
  in
  match c.format with
  | Json -> print_list_json ~os_key:d10.os_key summaries
  | Text -> print_list_text ~os_key:d10.os_key summaries

let list_cmd =
  let info = Cmd.info "list" ~doc:"List cached layers" in
  Cmd.v info Term.(const run_list $ Terms.common)

(* -- show ---------------------------------------------------------------- *)

(* Render the [Provenance.t] sidecar (when present) below the [layer.json]
   block. Provenance fields cover the inputs that produced the layer's
   content hash — opam origin, source URL+kind, declared depexts, ocaml
   version. Caller context (overlay handle, trigger, etc.) lives in
   {!print_callers} below, sourced from the audit log. *)
let pp_duration s =
  if s < 1.0 then Fmt.str "%.0fms" (s *. 1000.)
  else if s < 60.0 then Fmt.str "%.2fs" s
  else
    let m = int_of_float (s /. 60.0) in
    let r = s -. float_of_int (m * 60) in
    Fmt.str "%dm%.0fs" m r

let print_provenance (p : Oi.Provenance.t) =
  let o = p.opam.origin in
  let origin_label =
    match o.overlay with
    | Some ov -> Fmt.str "%a %a" Oi.Origin.pp_kind o.kind D10.Overlay.pp ov
    | None -> Fmt.str "%a" Oi.Origin.pp_kind o.kind
  in
  Fmt.pr "  method:   %s@," (Oi.Identity.string_of_method p.method_);
  Fmt.pr "  built:    %s (%s)@," (pp_time p.built_at) (pp_duration p.duration_s);
  Fmt.pr "  opam sha: %s@," p.opam.sha256;
  Fmt.pr "  origin:   %s@," origin_label;
  if o.path_in_repo <> "" then Fmt.pr "  path:     %s@," o.path_in_repo;
  (match p.source with
  | None -> ()
  | Some s -> (
      let kind = if s.kind = "" then "" else Fmt.str " (%s)" s.kind in
      Fmt.pr "  source:   %s%s@," s.url kind;
      match s.checksums with [] -> () | c :: _ -> Fmt.pr "  checksum: %s@," c));
  Fmt.pr "  ocaml:    %s@," p.build_env.ocaml_version;
  if p.depexts_declared <> [] then
    Fmt.pr "  depexts:  %s@," (String.concat ", " p.depexts_declared);
  let phases = p.phases in
  let phase_strs =
    List.filter_map
      (fun (name, v) ->
        Stdlib.Option.map (fun s -> Fmt.str "%s=%s" name (pp_duration s)) v)
      [
        ("fetch", phases.fetch);
        ("build", phases.build);
        ("install", phases.install);
        ("restore", phases.restore);
      ]
  in
  if phase_strs <> [] then
    Fmt.pr "  phases:   %s@," (String.concat ", " phase_strs)

(* Group [events] by overlay handle and render one row per distinct handle
   listing its outcome histogram and the most recent timestamp:

       @avsm     ok×1 cached×2  (last 2026-05-02 09:14 UTC)
       @samoht   restored×1     (last 2026-05-02 09:30 UTC)

   Events are pre-filtered to a single layer_hash by the caller. *)
let print_callers events =
  if events = [] then ()
  else begin
    let by_handle :
        (string, (Oi.Outcome.kind * int) list ref * float ref) Hashtbl.t =
      Hashtbl.create 4
    in
    List.iter
      (fun (e : Oi.Audit.event) ->
        let key =
          match e.context.overlay with
          | Some o -> "@" ^ o.handle
          | None -> "(no overlay)"
        in
        let outcomes_ref, ts_ref =
          match Hashtbl.find_opt by_handle key with
          | Some v -> v
          | None ->
              let v = (ref [], ref e.ts) in
              Hashtbl.replace by_handle key v;
              v
        in
        outcomes_ref :=
          Oi.Outcome.bump (Oi.Outcome.kind_of e.outcome) !outcomes_ref;
        ts_ref := Float.max !ts_ref e.ts)
      events;
    let rows =
      Hashtbl.fold
        (fun k (os_ref, ts_ref) acc -> (k, !os_ref, !ts_ref) :: acc)
        by_handle []
      |> List.sort (fun (a, _, _) (b, _, _) -> compare a b)
    in
    Fmt.pr "  callers:@,";
    List.iter
      (fun (handle, outcomes, last_ts) ->
        let outcome_str =
          Oi.Outcome.sort_histogram outcomes
          |> List.map (fun (k, c) ->
              Fmt.str "%s×%d" (Oi.Outcome.string_of_kind k) c)
          |> String.concat " "
        in
        Fmt.pr "    %-16s %s  (last %s)@," handle outcome_str (pp_time last_ts))
      rows
  end

type show_match = {
  layer_hash : string;
  meta : D10.Layer.meta;
  files_count : int;
  provenance : Oi.Provenance.t option;
  callers : Oi.Audit.event list;
}

let show_match_codec =
  let open Jsont in
  Object.map ~kind:"cache_show_layer"
    (fun layer_hash meta files_count provenance callers ->
      { layer_hash; meta; files_count; provenance; callers })
  |> Object.mem "layer_hash" string ~enc:(fun m -> m.layer_hash)
  |> Object.mem "meta" D10.Layer.meta_codec ~enc:(fun m -> m.meta)
  |> Object.mem "files_count" int ~enc:(fun m -> m.files_count)
  |> Object.opt_mem "provenance" Oi.Provenance.codec ~enc:(fun m ->
      m.provenance)
  |> Object.mem "callers"
       (list Oi.Audit.event_codec)
       ~dec_absent:[]
       ~enc:(fun m -> m.callers)
       ~enc_omit:(( = ) [])
  |> Object.finish

let show_envelope_codec =
  let open Jsont in
  Object.map ~kind:"oi_cache_show"
    (fun _schema_version os_key package_pattern matches ->
      (os_key, package_pattern, matches))
  |> Object.mem "schema_version" string ~enc:(fun _ ->
      Oi.Stamp.json_schema_version)
  |> Object.mem "os_key" string ~enc:(fun (k, _, _) -> k)
  |> Object.mem "package_pattern" string ~enc:(fun (_, p, _) -> p)
  |> Object.mem "matches" (list show_match_codec) ~enc:(fun (_, _, m) -> m)
  |> Object.finish

let record_event_for_layer tbl (e : Oi.Audit.event) =
  match e.target with
  | Solve_key _ -> ()
  | Layer hash ->
      let prev =
        match Hashtbl.find_opt tbl hash with Some xs -> xs | None -> []
      in
      Hashtbl.replace tbl hash (e :: prev)

(* Build a [hash -> Audit events] map from the local audit log so that
   [oi cache show] can attribute each layer to its callers. *)
let build_events_by_hash ~fs ~cache_root ~os_key ~layers_dir =
  let tbl = Hashtbl.create 256 in
  if not (Sys.file_exists layers_dir) then tbl
  else begin
    let all = Oi.Audit.read_all ~fs ~cache_root ~os_key in
    List.iter (record_event_for_layer tbl) all;
    tbl
  end

let matches_for_package ~fs ~os_key ~cache_root ~layers_dir ~root ~events_for
    package =
  if not (Sys.file_exists layers_dir) then []
  else
    Sys.readdir layers_dir |> Array.to_list
    |> List.filter_map (fun hash ->
        let json = Eio.Path.(root / "layers" / os_key / hash / "layer.json") in
        match D10.Layer.load_meta json with
        | Some m
          when String.length m.package >= String.length package
               && String.sub m.package 0 (String.length package) = package ->
            let files_count = count_files (layers_dir / hash / "fs") in
            let provenance =
              Oi.Provenance.read_one ~fs ~cache_root ~os_key ~hash
            in
            Some
              {
                layer_hash = hash;
                meta = m;
                files_count;
                provenance;
                callers = events_for hash;
              }
        | _ -> None)

let print_show_match (m : show_match) =
  let status =
    if m.meta.exit_status = 0 then Fmt.str "%a" Oi.Style.pp_ok_string "ok"
    else
      Fmt.str "%a (exit %d)" Oi.Style.pp_error_string "failed"
        m.meta.exit_status
  in
  Fmt.pr "@[<v>%a %s@," Oi.Style.pp_header_string "Layer" m.layer_hash;
  Fmt.pr "  package:  %s@," m.meta.package;
  Fmt.pr "  status:   %s@," status;
  Fmt.pr "  created:  %s@," (pp_time m.meta.created);
  Fmt.pr "  deps:     %s@,"
    (if m.meta.deps = [] then "(none)" else String.concat ", " m.meta.deps);
  Fmt.pr "  hashes:   %s@,"
    (if m.meta.hashes = [] then "(none)"
     else String.concat ", " (List.map short_hash m.meta.hashes));
  Fmt.pr "  files:    %d@," m.files_count;
  (match m.provenance with None -> () | Some p -> print_provenance p);
  print_callers m.callers;
  Fmt.pr "@,@]@."

let print_show_text ~package matches =
  if matches = [] then Fmt.pr "No layers found matching %S@." package
  else List.iter print_show_match matches

let print_show_json ~os_key ~package matches =
  match
    Jsont_bytesrw.encode_string ~format:Jsont.Indent show_envelope_codec
      (os_key, package, matches)
  with
  | Ok s ->
      print_string s;
      print_newline ()
  | Error e -> Oi.Error.fail_config_error "json encode failed: %s" e

let run_show (c : Terms.common) package =
  Harness.run @@ fun ~sw env ->
  let h =
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  with_d10 h @@ fun d10 ->
  let layers_dir = layers_root_s d10 in
  let cache_root = Eio.Path.native_exn d10.root in
  let events_by_hash =
    build_events_by_hash ~fs:h.fs ~cache_root ~os_key:d10.os_key ~layers_dir
  in
  let events_for hash =
    match Hashtbl.find_opt events_by_hash hash with
    | Some xs -> xs
    | None -> []
  in
  let matches =
    matches_for_package ~fs:h.fs ~os_key:d10.os_key ~cache_root ~layers_dir
      ~root:d10.root ~events_for package
  in
  match c.format with
  | Json -> print_show_json ~os_key:d10.os_key ~package matches
  | Text -> print_show_text ~package matches

let show_cmd =
  let package =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"PACKAGE" ~doc:"Package name; matched as a prefix." [])
  in
  let info = Cmd.info "show" ~doc:"Show details for a package's layers" in
  Cmd.v info Term.(const run_show $ Terms.common $ package)

(* -- binaries ------------------------------------------------------------ *)

type binary_entry = { binary : string; pkg_name : string; pkg_version : string }

let binary_entry_codec =
  let open Jsont in
  Object.map ~kind:"binary" (fun binary pkg_name pkg_version ->
      { binary; pkg_name; pkg_version })
  |> Object.mem "binary" string ~enc:(fun e -> e.binary)
  |> Object.mem "package" string ~enc:(fun e -> e.pkg_name)
  |> Object.mem "version" string ~enc:(fun e -> e.pkg_version)
  |> Object.finish

let binaries_envelope_codec =
  let open Jsont in
  Object.map ~kind:"oi_cache_binaries"
    (fun _schema_version os_key index_present binaries ->
      (os_key, index_present, binaries))
  |> Object.mem "schema_version" string ~enc:(fun _ ->
      Oi.Stamp.json_schema_version)
  |> Object.mem "os_key" string ~enc:(fun (k, _, _) -> k)
  |> Object.mem "index_present" bool ~enc:(fun (_, p, _) -> p)
  |> Object.mem "binaries" (list binary_entry_codec) ~enc:(fun (_, _, b) -> b)
  |> Object.finish

let read_indexed_binaries d10 ~index_path =
  if not (Sys.file_exists index_path) then []
  else
    let db = D10.Index.open_ ~fs:d10.D10.Config.fs ~path:index_path in
    let raw = D10.Index.all_binaries db ~os_key:d10.D10.Config.os_key in
    D10.Index.close db;
    List.map
      (fun (binary, pkg_name, pkg_version) ->
        { binary; pkg_name; pkg_version })
      raw

let print_binaries_text ~os_key ~index_present bins =
  if not index_present then
    Fmt.pr "No index found. Run %a first.@." Oi.Style.pp_accent_string
      "oi cache index"
  else begin
    Fmt.pr "%a %s@.@." Oi.Style.pp_header_string "Binaries" os_key;
    let rows =
      List.map
        (fun b ->
          [
            Tty.Span.text b.binary;
            Tty.Span.text b.pkg_name;
            Tty.Span.styled Oi.Style.dim b.pkg_version;
          ])
        bins
    in
    let table =
      Tty.Table.of_rows ~header_style:Oi.Style.header
        [
          Tty.Table.column "BINARY";
          Tty.Table.column "PACKAGE";
          Tty.Table.column "VERSION";
        ]
        rows
    in
    Oi.Style.pp_table Fmt.stdout table;
    Fmt.pr "@.%a %d binar%s@." Oi.Style.pp_header_string "Total:"
      (List.length bins)
      (if List.length bins = 1 then "y" else "ies")
  end

let print_binaries_json ~os_key ~index_present bins =
  match
    Jsont_bytesrw.encode_string ~format:Jsont.Indent binaries_envelope_codec
      (os_key, index_present, bins)
  with
  | Ok s ->
      print_string s;
      print_newline ()
  | Error e -> Oi.Error.fail_config_error "json encode failed: %s" e

let run_binaries (c : Terms.common) =
  Harness.run @@ fun ~sw env ->
  let h =
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  with_d10 h @@ fun d10 ->
  let index_path = index_path_s d10 in
  let index_present = Sys.file_exists index_path in
  let bins = read_indexed_binaries d10 ~index_path in
  match c.format with
  | Json -> print_binaries_json ~os_key:d10.os_key ~index_present bins
  | Text -> print_binaries_text ~os_key:d10.os_key ~index_present bins

let binaries_cmd =
  let info =
    Cmd.info "binaries" ~doc:"List binaries indexed in the layer cache"
  in
  Cmd.v info Term.(const run_binaries $ Terms.common)

(* -- index --------------------------------------------------------------- *)

let is_skipped_layers_entry ~layers_root entry =
  let dir = layers_root / entry in
  (not (Sys.is_directory dir))
  || entry = "." || entry = ".."
  || Filename.check_suffix entry ".db"
  || Filename.check_suffix entry ".db-shm"
  || Filename.check_suffix entry ".db-wal"

let add_stats (t : D10.Index.stats) (s : D10.Index.stats) : D10.Index.stats =
  {
    layers = t.layers + s.layers;
    binaries = t.binaries + s.binaries;
    files = t.files + s.files;
    findlib = t.findlib + s.findlib;
    tarballs = t.tarballs + s.tarballs;
  }

let index_row_of_stats ~include_files entry (s : D10.Index.stats) =
  let base =
    [
      Tty.Span.text entry;
      Tty.Span.text (string_of_int s.layers);
      Tty.Span.text (string_of_int s.binaries);
      Tty.Span.text (string_of_int s.findlib);
      Tty.Span.text (string_of_int s.tarballs);
    ]
  in
  if include_files then base @ [ Tty.Span.text (string_of_int s.files) ]
  else base

let index_os_key_entry ~h ~d10 ~include_files ~db ~layers_root ~totals ~rows
    entry =
  if is_skipped_layers_entry ~layers_root entry then ()
  else begin
    let c : D10.Config.t = { d10 with os_key = entry } in
    let cache_root = Eio.Path.native_exn d10.D10.Config.root in
    let overlay_for ~hash =
      Oi.Provenance.overlay_of_layer ~fs:h.Harness.fs ~cache_root ~os_key:entry
        ~hash
    in
    D10.Index.rebuild c ~overlay_for ~include_files db;
    let s = D10.Index.stats db ~os_key:entry in
    totals := add_stats !totals s;
    rows := index_row_of_stats ~include_files entry s :: !rows
  end

let print_index_table ~include_files rows =
  let cols =
    let base =
      [
        Tty.Table.column "OS_KEY";
        Tty.Table.column ~align:`Right "LAYERS";
        Tty.Table.column ~align:`Right "BINARIES";
        Tty.Table.column ~align:`Right "FINDLIB";
        Tty.Table.column ~align:`Right "TARBALLS";
      ]
    in
    if include_files then base @ [ Tty.Table.column ~align:`Right "FILES" ]
    else base
  in
  let table =
    Tty.Table.of_rows ~header_style:Oi.Style.header cols (List.rev rows)
  in
  Oi.Style.pp_table Fmt.stdout table

let print_index_trailer ~include_files (t : D10.Index.stats) ~index_path =
  let trailer =
    if include_files then
      Fmt.str "%d layers, %d binaries, %d findlib, %d tarballs, %d files"
        t.layers t.binaries t.findlib t.tarballs t.files
    else
      Fmt.str "%d layers, %d binaries, %d findlib, %d tarballs" t.layers
        t.binaries t.findlib t.tarballs
  in
  Fmt.pr "@.%a %s@." Oi.Style.pp_header_string "Total:" trailer;
  Fmt.pr "Index: %s@." index_path

let run_index (c : Terms.common) include_files =
  Harness.run @@ fun ~sw env ->
  let h =
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  with_d10 h @@ fun d10 ->
  let layers_root = Eio.Path.native_exn d10.root / "layers" in
  let index_path = layers_root / "index.db" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(d10.fs / layers_root);
  let db = D10.Index.open_ ~fs:d10.fs ~path:index_path in
  let totals =
    ref
      D10.Index.
        { layers = 0; binaries = 0; files = 0; findlib = 0; tarballs = 0 }
  in
  let rows = ref [] in
  if Sys.file_exists layers_root then
    Array.iter
      (index_os_key_entry ~h ~d10 ~include_files ~db ~layers_root ~totals ~rows)
      (Sys.readdir layers_root);
  D10.Index.close db;
  if !rows = [] then Fmt.pr "No layers indexed.@."
  else print_index_table ~include_files !rows;
  print_index_trailer ~include_files !totals ~index_path

let index_cmd =
  let include_files =
    Arg.(
      value & flag
      & info ~doc:"Index every file path. Bloats $(b,index.db); off by default."
          [ "include-files" ])
  in
  let info = Cmd.info "index" ~doc:"Rebuild the layer index" in
  Cmd.v info Term.(const run_index $ Terms.common $ include_files)

(* -- stats --------------------------------------------------------------- *)

type stats_view = {
  os_key : string;
  index_present : bool;
  s : D10.Index.stats;
  archives : int;
}

let stats_envelope_codec =
  let open Jsont in
  Object.map ~kind:"oi_cache_stats"
    (fun
      _schema_version
      os_key
      index_present
      n_layers
      n_bin
      n_findlib
      n_tar
      n_files
      n_archives
    ->
      let s : D10.Index.stats =
        {
          layers = n_layers;
          binaries = n_bin;
          files = n_files;
          findlib = n_findlib;
          tarballs = n_tar;
        }
      in
      { os_key; index_present; s; archives = n_archives })
  |> Object.mem "schema_version" string ~enc:(fun _ ->
      Oi.Stamp.json_schema_version)
  |> Object.mem "os_key" string ~enc:(fun v -> v.os_key)
  |> Object.mem "index_present" bool ~enc:(fun v -> v.index_present)
  |> Object.mem "layers" int ~enc:(fun v -> v.s.layers)
  |> Object.mem "binaries" int ~enc:(fun v -> v.s.binaries)
  |> Object.mem "findlib" int ~enc:(fun v -> v.s.findlib)
  |> Object.mem "tarballs" int ~enc:(fun v -> v.s.tarballs)
  |> Object.mem "files" int ~enc:(fun v -> v.s.files)
  |> Object.mem "archives" int ~enc:(fun v -> v.archives)
  |> Object.finish

(* Count [<sha>.tar.zst] files under [<cache>/d10ir/archives/]. The
   d10ir archive store is content-addressed by source-tree sha; one
   archive may back many layer hashes when a package is built across
   distros / toolchain variants. *)
let count_d10ir_archives ~cache =
  let dir = Oi.D10ir_archives.local_dir ~cache in
  if not (Sys.file_exists dir) then 0
  else
    Array.fold_left
      (fun n entry ->
        if Filename.check_suffix entry ".tar.zst" then n + 1 else n)
      0 (Sys.readdir dir)

let stats_of_index d10 ~index_path =
  if not (Sys.file_exists index_path) then
    D10.Index.
      { layers = 0; binaries = 0; files = 0; findlib = 0; tarballs = 0 }
  else
    let db = D10.Index.open_ ~fs:d10.D10.Config.fs ~path:index_path in
    let s = D10.Index.stats db ~os_key:d10.os_key in
    D10.Index.close db;
    s

let print_stats_text ~os_key ~index_present ~archives (s : D10.Index.stats) =
  if not index_present then
    Fmt.pr "No index found. Run %a first.@." Oi.Style.pp_accent_string
      "oi cache index"
  else begin
    Fmt.pr "@[<v>%a %s@," Oi.Style.pp_header_string "Stats" os_key;
    Fmt.pr "  layers:    %d@," s.layers;
    Fmt.pr "  binaries:  %d@," s.binaries;
    Fmt.pr "  findlib:   %d@," s.findlib;
    Fmt.pr "  tarballs:  %d@," s.tarballs;
    if s.files > 0 then Fmt.pr "  files:     %d@," s.files;
    Fmt.pr "  archives:  %d@,@]@." archives
  end

let print_stats_json view =
  match
    Jsont_bytesrw.encode_string ~format:Jsont.Indent stats_envelope_codec view
  with
  | Ok s ->
      print_string s;
      print_newline ()
  | Error e -> Oi.Error.fail_config_error "json encode failed: %s" e

let run_stats (c : Terms.common) =
  Harness.run @@ fun ~sw env ->
  let h =
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  with_d10 h @@ fun d10 ->
  let index_path = index_path_s d10 in
  let index_present = Sys.file_exists index_path in
  let s = stats_of_index d10 ~index_path in
  let archives = count_d10ir_archives ~cache:h.cache in
  let view = { os_key = d10.os_key; index_present; s; archives } in
  match c.format with
  | Json -> print_stats_json view
  | Text ->
      print_stats_text ~os_key:d10.os_key ~index_present ~archives s

let stats_cmd =
  let info = Cmd.info "stats" ~doc:"Summarise the layer cache" in
  Cmd.v info Term.(const run_stats $ Terms.common)

(* -- archives ------------------------------------------------------------ *)

type archive_entry = { sha : string; size : int }

let archive_entry_codec =
  let open Jsont in
  Object.map ~kind:"d10ir_archive" (fun sha size -> { sha; size })
  |> Object.mem "sha" string ~enc:(fun e -> e.sha)
  |> Object.mem "size" int ~enc:(fun e -> e.size)
  |> Object.finish

let archives_envelope_codec =
  let open Jsont in
  Object.map ~kind:"oi_cache_archives" (fun _schema_version dir entries ->
      (dir, entries))
  |> Object.mem "schema_version" string ~enc:(fun _ ->
      Oi.Stamp.json_schema_version)
  |> Object.mem "dir" string ~enc:(fun (d, _) -> d)
  |> Object.mem "archives" (list archive_entry_codec) ~enc:(fun (_, e) -> e)
  |> Object.finish

let pp_size n =
  let f = float_of_int n in
  if n < 1024 then Fmt.str "%dB" n
  else if n < 1024 * 1024 then Fmt.str "%.1fK" (f /. 1024.)
  else if n < 1024 * 1024 * 1024 then Fmt.str "%.1fM" (f /. (1024. *. 1024.))
  else Fmt.str "%.2fG" (f /. (1024. *. 1024. *. 1024.))

let publish_archives_to ~h ~output entries =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(h.Harness.fs / output);
  let { Oi.D10ir_archives.linked; present; missing = _ } =
    Oi.D10ir_archives.publish_all ~cache:h.cache ~output
  in
  let dst = Oi.D10ir_archives.dst_dir ~output in
  Fmt.pr
    "%a %d archive(s) at %s (%d new, %d already present; %d total in \
     cache)@."
    Oi.Style.pp_ok_string "✓" (linked + present) dst linked present
    (List.length entries)

let print_archives_text ~dir entries =
  Fmt.pr "%a %s@.@." Oi.Style.pp_header_string "d10ir archives" dir;
  if entries = [] then Fmt.pr "  (empty)@."
  else begin
    let total = List.fold_left (fun acc e -> acc + e.size) 0 entries in
    let rows =
      List.map
        (fun e ->
          [
            Tty.Span.styled Oi.Style.dim (short_hash e.sha);
            Tty.Span.text (pp_size e.size);
          ])
        entries
    in
    let table =
      Tty.Table.of_rows ~header_style:Oi.Style.header
        [
          Tty.Table.column "SHA";
          Tty.Table.column ~align:`Right "SIZE";
        ]
        rows
    in
    Oi.Style.pp_table Fmt.stdout table;
    Fmt.pr "@.%a %d archive(s), %s total@." Oi.Style.pp_header_string "Total:"
      (List.length entries) (pp_size total)
  end

let print_archives_json ~dir entries =
  match
    Jsont_bytesrw.encode_string ~format:Jsont.Indent archives_envelope_codec
      (dir, entries)
  with
  | Ok s ->
      print_string s;
      print_newline ()
  | Error e -> Oi.Error.fail_config_error "json encode failed: %s" e

let run_archives (c : Terms.common) to_dir =
  Harness.run @@ fun ~sw env ->
  let h =
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  let dir = Oi.D10ir_archives.local_dir ~cache:h.cache in
  let entries =
    Oi.D10ir_archives.list ~cache:h.cache
    |> List.map (fun (sha, size) -> { sha; size })
  in
  match to_dir with
  | Some output -> publish_archives_to ~h ~output entries
  | None -> (
      match c.format with
      | Json -> print_archives_json ~dir entries
      | Text -> print_archives_text ~dir entries)

let archives_cmd =
  let to_dir =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"DIR"
          ~doc:
            "Hardlink every cached archive into $(b,DIR/d10ir-archives/) — the \
             layout $(b,oi build) expects from a remote registry. Idempotent \
             on repeat invocations."
          [ "to"; "export" ])
  in
  let info =
    Cmd.info "archives" ~doc:"List or export baked d10ir source archives"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Without $(b,--to), enumerate every $(b,<sha>.tar.zst) under \
             $(b,\\$OI_CACHE_DIR/d10ir/archives/), the consolidated source \
             archives produced by $(b,oi repo bake) and consumed by $(b,oi \
             build) when an opam carries an $(b,x-d10-archive) marker.";
          `P
            "With $(b,--to=DIR), hardlink every archive into \
             $(b,DIR/d10ir-archives/) — the publishable layout a remote \
             registry serves. Use this when you want just the source-archive \
             tier without the layer cache that $(b,oi build --export) would \
             also publish. Reaches every archive in the cache, including ones \
             not currently referenced by any reporepo overlay (use $(b,oi repo \
             bake --to=DIR) to publish just the reporepo-referenced subset).";
          `S Manpage.s_examples;
          `Pre
            "  oi cache archives\n\
            \  oi cache archives --to=./registry\n\
            \  oi cache archives --export=/var/www/oi.example.com";
        ]
  in
  Cmd.v info Term.(const run_archives $ Terms.common $ to_dir)

(* -- explain ------------------------------------------------------------- *)

(* Tree projection of {!Oi.Provenance.t}. Used as the JSON shape emitted by
   [oi cache explain] — pulls the full transitive dependency closure into
   one document so an agent can ask "why does this layer behave this way?"
   without re-walking [provenance.json] sidecars itself. *)

type explain_node = {
  layer_hash : string;
  provenance : Oi.Provenance.t;
  depends_on : explain_node list;
}

let rec explain_node_codec =
  lazy
    (let open Jsont in
     Object.map ~kind:"layer_explanation"
       (fun layer_hash provenance depends_on ->
         { layer_hash; provenance; depends_on })
     |> Object.mem "layer_hash" string ~enc:(fun n -> n.layer_hash)
     |> Object.mem "provenance" Oi.Provenance.codec ~enc:(fun n -> n.provenance)
     |> Object.mem "depends_on"
          (list (Jsont.rec' explain_node_codec))
          ~dec_absent:[]
          ~enc:(fun n -> n.depends_on)
          ~enc_omit:(( = ) [])
     |> Object.finish)

let explain_envelope_codec =
  let open Jsont in
  Object.map ~kind:"oi_cache_explain" (fun _schema_version root -> root)
  |> Object.mem "schema_version" string ~enc:(fun _ ->
      Oi.Stamp.json_schema_version)
  |> Object.mem "root" (Lazy.force explain_node_codec) ~enc:(fun n -> n)
  |> Object.finish

let read_provenance_or_fail ~fs ~cache_root ~os_key ~hash =
  match Oi.Provenance.read_one ~fs ~cache_root ~os_key ~hash with
  | Some p -> p
  | None ->
      Oi.Error.fail_config_error
        "no provenance.json for layer %s under os %s — layer may be absent, \
         failed, or pre-provenance"
        hash os_key

let build_explain_tree ~fs ~cache_root ~os_key root_hash =
  let memo : (string, explain_node) Hashtbl.t = Hashtbl.create 64 in
  let rec walk hash =
    match Hashtbl.find_opt memo hash with
    | Some n -> n
    | None ->
        let prov = read_provenance_or_fail ~fs ~cache_root ~os_key ~hash in
        let depends_on =
          List.map (fun (d : Oi.Identity.dep) -> walk d.hash) prov.deps
        in
        let n = { layer_hash = hash; provenance = prov; depends_on } in
        Hashtbl.add memo hash n;
        n
  in
  walk root_hash

let rec print_explain_tree ~depth (n : explain_node) =
  let indent = String.make (depth * 2) ' ' in
  let pkg = Oi.Identity.to_string n.provenance.pkg in
  Fmt.pr "%s%s %a@." indent pkg Oi.Style.pp_dim_string (short_hash n.layer_hash);
  List.iter (print_explain_tree ~depth:(depth + 1)) n.depends_on

let print_explain_json tree =
  match
    Jsont_bytesrw.encode_string ~format:Jsont.Indent explain_envelope_codec
      tree
  with
  | Ok s ->
      print_string s;
      print_newline ()
  | Error e -> Oi.Error.fail_config_error "json encode failed: %s" e

let run_explain (c : Terms.common) hash =
  Harness.run @@ fun ~sw env ->
  let h =
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  with_d10 h @@ fun d10 ->
  let cache_root = Eio.Path.native_exn d10.root in
  let tree = build_explain_tree ~fs:h.fs ~cache_root ~os_key:d10.os_key hash in
  match c.format with
  | Json -> print_explain_json tree
  | Text -> print_explain_tree ~depth:0 tree

let explain_cmd =
  let hash =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"HASH"
          ~doc:"Full layer hash, as printed by $(b,oi cache list)." [])
  in
  let info =
    Cmd.info "explain" ~doc:"Trace a layer's provenance and dependency closure"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Walk the dependency closure of $(b,HASH) and print each layer's \
             provenance as a tree of $(i,package layer-hash) lines.";
          `P
            "With $(b,--format=json), emit one document carrying the full \
             provenance per node (opam origin, source URL and checksums, \
             depexts, build_env) and a $(b,depends_on) array of children.";
          `P
            "Failed layers have no provenance; use $(b,oi cache show) for \
             those.";
        ]
  in
  Cmd.v info Term.(const run_explain $ Terms.common $ hash)

(* -- group --------------------------------------------------------------- *)

let cmd =
  let info =
    Cmd.info "cache" ~doc:"Inspect the d10 layer cache"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Read-only views into the content-addressed layer cache that backs \
             $(b,oi build) and $(b,oi run). Layers live under \
             $(b,\\$OI_CACHE_DIR/layers/<os_key>/) with a sqlite index at \
             $(b,\\$OI_CACHE_DIR/layers/index.db); baked source archives live \
             under $(b,\\$OI_CACHE_DIR/d10ir/archives/).";
          `P
            "$(b,oi cache index) rebuilds the index; other subcommands query \
             it or read $(b,layer.json) sidecars. $(b,oi cache archives) \
             surfaces the d10ir source-archive store.";
          `S "SEE ALSO";
          `P "$(b,oi clean)(1), $(b,oi build --export)(1).";
        ]
  in
  Cmd.group info
    [
      list_cmd;
      show_cmd;
      binaries_cmd;
      archives_cmd;
      index_cmd;
      stats_cmd;
      explain_cmd;
    ]
