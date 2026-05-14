open Cmdliner

(* -- search -------------------------------------------------------------- *)

(* Find [seg] in [name] at or after [start]; [None] if no occurrence. *)
let find_from ~name ~seg start =
  let slen = String.length seg and nlen = String.length name in
  let rec loop i =
    if i + slen > nlen then None
    else if String.sub name i slen = seg then Some i
    else loop (i + 1)
  in
  loop start

(* Match the next non-empty segment of a glob pattern. [anchored_start]
   forces the first segment to start at position 0. *)
let glob_step ~name ~anchored_start ~pos seg =
  match find_from ~name ~seg pos with
  | None -> None
  | Some i when anchored_start && pos = 0 && i <> 0 -> None
  | Some i -> Some (i + String.length seg)

(* Walk every non-empty segment of a [*]-split glob pattern. [pos] is
   the cursor into [name]; the final segment must reach [name]'s tail
   when [anchored_end] is set. *)
let rec glob_walk ~name ~anchored_start ~anchored_end pos = function
  | [] -> true
  | [ last ] ->
      if anchored_end then
        String.ends_with ~suffix:last name
        && String.length name - String.length last >= pos
      else true
  | seg :: rest -> (
      match glob_step ~name ~anchored_start ~pos seg with
      | None -> false
      | Some next -> glob_walk ~name ~anchored_start ~anchored_end next rest)

(* Glob match with [*] wildcard. Used for package-name filtering in
   [oi search]; binary-name filtering goes through SQL's [LIKE] inside
   [D10.Index]. *)
let glob_matches ~pattern name =
  if not (String.contains pattern '*') then pattern = name
  else
    let segs = String.split_on_char '*' pattern in
    let anchored_start = not (String.starts_with ~prefix:"*" pattern) in
    let anchored_end = not (String.ends_with ~suffix:"*" pattern) in
    let non_empty = List.filter (fun s -> s <> "") segs in
    glob_walk ~name ~anchored_start ~anchored_end 0 non_empty
    && ((not anchored_start)
       || List.hd segs = ""
       || String.starts_with ~prefix:(List.hd segs) name)

(* Walk every overlay's [packages/] tree in the reporepo for package
   names matching [pattern]. Returns [(handle, pkg_name, pkg_version)]
   rows, one per [<handle>/packages/<name>/<name.version>/opam] hit. *)
let scan_declared_packages ~reporepo_path ~pattern ~overlay_filter =
  let rows = ref [] in
  Oi.Source.Reporepo.iter_opam_files ~path:reporepo_path
    ~include_handles:overlay_filter (fun ~handle ~pkg ~version ~opam_path:_ ->
      if glob_matches ~pattern pkg then rows := (handle, pkg, version) :: !rows);
  List.rev !rows

(* Rank of a search-result state. Used to collapse redundant rows
   for the same (kind, overlay, name, version): if a package is
   built locally AND declared in the overlay, we keep [Local] and
   drop [Declared]. *)
type state = Local | Remote | Declared

let state_rank = function Local -> 0 | Remote -> 1 | Declared -> 2

let state_label = function
  | Local -> "local"
  | Remote -> "remote"
  | Declared -> "declared"

(* Latest version per key, or every version when [all_versions] is set.
   Versions are compared via [OpamPackage.Version.compare]. *)
let trim_to_latest ~all_versions rows key version =
  if all_versions then rows
  else
    let by_key = Hashtbl.create 32 in
    List.iter
      (fun r ->
        let k = key r in
        let v = version r in
        match Hashtbl.find_opt by_key k with
        | None -> Hashtbl.replace by_key k r
        | Some prev
          when OpamPackage.Version.compare
                 (OpamPackage.Version.of_string v)
                 (OpamPackage.Version.of_string (version prev))
               > 0 ->
            Hashtbl.replace by_key k r
        | Some _ -> ())
      rows;
    Hashtbl.fold (fun _ r acc -> r :: acc) by_key []

(* One row of search output. Same shape for [bin] and [pkg] kinds so the
   caller can print them in a single uniform table. *)
type row = {
  kind : [ `Bin | `Pkg | `Lib ];
  overlay : string; (* "@handle" or "-" *)
  binary : string option; (* filled for [Bin]; [None] for [Pkg] / [Lib] *)
  findlib : string option; (* filled for [Lib]; [None] otherwise *)
  pkg_name : string;
  pkg_version : string;
  state : state;
  hash : string option; (* present for Local / Remote, absent for Declared *)
}

(* Accept [@handle/PATTERN] as shorthand for [--overlay=handle PATTERN],
   and bare [@handle] as "everything in this overlay" (pattern = [*]).
   Combines with any [--overlay] flags the user already passed. *)
let absorb_handle_prefix ~pattern ~overlay_filter =
  match Target.bare_handle pattern with
  | Some h -> ("*", h :: overlay_filter)
  | None -> (
      match Target.split_handle_prefix pattern with
      | None -> (pattern, overlay_filter)
      | Some (h, rest) -> (rest, h :: overlay_filter))

let open_local_index ~sys ~fs ~cache ~os_key ~clk =
  let index_path =
    Layer_index.ensure_local ~sys ~fs ~clock:clk ~cache ~os_key
  in
  D10.Index.open_ ~fs ~path:index_path

(* Fetch the registry's [index-full.json] once per [os_key] (it's
   memoised inside [D10.Remote_index]); [None] when no [--registry]
   was given or the registry doesn't publish one for this os_key. *)
let load_remote_full ~sys ~fs ~clk ~session ~cache ~os_key ~registry =
  if registry = "" then None
  else
    let cfg : D10.Config.t =
      { sys; fs; clock = clk; root = Oi.Cache.root cache; os_key }
    in
    D10.Remote_index.fetch_full cfg ~session ~remote:(`Http_remote registry)

let overlay_of ~overlay_filter = function
  | None -> "-"
  | Some (o : D10.Overlay.t) ->
      if overlay_filter <> [] && not (List.mem o.handle overlay_filter) then "-"
        (* shouldn't happen after later filter, but defensive *)
      else "@" ^ o.handle

let state_of_hash ~d10 hash =
  if D10.Layer.succeeded d10 ~hash then Local else Remote

let bin_rows_of ~db ~d10 ~os_key ~overlay_filter ~pattern =
  D10.Index.search_binary db ~pattern ~os_key
  |> List.map (fun (bin, pkg_name, pkg_ver, hash, overlay) ->
      {
        kind = `Bin;
        overlay = overlay_of ~overlay_filter overlay;
        binary = Some bin;
        findlib = None;
        pkg_name;
        pkg_version = pkg_ver;
        state = state_of_hash ~d10 hash;
        hash = Some hash;
      })

let pkg_built_rows_of ~db ~d10 ~os_key ~overlay_filter ~pattern =
  D10.Index.search_package db ~pattern ~os_key
  |> List.map (fun (pkg_name, pkg_ver, hash, overlay) ->
      {
        kind = `Pkg;
        overlay = overlay_of ~overlay_filter overlay;
        binary = None;
        findlib = None;
        pkg_name;
        pkg_version = pkg_ver;
        state = state_of_hash ~d10 hash;
        hash = Some hash;
      })

let lib_rows_of ~db ~d10 ~os_key ~overlay_filter ~pattern =
  D10.Index.meta_for db ~findlib_pkg:pattern ~os_key
  |> List.map (fun (pkg_name, pkg_ver, hash, overlay) ->
      {
        kind = `Lib;
        overlay = overlay_of ~overlay_filter overlay;
        binary = None;
        findlib = Some pattern;
        pkg_name;
        pkg_version = pkg_ver;
        state = state_of_hash ~d10 hash;
        hash = Some hash;
      })

let pkg_declared_rows_of ~reporepo_path ~pattern ~overlay_filter =
  scan_declared_packages ~reporepo_path ~pattern ~overlay_filter
  |> List.map (fun (handle, name, version) ->
      {
        kind = `Pkg;
        overlay = "@" ^ handle;
        binary = None;
        findlib = None;
        pkg_name = name;
        pkg_version = version;
        state = Declared;
        hash = None;
      })

(* Apply overlay filter everywhere. The [@default] tag sits on the base
   opam-repository clone, so passing [--overlay=default] keeps base-repo
   rows. *)
let filter_by_overlay ~overlay_filter rows =
  match overlay_filter with
  | [] -> rows
  | xs ->
      List.filter
        (fun r ->
          let h =
            if String.length r.overlay > 1 && r.overlay.[0] = '@' then
              String.sub r.overlay 1 (String.length r.overlay - 1)
            else r.overlay
          in
          List.mem h xs)
        rows

(* Collapse redundant rows for the same package: a locally built
   package also appears as a [Declared] row from the overlay scan;
   keep the strongest state ([Local] > [Remote] > [Declared]). *)
let strongest_per_key rows =
  let by_key = Hashtbl.create 32 in
  List.iter
    (fun r ->
      let k = (r.kind, r.overlay, r.pkg_name, r.pkg_version, r.binary) in
      match Hashtbl.find_opt by_key k with
      | None -> Hashtbl.replace by_key k r
      | Some prev when state_rank r.state < state_rank prev.state ->
          Hashtbl.replace by_key k r
      | Some _ -> ())
    rows;
  Hashtbl.fold (fun _ r acc -> r :: acc) by_key []

let kind_rank = function `Bin -> 0 | `Lib -> 1 | `Pkg -> 2

let compare_binary a b =
  match (a, b) with
  | Some x, Some y -> String.compare x y
  | None, Some _ -> 1
  | Some _, None -> -1
  | None, None -> 0

(* Stable ordering for readable output: bins first, then pkgs,
   alphabetic by name, version descending. *)
let compare_rows a b =
  let c = compare (kind_rank a.kind) (kind_rank b.kind) in
  if c <> 0 then c
  else
    let c = String.compare a.pkg_name b.pkg_name in
    if c <> 0 then c
    else
      let c = compare_binary a.binary b.binary in
      if c <> 0 then c
      else
        OpamPackage.Version.compare
          (OpamPackage.Version.of_string b.pkg_version)
          (OpamPackage.Version.of_string a.pkg_version)

let short_hash = function
  | None -> ""
  | Some h -> String.sub h 0 (min 12 (String.length h))

let kind_string = function `Bin -> "bin" | `Lib -> "lib" | `Pkg -> "pkg"

let json_row_codec =
  let open Jsont in
  Object.map ~kind:"search_row"
    (fun kind overlay binary pkg_name pkg_version state hash ->
      (kind, overlay, binary, pkg_name, pkg_version, state, hash))
  |> Object.mem "kind" string ~enc:(fun (k, _, _, _, _, _, _) -> k)
  |> Object.mem "overlay" string ~enc:(fun (_, o, _, _, _, _, _) -> o)
  |> Object.opt_mem "binary" string ~enc:(fun (_, _, b, _, _, _, _) -> b)
  |> Object.mem "package" string ~enc:(fun (_, _, _, p, _, _, _) -> p)
  |> Object.mem "version" string ~enc:(fun (_, _, _, _, v, _, _) -> v)
  |> Object.mem "state" string ~enc:(fun (_, _, _, _, _, s, _) -> s)
  |> Object.opt_mem "hash" string ~enc:(fun (_, _, _, _, _, _, h) -> h)
  |> Object.finish

let json_envelope_codec =
  let open Jsont in
  Object.map ~kind:"oi_search" (fun _schema_version pattern matches ->
      (pattern, matches))
  |> Object.mem "schema_version" string ~enc:(fun _ ->
      Oi.Stamp.json_schema_version)
  |> Object.mem "pattern" string ~enc:(fun (p, _) -> p)
  |> Object.mem "matches" (list json_row_codec) ~enc:(fun (_, m) -> m)
  |> Object.finish

let render_json ~pattern ~sorted =
  let rows =
    List.map
      (fun r ->
        ( kind_string r.kind,
          r.overlay,
          r.binary,
          r.pkg_name,
          r.pkg_version,
          state_label r.state,
          r.hash ))
      sorted
  in
  match
    Jsont_bytesrw.encode_string ~format:Jsont.Indent json_envelope_codec
      (pattern, rows)
  with
  | Ok s ->
      print_string s;
      print_newline ()
  | Error e -> Oi.Error.fail_config_error "json encode failed: %s" e

let state_style = function
  | Local -> Oi.Style.ok
  | Remote -> Oi.Style.info
  | Declared -> Oi.Style.warn

let row_spans r =
  let nv =
    match (r.binary, r.findlib) with
    | Some b, _ -> Fmt.str "%s (%s.%s)" b r.pkg_name r.pkg_version
    | None, Some lib -> Fmt.str "%s (%s.%s)" lib r.pkg_name r.pkg_version
    | None, None -> Fmt.str "%s.%s" r.pkg_name r.pkg_version
  in
  [
    Tty.Span.text (kind_string r.kind);
    Tty.Span.text r.overlay;
    Tty.Span.text nv;
    Tty.Span.styled Oi.Style.dim (short_hash r.hash);
    Tty.Span.styled (state_style r.state) (state_label r.state);
  ]

let table_cols =
  [
    Tty.Table.column "KIND";
    Tty.Table.column "OVERLAY";
    Tty.Table.column "NAME.VERSION";
    Tty.Table.column "HASH";
    Tty.Table.column "STATE";
  ]

(* Append the dependency rollup that [--long] prints under each row. *)
let print_row_deps ~db = function
  | None -> ()
  | Some h ->
      let deps = D10.Index.deps db ~hash:h in
      if deps = [] then Fmt.pr "  %a@." Oi.Style.pp_dim_string "(no deps)"
      else
        List.iter
          (fun (dep_name, dep_ver, dep_hash) ->
            Fmt.pr "  %a %s.%s@." Oi.Style.pp_dim_string
              (String.sub dep_hash 0 (min 12 (String.length dep_hash)))
              dep_name dep_ver)
          deps

(* Long mode interleaves dep lines under each row, so render one row at a
   time and append deps inline. *)
let render_long ~db sorted =
  List.iter
    (fun r ->
      let table =
        Tty.Table.of_rows ~header_style:Oi.Style.header table_cols
          [ row_spans r ]
      in
      Oi.Style.pp_table Fmt.stdout table;
      print_row_deps ~db r.hash;
      Fmt.pr "@.")
    sorted

let render_compact sorted =
  let table =
    Tty.Table.of_rows ~header_style:Oi.Style.header table_cols
      (List.map row_spans sorted)
  in
  Oi.Style.pp_table Fmt.stdout table

let render_text ~db ~long ~pattern sorted =
  if sorted = [] then Fmt.pr "No matches for %s@." pattern
  else if long then render_long ~db sorted
  else render_compact sorted

(* Project remote [index-full.json] rows into the [row] shape so they
   merge with the local-SQLite-derived rows. Remote rows are always
   tagged [Remote] (we don't try to cross-check whether the local d10
   has the layer; [strongest_per_key] picks [Local] when both are
   present). *)
let remote_bin_rows_of ~remote_full ~overlay_filter ~pattern =
  match remote_full with
  | None -> []
  | Some full ->
      Layer_index.remote_search_binary ~pattern full
      |> List.map (fun (bin, pkg_name, pkg_ver, hash, overlay) ->
          {
            kind = `Bin;
            overlay = overlay_of ~overlay_filter overlay;
            binary = Some bin;
            findlib = None;
            pkg_name;
            pkg_version = pkg_ver;
            state = Remote;
            hash = Some hash;
          })

let remote_pkg_rows_of ~remote_full ~overlay_filter ~pattern =
  match remote_full with
  | None -> []
  | Some full ->
      Layer_index.remote_search_package ~pattern full
      |> List.map (fun (pkg_name, pkg_ver, hash, overlay) ->
          {
            kind = `Pkg;
            overlay = overlay_of ~overlay_filter overlay;
            binary = None;
            findlib = None;
            pkg_name;
            pkg_version = pkg_ver;
            state = Remote;
            hash = Some hash;
          })

(* Build every row source and merge them: indexed binaries, packages, and
   findlib libs, plus declared packages scanned from overlay clones,
   plus whatever the registry's [index-full.json] reports. *)
let collect_rows ~db ~d10 ~remote_full ~reporepo_path ~os_key ~overlay_filter
    ~pattern =
  let bin = bin_rows_of ~db ~d10 ~os_key ~overlay_filter ~pattern in
  let pkg_built = pkg_built_rows_of ~db ~d10 ~os_key ~overlay_filter ~pattern in
  let lib = lib_rows_of ~db ~d10 ~os_key ~overlay_filter ~pattern in
  let pkg_declared =
    pkg_declared_rows_of ~reporepo_path ~pattern ~overlay_filter
  in
  let remote_bin = remote_bin_rows_of ~remote_full ~overlay_filter ~pattern in
  let remote_pkg = remote_pkg_rows_of ~remote_full ~overlay_filter ~pattern in
  filter_by_overlay ~overlay_filter bin
  @ filter_by_overlay ~overlay_filter pkg_built
  @ filter_by_overlay ~overlay_filter lib
  @ filter_by_overlay ~overlay_filter pkg_declared
  @ filter_by_overlay ~overlay_filter remote_bin
  @ filter_by_overlay ~overlay_filter remote_pkg

(* Collapse to latest version per (kind, overlay, name, binary) unless
   [--all-versions], then sort for stable readable output. *)
let finalise_rows ~all_versions rows =
  let strongest = strongest_per_key rows in
  let displayed =
    trim_to_latest ~all_versions strongest
      (fun r -> (r.kind, r.overlay, r.pkg_name, r.binary))
      (fun r -> r.pkg_version)
  in
  List.sort compare_rows displayed

let man_block =
  [
    `S Manpage.s_description;
    `P
      "Match $(b,PATTERN) against the local layer cache, the remote registry, \
       and every reporepo overlay's package list. One row per match.";
    `S "COLUMNS";
    `I
      ( "$(b,KIND)",
        "$(b,bin) (binary in $(b,fs/bin/)), $(b,lib) (ocamlfind library), or \
         $(b,pkg) (opam metadata)." );
    `I
      ( "$(b,OVERLAY)",
        "$(b,@handle) the match came from, or $(b,-) for pin-depends/untagged \
         layers." );
    `I
      ( "$(b,NAME.VERSION)",
        "Package; binary or library name prefixed for $(b,bin) and $(b,lib) \
         rows." );
    `I ("$(b,HASH)", "Short layer hash.");
    `I
      ( "$(b,STATE)",
        "$(b,local) (cached), $(b,remote) (fetchable), or $(b,declared) \
         (metadata only)." );
    `S Manpage.s_examples;
    `Pre
      "  oi search dune\n\
      \  oi search 'ocaml*'\n\
      \  oi search @avsm/irmin\n\
      \  oi search --overlay=avsm --overlay=default 'fmt*'\n\
      \  oi search --all-versions -l jsont";
  ]

(* Pattern positional argument. *)
let pattern_arg =
  Arg.(
    required
    & pos 0 (some string) None
    & info ~docv:"PATTERN"
        ~doc:
          "Name or glob ($(b,*) wildcard). Matched against binary names, opam \
           package names, and ocamlfind library names. $(b,@HANDLE/PATTERN) \
           restricts to one overlay; bare $(b,@HANDLE) lists everything in it \
           ($(b,@HANDLE/*))."
        [])

let all_versions_arg =
  Arg.(
    value & flag
    & info ~doc:"List every version. Default: latest per overlay only."
        [ "all-versions" ])

let overlay_arg =
  Arg.(
    value & opt_all string []
    & info ~docv:"HANDLE"
        ~doc:
          "Restrict results to an overlay. Repeatable. $(b,@HANDLE/PATTERN) is \
           shorthand."
        [ "overlay" ])

let long_arg =
  Arg.(
    value & flag
    & info ~doc:"List direct dependencies under each built match."
        [ "l"; "long" ])

let cmd =
  let run (c : Terms.common) registry all_versions overlay_filter long pattern =
    Harness.run @@ fun ~sw env ->
    let { Harness.fs; clock; sys; os_key; cache; http_session; _ } =
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    let reporepo_path = Oi.Source.Reporepo.env_path () in
    let pattern, overlay_filter =
      absorb_handle_prefix ~pattern ~overlay_filter
    in
    let clk = (clock :> D10.Config.clk) in
    let db = open_local_index ~sys ~fs ~cache ~os_key ~clk in
    let d10 : D10.Config.t =
      { sys; fs; clock = clk; root = Oi.Cache.root cache; os_key }
    in
    let remote_full =
      load_remote_full ~sys ~fs ~clk ~session:http_session ~cache ~os_key
        ~registry
    in
    let all_rows =
      collect_rows ~db ~d10 ~remote_full ~reporepo_path ~os_key ~overlay_filter
        ~pattern
    in
    let sorted = finalise_rows ~all_versions all_rows in
    (match c.format with
    | Json ->
        render_json ~pattern ~sorted;
        D10.Index.close db;
        exit 0
    | Text -> ());
    render_text ~db ~long ~pattern sorted;
    D10.Index.close db
  in
  let info =
    Cmd.info "search"
      ~doc:
        "Find binaries, libraries, and opam packages across caches and overlays"
      ~man:man_block
  in
  Cmd.v info
    Term.(
      const run $ Terms.common $ Terms.registry $ all_versions_arg $ overlay_arg
      $ long_arg $ pattern_arg)
