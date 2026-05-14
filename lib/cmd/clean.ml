open Cmdliner

let ( / ) = Filename.concat

(* -- Per-package layer invalidation -------------------------------------- *)

let parse_target s =
  match String.index_opt s '.' with
  | Some i when i > 0 && i < String.length s - 1 ->
      let name = String.sub s 0 i in
      let version = String.sub s (i + 1) (String.length s - i - 1) in
      (name, Some version)
  | _ -> (s, None)

let short h = String.sub h 0 (min 12 (String.length h))

(* Walk the dep graph upward to find every layer that transitively depends on
   the [frontier] set. Used by [pkg_clean] so dropping a leaf doesn't leave
   dangling consumers in the cache. *)
let close_dependents db ~os_key direct_hashes =
  let rec walk acc frontier =
    if frontier = [] then acc
    else
      let next = D10.Index.dependents db ~hashes:frontier ~os_key in
      let fresh = List.filter (fun h -> not (List.mem h acc)) next in
      walk (acc @ fresh) fresh
  in
  walk [] direct_hashes

let direct_layers_for db ~os_key ~name ~version_opt =
  match version_opt with
  | Some v -> (
      match D10.Index.find_layer db ~name ~version:v ~os_key with
      | Some (h, _) -> [ (name, v, h) ]
      | None -> [])
  | None ->
      D10.Index.search_package db ~pattern:name ~os_key
      |> List.map (fun (n, v, h, _) -> (n, v, h))

let print_pkg_clean_plan ~dry_run ~target ~direct ~dependents =
  let verb = if dry_run then "Would remove" else "Removing" in
  let dim s = Fmt.str "%a" Oi.Style.pp_dim_string s in
  Oi.Say.step "%s %d layer(s) for %s" verb (List.length direct) target;
  List.iter
    (fun (n, v, h) -> Oi.Say.info "%s.%s  %s" n v (dim (short h)))
    direct;
  if dependents <> [] then begin
    Oi.Say.step "%s %d dependent layer(s)" verb (List.length dependents);
    List.iter (fun h -> Oi.Say.info "%s" (dim (short h))) dependents
  end

(* Actually remove every layer hash in [all_hashes] from both the
   on-disk store under [layers_root] and the index [db]. *)
let pkg_clean_apply ~fs ~db ~layers_root ~all_hashes =
  List.iter
    (fun h -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(fs / layers_root / h))
    all_hashes;
  D10.Index.delete_layers db ~hashes:all_hashes;
  Oi.Say.ok "removed %d layer(s)" (List.length all_hashes);
  Oi.Say.info "run 'oi build' to rebuild what you still need"

(* Plan + execute the clean for the layers that match [target] (already
   resolved into [direct]). Returns the total number of layers removed
   (or that would be removed under [--dry-run]). *)
let pkg_clean_with_layers ~fs ~db ~os_key ~layers_root ~target ~direct ~dry_run
    =
  let direct_hashes = List.map (fun (_, _, h) -> h) direct in
  let dependents = close_dependents db ~os_key direct_hashes in
  let all_hashes = direct_hashes @ dependents in
  print_pkg_clean_plan ~dry_run ~target ~direct ~dependents;
  if not dry_run then pkg_clean_apply ~fs ~db ~layers_root ~all_hashes;
  D10.Index.close db;
  List.length all_hashes

let pkg_clean ~sys ~fs ~clock ~cache ~os_key ~target ~dry_run =
  let name, version_opt = parse_target target in
  let layers_root = Oi.Cache.root_s cache / "layers" / os_key in
  let index_path = Layer_index.ensure_local ~sys ~fs ~clock ~cache ~os_key in
  if not (Eio.Path.is_file Eio.Path.(fs / index_path)) then begin
    Oi.Say.info "no layer index for %s; nothing to do" os_key;
    0
  end
  else
    let db = D10.Index.open_ ~fs ~path:index_path in
    let direct = direct_layers_for db ~os_key ~name ~version_opt in
    if direct = [] then begin
      Oi.Say.info "no cached layers found for %s" target;
      D10.Index.close db;
      0
    end
    else
      pkg_clean_with_layers ~fs ~db ~os_key ~layers_root ~target ~direct
        ~dry_run

(* -- Bulk clean ---------------------------------------------------------- *)

(* The full set of "what would I clean?" flags from the Cmdliner term.
   Bundled into a record so the helpers don't have a dozen-arg call
   signature each. *)
type bulk = {
  all : bool;
  toolchains : bool;
  sources : bool;
  mirror : bool;
  layers : bool;
  prefixes : bool;
  build : bool;
  runs : bool;
  run_cache : bool;
  solve_cache : bool;
  dune_cache : bool;
  repos : bool;
  opam_root : bool;
  pins : bool;
}

let any_bulk (b : bulk) =
  b.all || b.toolchains || b.sources || b.mirror || b.layers || b.prefixes
  || b.build || b.runs || b.run_cache || b.solve_cache || b.dune_cache
  || b.repos || b.opam_root || b.pins

(* [--all] sweeps everything in [cleanable_items], so adding a new
   category there picks it up automatically. Each label shown in the
   cleanable-items table has a matching flag, plus convenience bundles:
   [--sources] clears both the tarball directory and the
   content-addressed mirror; [--layers] cascades into every cache that
   indexes off layer hashes (prefixes, run-cache, solve-cache, build,
   runs) because dropping a layer invalidates those too. *)
let want_label (b : bulk) = function
  | _ when b.all -> true
  | "toolchains" -> b.toolchains
  | "sources" -> b.sources
  | "mirror" -> b.sources || b.mirror
  | "layers" -> b.layers
  | "runs" -> b.layers || b.runs
  | "run-cache" -> b.layers || b.run_cache
  | "solve-cache" -> b.layers || b.solve_cache
  | "build" -> b.layers || b.build
  | "prefixes" -> b.layers || b.prefixes
  | "dune" -> b.dune_cache
  | "repos" -> b.repos
  | "opam-root" -> b.opam_root
  | "pins" -> b.pins
  | _ -> false

(* One row of the "what could you clean?" table emitted when [oi clean]
   runs with no flags. *)
let cleanable_row ~sys (item : Oi.Cache.item) =
  let flag = "--" ^ item.label in
  let size =
    if Eio.Path.is_directory item.path || Eio.Path.is_file item.path then
      Fmt.kstr Tty.Span.text "%a" Oi.Cache.pp_size
        (Oi.Cache.size ~sys item.path)
    else Tty.Span.styled Oi.Style.dim "(empty)"
  in
  [ Tty.Span.text flag; size; Tty.Span.text item.description ]

let list_cleanable ~sys ~cache ~data_dir =
  Fmt.pr "%a@.@." Oi.Style.pp_header_string "Cleanable items";
  let items = Oi.Cache.cleanable_items cache ~data_dir in
  let rows = List.map (cleanable_row ~sys) items in
  let table =
    Tty.Table.of_rows ~header_style:Oi.Style.header
      [
        Tty.Table.column "FLAG";
        Tty.Table.column ~align:`Right "SIZE";
        Tty.Table.column "DESCRIPTION";
      ]
      rows
  in
  Oi.Style.pp_table Fmt.stdout table;
  Oi.Say.newline ();
  Oi.Say.info "use --all to clean everything, or select specific items"

(* Remove one cache item, or print what would be removed under
   [--dry-run]. Skips items whose path doesn't exist on disk. *)
let rm_item ~sys ~dry_run (item : Oi.Cache.item) =
  if Eio.Path.is_directory item.path || Eio.Path.is_file item.path then
    let sz = Oi.Cache.size ~sys item.path in
    if dry_run then
      Oi.Say.info "would remove %s (%a) %s" item.label Oi.Cache.pp_size sz
        (Eio.Path.native_exn item.path)
    else begin
      Eio.Path.rmtree ~missing_ok:true item.path;
      Oi.Say.ok "removed %s (%a)" item.label Oi.Cache.pp_size sz
    end

let bulk_clean ~sys ~cache ~data_dir ~bulk ~dry_run =
  let items = Oi.Cache.cleanable_items cache ~data_dir in
  List.iter
    (fun (item : Oi.Cache.item) ->
      if want_label bulk item.label then rm_item ~sys ~dry_run item)
    items;
  Oi.Say.step "Done"

(* -- Cmdliner glue ------------------------------------------------------- *)

let target_conflict_msg =
  "PKG positional cannot be combined with bulk flags (--all, --toolchains, \
   --sources, --mirror, --layers, --prefixes, --build, --runs, --run-cache, \
   --solve-cache, --dune, --repos, --opam-root, --pins)"

let run (c : Terms.common) (bulk : bulk) ~dry_run ~target =
  Harness.run @@ fun ~sw env ->
  let { Harness.fs; clock; sys; os_key; cache; _ } =
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  let data_dir = c.data_dir in
  let has_bulk = any_bulk bulk in
  match target with
  | Some t ->
      if has_bulk then begin
        Oi.Say.error "%s" target_conflict_msg;
        exit 1
      end
      else
        let clk = (clock :> D10.Config.clk) in
        ignore (pkg_clean ~sys ~fs ~clock:clk ~cache ~os_key ~target:t ~dry_run)
  | None when not has_bulk -> list_cleanable ~sys ~cache ~data_dir
  | None -> bulk_clean ~sys ~cache ~data_dir ~bulk ~dry_run

let flag_arg ~doc name = Arg.(value & flag & info ~doc [ name ])

let layers_arg =
  Arg.(
    value & flag
    & info
        ~doc:
          "Layer cache. Cascades into the prefixes, run cache, solve cache, \
           build state, and runs since they all index off layer hashes."
        [ "layers" ])

(* Cmdliner can't $-apply 14 booleans into a record builder without
   triggering the "boolean blindness" lint, so we build [bulk_term] as a
   tower of pairs and unpack into the record once at the end. The two
   halves are [bulk_term_a] (broad knobs) and [bulk_term_b] (cache
   sub-divisions); the order matches [bulk] field order so the unpack is
   mechanical. *)
let bulk_term_a =
  let all = flag_arg ~doc:"Every category. Full reset." "all" in
  let toolchains =
    flag_arg ~doc:"Fixed-prefix toolchain installs (oxcaml)." "toolchains"
  in
  let sources =
    flag_arg ~doc:"Source tarballs and content-addressed mirror." "sources"
  in
  let mirror = flag_arg ~doc:"Content-addressed source mirror only." "mirror" in
  let prefixes = flag_arg ~doc:"Assembled prefix cache only." "prefixes" in
  let build =
    flag_arg ~doc:"Build state ($(b,_build/), logs, audit log)." "build"
  in
  let runs = flag_arg ~doc:"Cached script builds." "runs" in
  Term.(
    const (fun a b c d e f g -> (a, b, c, d, e, f, g))
    $ all $ toolchains $ sources $ mirror $ prefixes $ build $ runs)

let bulk_term_b =
  let run_cache =
    flag_arg ~doc:"Fast-exec cache for $(b,oi run) only." "run-cache"
  in
  let solve_cache = flag_arg ~doc:"Solver memoisation only." "solve-cache" in
  let dune_cache = flag_arg ~doc:"Dune's shared build cache." "dune" in
  let repos = flag_arg ~doc:"$(b,--with-repo) clones." "repos" in
  let opam_root =
    flag_arg ~doc:"Opam scaffolding under $(b,\\$OI_DATA_DIR/opam-root/)."
      "opam-root"
  in
  let pins =
    flag_arg ~doc:"Pin-depends sources and synthesized package trees." "pins"
  in
  Term.(
    const (fun a b c d e f -> (a, b, c, d, e, f))
    $ run_cache $ solve_cache $ dune_cache $ repos $ opam_root $ pins)

let bulk_term =
  let assemble (all, toolchains, sources, mirror, prefixes, build, runs) layers
      (run_cache, solve_cache, dune_cache, repos, opam_root, pins) =
    {
      all;
      toolchains;
      sources;
      mirror;
      layers;
      prefixes;
      build;
      runs;
      run_cache;
      solve_cache;
      dune_cache;
      repos;
      opam_root;
      pins;
    }
  in
  Term.(const assemble $ bulk_term_a $ layers_arg $ bulk_term_b)

let dry_run_arg =
  Arg.(
    value & flag
    & info ~doc:"Print what would be removed; delete nothing."
        [ "n"; "dry-run" ])

let target_arg =
  Arg.(
    value
    & pos 0 (some string) None
    & info ~docv:"PKG[.VERSION]"
        ~doc:
          "Drop layers for one package; transitive dependents are cascaded. \
           Mutually exclusive with the bulk flags."
        [])

let man =
  [
    `S Manpage.s_description;
    `P
      "Remove rebuildable cache data. With no flags or $(b,PKG), list each \
       category and its disk usage. Flags are additive. A project's $(b,_oi/) \
       and the reporepo are never touched.";
    `P
      "$(b,PKG) drops every cached version of that package; $(b,PKG.VERSION) \
       drops one. Layers that transitively depend on a removed entry are \
       dropped too. Re-run $(b,oi build) to rebuild.";
    `Pre "  oi clean --layers\n  oi clean dune\n  oi clean dune.3.22.1 -n";
  ]

let cmd =
  let info =
    Cmd.info "clean" ~doc:"Delete cached data to free disk space" ~man
  in
  let go c bulk dry_run target = run c bulk ~dry_run ~target in
  Cmd.v info
    Term.(const go $ Terms.common $ bulk_term $ dry_run_arg $ target_arg)
