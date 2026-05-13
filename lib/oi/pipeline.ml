[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.pipeline"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* -- Platform / d10 wiring ----------------------------------------------- *)

let conf ~platform:(p : Osrel.t) ~ocaml_version : Solver.Ctx.conf =
  {
    arch = Osrel.Arch.to_string p.arch;
    os = Osrel.OS.to_string p.os;
    os_distribution = Osrel.OS.kind_to_string p.os.kind;
    os_version = p.os.version;
    os_family = p.os.family;
    ocaml_version;
    jobs = p.jobs;
  }

let d10 ~sys ~fs ~clock ~cache ~os_key : D10.Config.t =
  { sys; fs; clock; root = Cache.root cache; os_key }

let init_opam_root ~fs ~data_dir =
  let opam_root = data_dir / "opam-root" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / opam_root);
  Solver.Ctx.init_opam ~root:opam_root

(* -- Toolchain ----------------------------------------------------------- *)

let solver_inputs info conf =
  let conf = Toolchain.apply_conf info conf in
  let ctx = Option.map Toolchain.opam_ctx_of_info info in
  (conf, ctx)

(* Toolchain names implied by a single handle: the [x-oi-toolchain]
   field on its latest reporepo entry, plus the handle's own name when
   it itself is a toolchain definition (i.e. some entry has matching
   [x-oi-toolchain-name]). Both pickup paths are needed: an overlay
   pinned to a non-default toolchain via [oi repo add --toolchain=oxcaml]
   uses (1); a [@oxcaml/utop] target uses (2). *)
let toolchain_names_of_handle entries handle =
  let from_field =
    match Source.Reporepo.latest entries ~handle with
    | Some (e : Source.Reporepo.entry) -> Stdlib.Option.to_list e.toolchain
    | None -> []
  in
  let from_self =
    if Toolchain.depends_of ~handle <> None then [ handle ] else []
  in
  from_field @ from_self

let no_default_toolchain_fail ~entries ~path =
  let known =
    entries
    |> List.filter_map (fun (e : Source.Reporepo.entry) -> e.toolchain_name)
    |> List.sort_uniq String.compare
  in
  let hint =
    if known = [] then "the reporepo has no toolchain definitions yet"
    else
      Fmt.str
        "mark one with: oi repo bump <handle> --default. Known toolchains: %s"
        (String.concat ", " known)
  in
  Error.fail_config_error
    "no default toolchain set in reporepo at %s — %s. Or pass --toolchain=NAME \
     explicitly."
    path hint

let pick_default_or_fail ~entries ~path =
  match Source.Reporepo.default_toolchain entries with
  | Some e ->
      let n = Stdlib.Option.value e.toolchain_name ~default:e.handle in
      Log.debug (fun m -> m "Using default toolchain %s" n);
      n
  | None -> no_default_toolchain_fail ~entries ~path

let pick_from_scope_or_default ~entries ~path ~handles =
  let from_scope =
    List.concat_map (toolchain_names_of_handle entries) handles
    |> List.sort_uniq String.compare
  in
  match from_scope with
  | [ n ] ->
      Log.debug (fun m -> m "Using toolchain %s from handle scope" n);
      n
  | many when many <> [] ->
      Error.fail_config_error
        "overlays in scope declare conflicting toolchains: %s — pass \
         --toolchain=NAME to disambiguate"
        (String.concat ", " many)
  | _ -> pick_default_or_fail ~entries ~path

let pick_toolchain_handle ~entries ~path ~override ~handles =
  match override with
  | Some h ->
      Log.debug (fun m -> m "Using --toolchain=%s" h);
      h
  | None -> pick_from_scope_or_default ~entries ~path ~handles

let pick_toolchain ~fs ~sys ~data_dir ~conf ~install ~override ~handles
    ?(reporter = Build_progress.null) () =
  let path = Source.Reporepo.env_path () in
  let entries =
    if Sys.file_exists path then
      try Source.Reporepo.load ~path with Error.E _ -> []
    else []
  in
  let handle = pick_toolchain_handle ~entries ~path ~override ~handles in
  let info = Toolchain.resolve ~fs ~sys ~data_dir ~conf ~handle in
  if install then Toolchain.ensure_installed ~reporter ~fs info;
  Some info

let strip_compiler_roots_for_override ~override ~toolchain names =
  match (override, (toolchain : Toolchain.info option)) with
  | None, _ | _, None -> names
  | Some _, Some info ->
      List.filter
        (fun n -> not (OpamPackage.Name.Set.mem n info.root_names))
        names

(* -- Sources ------------------------------------------------------------- *)

let classify_with_args ~fs ~sys ~cache ?refresh
    ?(reporter = Build_progress.null) with_deps =
  if with_deps <> [] then
    Fmt.kstr
      (fun s -> reporter.event (Status s))
      "Loading %d --with arg(s)" (List.length with_deps);
  let urls, pkg_deps = Project.Url.classify_all with_deps in
  let url_project =
    Project.Url.materialize ~reporter ~fs ~sys ~cache ?refresh urls
  in
  (pkg_deps, url_project)

let overlay_compatible ~entries ~(info : Toolchain.info) h =
  match Source.Reporepo.latest entries ~handle:h with
  | None -> true
  | Some (e : Source.Reporepo.entry) -> (
      match e.toolchain with
      | None -> true
      | Some t when t = info.handle -> true
      | Some t ->
          Logs.info (fun m ->
              m
                "Dropping overlay @%s: built against toolchain %s, \
                 incompatible with auto-picked toolchain %s. Pass \
                 --toolchain=%s to override."
                h t info.handle t);
          false)

let filter_compatible_overlays ~reporepo_path ?(override = None) ~toolchain
    handles =
  match (override, (toolchain : Toolchain.info option)) with
  | Some _, _ ->
      (* User explicitly named a toolchain via [--toolchain=NAME]. Honour
         every declared overlay verbatim — if one is genuinely incompatible
         the solver will surface the constraint failure. *)
      handles
  | None, None -> handles
  | None, Some info ->
      let entries =
        try Source.Reporepo.load ~path:reporepo_path with Error.E _ -> []
      in
      List.filter (overlay_compatible ~entries ~info) handles

(* -- Build helpers ------------------------------------------------------- *)

let cache_urls ~cache ~source_remote =
  let local = Source.Mirror.url ~cache in
  match source_remote with
  | Some (`Http_remote r) -> [ local; Source.Mirror.remote_url ~registry:r ]
  | None | Some _ -> [ local ]

(* HTTP fetches are I/O-bound: a fiber spends ~all of its wall-time
   waiting on the wire, so we run more concurrent fibers than CPUs.
   Default 8 — large enough to amortise TLS handshakes against a small
   pool, small enough that one wedged registry connection doesn't
   monopolise the [max_connections_per_host] cap and starve the
   remaining fibers. Override via [OI_HTTP_PARALLELISM]; [?jobs] (which
   originates from [-j N] and primarily caps build subprocesses) wins
   when explicitly set.

   Previously defaulted to [max(domain_count, 16)] which on a 32-core
   build host (oi.ci.dev) opened up to 32 simultaneous registry fetches
   with one HTTPS-keepalive connection each — when a remote stops
   responding mid-stream, the half-closed sockets pile up faster than
   the per-host pool can recycle them. *)
let fetch_parallelism ?jobs () =
  let default = 8 in
  match jobs with
  | Some n when n > 0 -> n
  | _ -> (
      match Sys.getenv_opt "OI_HTTP_PARALLELISM" with
      | Some s -> (
          match int_of_string_opt s with Some n when n > 0 -> n | _ -> default)
      | None -> default)

(* -- Layer fetch ---------------------------------------------------------- *)

(* Pull every layer in [available] from [remote]. UI is decoupled —
   we only emit typed events through [reporter]; the cmdliner layer
   renders them as a multi-line bar (or text, or nothing). *)
let fetch_layers ?jobs ~reporter ~session ~remote ~d10 ~index ~available ~pkg_of
    () =
  let bytes_received = ref 0L in
  let done_count = ref 0 in
  let lock = Mutex.create () in
  let with_lock f = Mutex.protect lock f in
  let total = List.length available in
  let progressed = ref 0 in
  (* Initial Aggregate seeds the bar's denominator so the count
     fraction is meaningful from the moment the phase begins. *)
  reporter.Build_progress.event
    (Aggregate { phase = Fetching; total; current = 0 });
  Eio.Fiber.List.iter
    ~max_fibers:(fetch_parallelism ?jobs ())
    (fun hash ->
      let sha256 =
        Option.map
          (fun (e : D10.Layer.index_entry) -> e.sha256)
          (Hashtbl.find_opt index hash)
      in
      let size =
        match Hashtbl.find_opt index hash with
        | Some (e : D10.Layer.index_entry) -> e.size
        | None -> 0L
      in
      let pkg =
        Stdlib.Option.value (Hashtbl.find_opt pkg_of hash) ~default:""
      in
      reporter.Build_progress.event
        (Fetch_started { kind = Layer; key = hash; pkg; size });
      let received_ref = ref 0L in
      let on_progress ~received ~total =
        with_lock (fun () ->
            let prev = !received_ref in
            received_ref := received;
            bytes_received :=
              Int64.add !bytes_received (Int64.sub received prev));
        let total_known = match total with Some t -> t | None -> size in
        reporter.event
          (Fetch_progress
             { kind = Layer; key = hash; bytes = received; total = total_known })
      in
      let ok =
        D10.Layer.pull_remote d10 ~remote ~hash ~session ~on_progress ?sha256 ()
      in
      reporter.event (Fetch_finished { kind = Layer; key = hash });
      let now =
        with_lock (fun () ->
            incr progressed;
            !progressed)
      in
      reporter.event (Aggregate { phase = Fetching; total; current = now });
      if ok then begin
        with_lock (fun () -> incr done_count);
        Logs.info (fun m -> m "Fetched %s from registry" hash)
      end)
    available;
  (!done_count, !bytes_received)

let fetch_layer_hashes ?(reporter = Build_progress.null) ?jobs ~session ~remote
    ~d10 ~index ~hashes ~pkg_of () =
  ignore
    (fetch_layers ?jobs ~reporter ~session ~remote ~d10 ~index ~available:hashes
       ~pkg_of ())

let assemble_prefix ~sys ~fs ~clock ~cache ~os_key ~layer_hashes =
  let d10 = d10 ~sys ~fs ~clock:(clock :> D10.Config.clk) ~cache ~os_key in
  D10.Prefix.assemble_cached d10 ~layer_hashes
