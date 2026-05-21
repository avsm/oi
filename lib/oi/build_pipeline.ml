let ( / ) = Filename.concat

type env = {
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  sys : D10.Sysops.t;
  os_key : string;
  cache : Cache.t;
  data_dir : string;
  http_session : D10.Sysops.Http.session;
}

type target =
  | Plain of string
  | Group of { tokens : string list; handles : string list }
  | Overlay_pkg of { handle : string; spec : string }
  | Overlay_all of string

let parse s =
  match Build_request.parse_build_target s with
  | Plain_target s -> Plain s
  | Overlay_pkg (h, p) -> Overlay_pkg { handle = h; spec = p }
  | Overlay_all h -> Overlay_all h

type request = {
  targets : target list;
  with_repos : string list;
  pins : Project.pin list;
  extra_repos : Project.extra_repo list;
  constraints : OpamFormula.version_constraint OpamPackage.Name.Map.t;
  toolchain_override : string option;
  toolchain : Toolchain.info option;
  conf : Solver.Ctx.conf;
  local_packages_dir : string option;
  project_root : string option;
  force_source : bool;
  with_test : bool;
  refresh : bool;
}

type group = {
  label : string;
  tokens : string list;
  names : OpamPackage.Name.t list;
  handles : string list;
  group_constraints : OpamFormula.version_constraint OpamPackage.Name.Map.t;
}

type group_error =
  | Solve_failed of { msg : string; log_path : string }
  | Cycle of OpamPackage.t list list
  | Empty_after_strip
  | Elaborate_failed of { msg : string }
  | Emit_failed of { msg : string }

type group_result = {
  group : group;
  toolchain : Toolchain.info option;
  pkgs_dir : string list;
  pkgs : OpamPackage.t list;
  exec_plan : Plan.t option;
  recipe : D10ir.Plan.t option;
  error : (unit, group_error) result;
}

type solved = {
  groups : group_result list;
  merged : D10ir.Plan.t option;
  toolchain : Toolchain.info option;
}

(* -- Target → solve-group expansion --------------------------------------- *)

(* Expand bare [@HANDLE] entries into per-package solve groups by reading the
   overlay's [x-root-packages] (or, if empty, every package the overlay
   ships). Plain and [Overlay_pkg] targets become singleton groups; the
   overall token list mirrors what [oi build]'s loop sees today. *)
let expand_targets ~fs ~sys ~reporepo_path ~reporepo_url (targets : target list)
    : (string list * string list) list =
  let need_reporepo =
    List.exists
      (function
        | Overlay_all _ | Overlay_pkg _ -> true
        | Plain _ -> false
        | Group { handles; _ } -> handles <> [])
      targets
  in
  let entries_lazy =
    if need_reporepo then
      lazy
        (Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false
           ~path:reporepo_path ~url:reporepo_url ();
         Source.Reporepo.load ~path:reporepo_path)
    else lazy []
  in
  let groups_of_overlay_entry ~handle (e : Source.Reporepo.entry) =
    if e.root_packages <> [] then e.root_packages
    else
      let pkgs_dir =
        Source.Reporepo.assert_overlay_dir ~path:reporepo_path ~handle
      in
      Sys.readdir pkgs_dir |> Array.to_list
      |> List.filter (fun n -> Sys.is_directory (pkgs_dir / n))
      |> List.sort String.compare
      |> List.map (fun p -> [ p ])
  in
  let expand_overlay_all handle =
    let entries = Lazy.force entries_lazy in
    match Source.Reporepo.latest entries ~handle with
    | None -> Error.fail_config_error "no overlay @%s in reporepo" handle
    | Some e ->
        List.map
          (fun group -> (group, [ handle ]))
          (groups_of_overlay_entry ~handle e)
  in
  List.concat_map
    (fun t ->
      match t with
      | Plain p -> [ ([ p ], []) ]
      | Group { tokens; handles } -> [ (tokens, handles) ]
      | Overlay_pkg { handle; spec } -> [ ([ spec ], [ handle ]) ]
      | Overlay_all handle -> expand_overlay_all handle)
    targets

(* -- Per-group toolchain / packages_dirs ---------------------------------- *)

type aux_installer =
  env:env -> ?reporter:Build_progress.reporter -> Toolchain.info -> unit

(* Pick one toolchain for the whole batch. Same precedence rules
   {Pipeline.pick_toolchain} uses for any other command — explicit
   override wins, then implicit pickup from the union of in-scope
   handles, then the reporepo default. Per-group toolchain selection
   is a corner case [oi build] supports today; we'll thread that in if
   a caller actually needs it.

   No "is the prefix populated?" post-condition here: for non-relocatable
   toolchains the populating happens later, via the [aux_installer] hook
   that {!solve_uncached} invokes immediately after this call returns.
   Relocatable toolchains never need the prefix on disk in the first
   place. *)
let pick_batch_toolchain ?reporter ~env ~conf ~override ~all_handles () =
  let key = List.sort_uniq String.compare all_handles in
  Pipeline.pick_toolchain ?reporter ~fs:env.fs ~sys:env.sys
    ~data_dir:env.data_dir ~conf ~install:true ~override ~handles:key ()

(* Preserve insertion order (not alphabetical) so the overlay-precedence
   filter in {!Solver.Ctx.create} sees [dep_handles] in the order the
   toolchain reporepo entry declared them. Without this, two overlays
   that ship the same package name would be loaded alphabetically and the
   "earlier wins" rule could pick the wrong one. *)
let dedup_preserving_order xs =
  let seen = Hashtbl.create 8 in
  List.filter
    (fun x ->
      if Hashtbl.mem seen x then false
      else (
        Hashtbl.add seen x ();
        true))
    xs

let load_reporepo_entries ~reporepo_path =
  try Source.Reporepo.load ~path:reporepo_path
  with Sys_error _ | Failure _ -> []

(* Resolve [effective] handles to reporepo entries. With no active
   toolchain we run the full [resolve] dep walk so package ordering
   matches [dep_handles]; with a toolchain we just look up the latest
   entry per handle — the toolchain context already pins versions. *)
let resolve_overlay_entries ~toolchain ~entries effective =
  match (toolchain : Toolchain.info option) with
  | None -> (
      let roots =
        List.rev effective
        |> List.map (fun h : Source.Reporepo.root ->
            { handle = h; version = None })
      in
      try Source.Reporepo.resolve entries ~roots |> List.rev
      with Error.E _ -> [])
  | Some _ ->
      List.filter_map
        (fun h -> Source.Reporepo.latest entries ~handle:h)
        effective

let packages_dirs_for_group ~reporepo_path ~base_pkgs_dirs
    ?(toolchain_override = None) ?pin_dir ?local_packages_dir ~global_handles
    ~toolchain handles =
  let global_handles =
    Pipeline.filter_compatible_overlays ~reporepo_path
      ~override:toolchain_override ~toolchain global_handles
  in
  let effective = dedup_preserving_order (global_handles @ handles) in
  let entries = load_reporepo_entries ~reporepo_path in
  let resolved = resolve_overlay_entries ~toolchain ~entries effective in
  let overlay_dirs =
    List.map
      (fun (e : Source.Reporepo.entry) ->
        Source.Reporepo.assert_overlay_dir ~path:reporepo_path ~handle:e.handle)
      resolved
  in
  let base =
    match (toolchain : Toolchain.info option) with
    | None -> base_pkgs_dirs
    | Some i -> i.packages_dirs
  in
  dedup_preserving_order
    (Stdlib.Option.to_list local_packages_dir
    @ Stdlib.Option.to_list pin_dir
    @ overlay_dirs @ base)

(* -- Per-group solve / elaborate / emit ----------------------------------- *)

(* Accumulated state of a single solve_group invocation. Each phase updates
   one or two fields and either short-circuits with an error or threads the
   state forward. [finalize] turns it into the externally-visible
   {!group_result}. *)
type group_state = {
  group : group;
  toolchain : Toolchain.info option;
  pkgs_dir : string list;
  pkgs : OpamPackage.t list;
  exec_plan : Plan.t option;
  recipe : D10ir.Plan.t option;
}

let finalize (s : group_state) error : group_result =
  {
    group = s.group;
    toolchain = s.toolchain;
    pkgs_dir = s.pkgs_dir;
    pkgs = s.pkgs;
    exec_plan = s.exec_plan;
    recipe = s.recipe;
    error;
  }

(* Strip compiler-family roots from a group's tokens when a [--toolchain]
   override is active, so the override's pins land cleanly. *)
let strip_toolchain_tokens ~toolchain_override ~toolchain tokens =
  match (toolchain_override, toolchain) with
  | Some _, Some (info : Toolchain.info) ->
      List.filter
        (fun pkg ->
          let name, _ = Build_request.parse_pkg_target pkg in
          not (OpamPackage.Name.Set.mem name info.root_names))
        tokens
  | _ -> tokens

(* Build a group record + the matching constraints map from the stripped
   token list. *)
let build_group_record ~label ~tokens ~stripped_tokens ~base_constraints
    ~group_handles =
  let items = List.map Build_request.parse_pkg_target stripped_tokens in
  let names = List.map fst items in
  let group_constraints =
    List.fold_left
      (fun acc (name, c) ->
        match c with
        | None -> acc
        | Some c -> OpamPackage.Name.Map.add name c acc)
      base_constraints items
  in
  let group =
    { label; tokens; names; handles = group_handles; group_constraints }
  in
  (group, names, group_constraints)

(* Persist the solver failure log under [<cache>/build/logs] and return the
   path so the user can pick it up from the build summary. *)
let write_solve_log ~env ~cache_root ~stripped_tokens ~group_handles msg =
  let key =
    String.concat " "
      (stripped_tokens @ List.map (fun h -> "@" ^ h) group_handles)
  in
  let hash = Digest.to_hex (Digest.string key) in
  let first = match stripped_tokens with t :: _ -> t | [] -> "solve" in
  let path = Cache.Logs.path ~cache_root ~kind:"solve" ~name:first ~hash in
  let body =
    Fmt.str "targets: %s\nhandles: %s\n\n%s%s"
      (String.concat ", " stripped_tokens)
      (if group_handles = [] then "(base only)"
       else String.concat ", " (List.map (fun h -> "@" ^ h) group_handles))
      msg
      (if msg = "" || msg.[String.length msg - 1] = '\n' then "" else "\n")
  in
  Cache.Logs.write ~fs:env.fs ~cache_root path body;
  path

(* Pick the recipe's informational [base_layer]: prefer external layers
   (non-relocatable toolchain pkgs), else first consumer package's hash.
   Empty result means everything was toolchain-provided. *)
let toolchain_layer_of (p : Plan.t) =
  match (p.external_layer_hashes, p.packages) with
  | h :: _, _ -> h
  | [], q :: _ -> q.layer_hash
  | [], [] -> ""

(* Pins and local trees don't carry [x-d10-archive] in their opam files —
   that field is only written by [oi repo bump]. Recipe emit requires an
   archive sha for every package, so we inline-bake those here. *)
let bake_inline_archives ~env ~d10 ~cache_root (p : Plan.t) =
  let bake_one (q : Plan.package_plan) =
    match (q.d10_archive, q.overlay) with
    | None, None ->
        let built =
          Archive_builder.build ~proc_mgr:env.proc_mgr ~fs:env.fs ~d10
            ~cache_root q
        in
        { q with d10_archive = Some built.sha256 }
    | _ -> q
  in
  { p with packages = List.map bake_one p.packages }

(* Run [Plan.of_solution] under a [Cycle] guard. *)
let plan_of_solution ~force_source ~d10 ~gctx ~pkgs_dir pkgs =
  try
    let plan_d10 = if force_source then None else Some d10 in
    Ok (Plan.of_solution gctx ?d10:plan_d10 ~packages_dirs:pkgs_dir pkgs)
  with Plan.Cycle cs -> Error (Cycle cs)

(* Run [Plan.elaborate] under [Error.E] / [Failure] guards. *)
let elaborate_plan ~env ~cache_root ~gctx ~pkgs_dir ~group_conf build_plan =
  try
    Ok
      (Plan.elaborate gctx ~packages_dirs:pkgs_dir ~cache_root
         ~os_key:env.os_key ~ocaml_version:group_conf.Solver.Ctx.ocaml_version
         build_plan)
  with
  | Error.E e -> Error (Elaborate_failed { msg = Fmt.str "%a" Error.pp e })
  | Failure msg -> Error (Elaborate_failed { msg })

(* Bake archives + emit recipe under [Error.E] / [Failure] / [Invalid_argument]
   guards. Returns the (possibly mutated) exec_plan alongside the recipe. *)
let emit_recipe ~env ~d10 ~cache_root ~extra_owned_paths ~toolchain_name
    ~toolchain_layer exec_plan =
  try
    let exec_plan = bake_inline_archives ~env ~d10 ~cache_root exec_plan in
    let recipe =
      Recipe_emitter.emit ~d10 ~cli_invocation:(Array.to_list Sys.argv)
        ~extra_owned_paths ~toolchain_name ~toolchain_layer exec_plan
    in
    Ok (exec_plan, recipe)
  with
  | Error.E e -> Error (Emit_failed { msg = Fmt.str "%a" Error.pp e })
  | Failure msg | Invalid_argument msg -> Error (Emit_failed { msg })

let toolchain_handle_or_system (t : Toolchain.info option) =
  match t with Some i -> i.handle | None -> "system"

(* Stage 2 of the pipeline: post-elaborate handling — handle the
   all-toolchain-pkgs short-circuit and the recipe emit + archive bake. *)
let finish_after_elaborate ~env ~d10 ~cache_root ~toolchain exec_plan =
  let toolchain_name = toolchain_handle_or_system toolchain in
  (* A non-relocatable toolchain (oxcaml) lives at a fixed
     [$XDG_CACHE_HOME/oi/toolchains/<id>] prefix that [oi] source-built and
     owns. The dir is already under [Cache.toolchains_root] which
     [Recipe_emitter]'s PATH scrub keeps unconditionally, so no extra
     path-owning bookkeeping is needed for the [+ox] flow today; we still
     thread an empty list here in case future toolchains land outside the
     cache. *)
  let extra_owned_paths =
    match (toolchain : Toolchain.info option) with
    | Some i when not i.relocatable -> [ i.install_prefix ]
    | _ -> []
  in
  let toolchain_layer = toolchain_layer_of exec_plan in
  if toolchain_layer = "" then
    (* Every selected package was filtered out by [elaborate] (target
       reduced entirely to toolchain-provided packages, e.g.
       [oi build ocaml-variants] under a non-relocatable toolchain).
       Nothing for the d10ir executor to do. *)
    Error
      ( Some exec_plan,
        Emit_failed
          {
            msg =
              "all selected packages are toolchain-provided; nothing to build";
          } )
  else
    match
      emit_recipe ~env ~d10 ~cache_root ~extra_owned_paths ~toolchain_name
        ~toolchain_layer exec_plan
    with
    | Ok (exec_plan, recipe) -> Ok (exec_plan, recipe)
    | Error err -> Error (Some exec_plan, err)

(* Combined [of_solution → elaborate → emit_recipe] pipeline. Returns the
   final [(exec_plan, recipe)] pair on success or [(exec_plan_so_far, err)]
   on failure so the caller still gets a partial [Plan.t] in the result. *)
let build_recipe_pipeline ~env ~d10 ~cache_root ~gctx ~pkgs_dir ~group_conf
    ~force_source ~toolchain ~pkgs :
    (Plan.t * D10ir.Plan.t, Plan.t option * group_error) result =
  match plan_of_solution ~force_source ~d10 ~gctx ~pkgs_dir pkgs with
  | Error err -> Error (None, err)
  | Ok build_plan -> (
      match
        elaborate_plan ~env ~cache_root ~gctx ~pkgs_dir ~group_conf build_plan
      with
      | Error err -> Error (None, err)
      | Ok exec_plan ->
          finish_after_elaborate ~env ~d10 ~cache_root ~toolchain exec_plan)

(* Run the solver + recipe pipeline once a group's solver context is
   prepared. Threads [state] forward, attaching [pkgs] / [exec_plan] /
   [recipe] as each phase succeeds. *)
let run_group_solve ~env ~d10 ~cache_root ~gctx ~pkgs_dir ~group_conf
    ~group_constraints ~stripped_tokens ~group_handles ~force_source ~toolchain
    ~with_test ~names state =
  let test_for_roots =
    if with_test then OpamPackage.Name.Set.of_list names
    else OpamPackage.Name.Set.empty
  in
  match
    Solver.solve ~test:test_for_roots ~sys:env.sys ~fs:env.fs ~cache_root gctx
      ~packages_dirs:pkgs_dir ~constraints:group_constraints names
  with
  | Error msg ->
      let log_path =
        write_solve_log ~env ~cache_root ~stripped_tokens ~group_handles msg
      in
      finalize state (Error (Solve_failed { msg; log_path }))
  | Ok pkgs -> (
      let state = { state with pkgs } in
      match
        build_recipe_pipeline ~env ~d10 ~cache_root ~gctx ~pkgs_dir ~group_conf
          ~force_source ~toolchain ~pkgs
      with
      | Ok (exec_plan, recipe) ->
          finalize
            { state with exec_plan = Some exec_plan; recipe = Some recipe }
            (Ok ())
      | Error (exec_plan_opt, err) ->
          finalize { state with exec_plan = exec_plan_opt } (Error err))

let solve_group ~env ~conf ~toolchain_override ~global_handles ~base_pkgs_dirs
    ~pin_dir ?local_packages_dir ~reporepo_path ~base_constraints ~build_prefix
    ~cache_root ~d10 ~force_source ~with_test
    (toolchain : Toolchain.info option)
    ((tokens, group_handles) : string list * string list) : group_result =
  let label = String.concat ", " tokens in
  let pkgs_dir =
    packages_dirs_for_group ~reporepo_path ~base_pkgs_dirs ~toolchain_override
      ?pin_dir ?local_packages_dir ~global_handles ~toolchain group_handles
  in
  let group_conf, tc_ctx = Pipeline.solver_inputs toolchain conf in
  let stripped_tokens =
    strip_toolchain_tokens ~toolchain_override ~toolchain tokens
  in
  let empty_group =
    {
      label;
      tokens;
      names = [];
      handles = group_handles;
      group_constraints = OpamPackage.Name.Map.empty;
    }
  in
  let state0 =
    {
      group = empty_group;
      toolchain;
      pkgs_dir;
      pkgs = [];
      exec_plan = None;
      recipe = None;
    }
  in
  if stripped_tokens = [] then finalize state0 (Error Empty_after_strip)
  else
    let group, names, group_constraints =
      build_group_record ~label ~tokens ~stripped_tokens ~base_constraints
        ~group_handles
    in
    let gctx =
      Solver.Ctx.create ~prefix:build_prefix ~packages_dirs:pkgs_dir
        ~conf:group_conf ?toolchain:tc_ctx ()
    in
    run_group_solve ~env ~d10 ~cache_root ~gctx ~pkgs_dir ~group_conf
      ~group_constraints ~stripped_tokens ~group_handles ~force_source
      ~toolchain ~with_test ~names { state0 with group }

(* -- Persistent cache for the [solved] struct ----------------------------- *)

(* Bumped whenever the [solved] / [group_result] / [Plan.t] / [D10ir.Plan.t]
   shape changes in a way that breaks Marshal compatibility, or when
   [add_target] / [add_request_body] change their canonical byte
   encoding (so an old cache entry that keyed under a different
   canonicalisation produces a miss rather than a wrong hit). Folded
   into the cache key so a stale entry produces a key miss rather than
   a crash on [Marshal.from_channel].
   Bumped to v4: [request.with_test] now gates whether the per-group
   solve enables [{with-test}] for its roots; v3 keys did not encode
   that bit, so toggling between [oi build] (no test deps) and
   [oi build --test] (test deps) on the same target would alias to the
   same key and return a stale solve. *)
let cache_schema = "v4"
let log_src = Logs.Src.create "oi.build_pipeline.cache"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* Process-memo of [git -C <repo> rev-parse HEAD] so a multi-call
   command (e.g. [oi build] solving 81 groups) doesn't re-shell N
   times for the same repo. *)
let head_memo : (string, string option) Hashtbl.t = Hashtbl.create 8

(* Run an argv via the Eio process manager and return its trimmed stdout, or
   [None] on any failure (non-git directory, missing git, signal kill).
   Equivalent of the old [Unix.open_process_in + close_process_in WEXITED 0]
   dance, now routed through [D10.Sysops.Cmd.run_out_quiet] so stderr is
   swallowed and the whole call lives under the Eio fiber tree. *)
let run_out_opt ~sys argv =
  try
    let s = D10.Sysops.Cmd.run_out_quiet sys argv |> String.trim in
    if s = "" then None else Some s
  with Eio.Io _ -> None

let git_head_of ~sys dir =
  match Hashtbl.find_opt head_memo dir with
  | Some r -> r
  | None ->
      let r = run_out_opt ~sys [ "git"; "-C"; dir; "rev-parse"; "HEAD" ] in
      Hashtbl.add head_memo dir r;
      r

(* Cache-key signature: the reporepo HEAD plus a digest of the
   request's targets / scope / constraints / conf. Returns [None] if
   the reporepo isn't a git tree (the common case in tests / fresh
   installs without [oi repo bump]) — the caller falls back to an
   uncached solve.

   The encoding canonicalises equivalent target shapes so the same
   user-visible target keys identically regardless of how the CLI
   front-end built it. [Group { tokens = [t]; handles = [h] }] and
   [Overlay_pkg { handle = h; spec = t }] expand into the same
   [(tokens, handles)] pair in [expand_targets]; [Group { tokens = [t];
   handles = [] }] is equivalent to [Plain t]. Without this, [oi build
   @h/t] (which constructs the [Group] form) and [oi install @h/t]
   (which constructs [Overlay_pkg]) produced different cache keys for
   the same solve, so one path could hit a stale marshalled solve while
   the other freshly re-solved — exactly the divergence we hit with
   [@samoht/merlint] when its source moved upstream. *)
let add_target add t =
  match t with
  | Plain p -> add ("Plain:" ^ p)
  | Group { tokens = [ tok ]; handles = [] } -> add ("Plain:" ^ tok)
  | Group { tokens = [ tok ]; handles = [ h ] } -> add ("Pkg:" ^ h ^ "/" ^ tok)
  | Group { tokens; handles } ->
      add "Group:";
      List.iter add tokens;
      add "|";
      List.iter add handles
  | Overlay_pkg { handle; spec } -> add ("Pkg:" ^ handle ^ "/" ^ spec)
  | Overlay_all h -> add ("All:" ^ h)

let add_conf add (c : Solver.Ctx.conf) =
  add ("ocaml:" ^ c.ocaml_version);
  add ("arch:" ^ c.arch);
  add ("os:" ^ c.os);
  add ("os_distribution:" ^ c.os_distribution);
  add ("os_version:" ^ c.os_version);
  add ("os_family:" ^ c.os_family)

let add_request_body add (req : request) =
  add "TARGETS:";
  List.iter (add_target add) req.targets;
  add "WITH_REPOS:";
  List.iter add (List.sort String.compare req.with_repos);
  add "PINS:";
  List.iter
    (fun (p : Project.pin) ->
      add (OpamPackage.to_string p.pkg);
      add (OpamUrl.to_string p.url))
    req.pins;
  add "EXTRA:";
  List.iter
    (fun (e : Project.extra_repo) ->
      add e.name;
      add e.url;
      add (Stdlib.Option.value e.local_packages_dir ~default:""))
    req.extra_repos;
  add "CONSTRAINTS:";
  OpamPackage.Name.Map.iter
    (fun n (relop, v) ->
      add (OpamPackage.Name.to_string n);
      add (OpamPrinter.FullPos.relop_kind relop);
      add (OpamPackage.Version.to_string v))
    req.constraints;
  add ("override:" ^ Stdlib.Option.value req.toolchain_override ~default:"");
  add
    ("toolchain_hash:"
    ^ match req.toolchain with Some i -> i.hash | None -> "");
  add_conf add req.conf;
  add ("local_pkg_dir:" ^ Stdlib.Option.value req.local_packages_dir ~default:"");
  add ("project_root:" ^ Stdlib.Option.value req.project_root ~default:"");
  add ("force_source:" ^ string_of_bool req.force_source);
  add ("with_test:" ^ string_of_bool req.with_test)

let cache_key ~sys ~reporepo_path (req : request) : string option =
  match git_head_of ~sys reporepo_path with
  | None ->
      Log.info (fun m -> m "cache disabled: no git HEAD at %s" reporepo_path);
      None
  | Some head ->
      let buf = Buffer.create 1024 in
      let add s =
        Buffer.add_string buf s;
        Buffer.add_char buf '\x00'
      in
      add ("schema:" ^ cache_schema);
      add ("repo:" ^ head);
      add_request_body add req;
      Some (Digest.to_hex (Digest.string (Buffer.contents buf)))

let cache_path ~cache_root ~key =
  Filename.concat cache_root
    (Filename.concat "solve-cache"
       (Filename.concat (String.sub key 0 2) (key ^ ".marshal")))

let cache_lookup ~cache_root ~key : solved option =
  let path = cache_path ~cache_root ~key in
  if not (Sys.file_exists path) then None
  else
    try
      Some
        (In_channel.with_open_bin path (fun ic ->
             (Marshal.from_channel ic : solved)))
    with exn ->
      Log.info (fun m ->
          m "solve cache: ignoring stale entry %s (%s)" (String.sub key 0 12)
            (Printexc.to_string exn));
      (try Sys.remove path with Sys_error _ -> ());
      None

let cache_store ~fs ~cache_root ~key (s : solved) : unit =
  let path = cache_path ~cache_root ~key in
  let dir = Filename.dirname path in
  (try Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dir)
   with Eio.Exn.Io _ -> ());
  let tmp = path ^ ".tmp" in
  try
    Out_channel.with_open_bin tmp (fun oc -> Marshal.to_channel oc s []);
    Sys.rename tmp path;
    Log.info (fun m ->
        m "solve cache stored %s (%d groups)" (String.sub key 0 12)
          (List.length s.groups))
  with exn -> (
    Log.info (fun m ->
        m "solve cache: failed to store %s (%s)" (String.sub key 0 12)
          (Printexc.to_string exn));
    try Sys.remove tmp with Sys_error _ -> ())

(* -- The top-level entry point -------------------------------------------- *)

(* Union every handle in scope: [with_repos] plus per-target [@h] tokens.
   Used both for toolchain selection and per-group [packages_dirs]. *)
let collect_handles (req : request) =
  let token_handles =
    List.concat_map
      (function
        | Plain _ -> []
        | Group { handles; _ } -> handles
        | Overlay_pkg { handle; _ } | Overlay_all handle -> [ handle ])
      req.targets
  in
  let all_handles =
    List.sort_uniq String.compare (req.with_repos @ token_handles)
  in
  (token_handles, all_handles)

(* Materialise pin sources and resolve [base_pkgs_dirs] / [global_handles] —
   everything the per-group solver needs to find on disk. *)
let prepare_sources ~env ~reporter ~req ~toolchain ~token_handles =
  let _ : string list =
    Source.Reporepo.ensure_base ~fs:env.fs ~sys:env.sys ~data_dir:env.data_dir
      ~refresh:req.refresh ()
  in
  let pins =
    Source.Pin.resolve_pins ~fs:env.fs ~sys:env.sys
      ?project_root:req.project_root req.pins
  in
  let pin_dir =
    Source.Pin.materialize ~fs:env.fs ~sys:env.sys ~cache:env.cache
      ~refresh:req.refresh ~reporter pins
  in
  let _extra_pkg_dirs : string list =
    Source.Repo.ensure_many ~fs:env.fs ~data_dir:env.data_dir
      ~refresh:req.refresh req.extra_repos
  in
  let base_pkgs_dirs =
    match toolchain with
    | None ->
        Source.Reporepo.ensure_base ~fs:env.fs ~sys:env.sys
          ~data_dir:env.data_dir ()
    | Some (i : Toolchain.info) -> i.packages_dirs
  in
  let global_handles =
    let toks = List.sort_uniq String.compare token_handles in
    List.filter (fun h -> not (List.mem h toks)) req.with_repos
  in
  (pin_dir, base_pkgs_dirs, global_handles)

(* Loop over every solve group, emitting per-group + aggregate progress
   events, returning the list of per-group results in order. *)
let solve_each_group ~env ~reporter ~conf ~toolchain ~req ~global_handles
    ~base_pkgs_dirs ~pin_dir ~reporepo_path ~build_prefix ~cache_root ~d10
    ~with_test token_groups =
  let n_groups = List.length token_groups in
  reporter.Build_progress.event
    (Phase_started
       { phase = Solving; label = Fmt.str "Solving %d group(s)" n_groups });
  reporter.event (Aggregate { phase = Solving; total = n_groups; current = 0 });
  let solved_count = ref 0 in
  let groups =
    List.map
      (fun ((tokens, _) as g) ->
        let label = String.concat ", " tokens in
        reporter.event (Solve_started { label });
        let res =
          solve_group ~env ~conf ~toolchain_override:req.toolchain_override
            ~global_handles ~base_pkgs_dirs ~pin_dir
            ?local_packages_dir:req.local_packages_dir ~reporepo_path
            ~base_constraints:req.constraints ~build_prefix ~cache_root ~d10
            ~force_source:req.force_source ~with_test toolchain g
        in
        reporter.event (Solve_finished { label });
        incr solved_count;
        reporter.event
          (Aggregate
             { phase = Solving; total = n_groups; current = !solved_count });
        res)
      token_groups
  in
  reporter.event (Phase_done Solving);
  groups

(* Merge every group's recipe into one [D10ir.Plan.t]; [None] when no
   group produced a recipe. *)
let merge_group_recipes groups =
  let recipes = List.filter_map (fun (gr : group_result) -> gr.recipe) groups in
  match recipes with
  | [] -> None
  | _ -> (
      match D10ir.Plan.merge recipes with
      | Ok r -> Some r
      | Error msg ->
          Error.fail_config_error
            "merging %d recipes failed: %s. This is a bug in D10ir.Plan.merge: \
             every recipe was produced by the same toolchain in the same \
             batch."
            (List.length recipes) msg)

let solve_uncached env ?(reporter = Build_progress.null) ?aux_installer
    (req : request) : solved =
  let reporepo_path = Source.Reporepo.env_path () in
  let reporepo_url = Source.Reporepo.env_url () in
  Pipeline.init_opam_root ~fs:env.fs ~data_dir:env.data_dir;
  let token_handles, all_handles = collect_handles req in
  let toolchain =
    match req.toolchain with
    | Some i -> Some i
    | None ->
        pick_batch_toolchain ~reporter ~env ~conf:req.conf
          ~override:req.toolchain_override ~all_handles ()
  in
  (* Eagerly populate the toolchain's aux prefix here, after we've
     resolved [toolchain] regardless of how (caller-supplied
     [req.toolchain] OR our own [pick_batch_toolchain]). The
     [aux_installer] parameter passes through to {!Aux_install.ensure}
     when supplied; its sub-build's recursive {!solve} omits the
     parameter so we don't re-enter. *)
  (match (toolchain, aux_installer) with
  | Some i, Some f -> f ~env ?reporter:(Some reporter) i
  | _ -> ());
  let conf, _tc_ctx = Pipeline.solver_inputs toolchain req.conf in
  let pin_dir, base_pkgs_dirs, global_handles =
    prepare_sources ~env ~reporter ~req ~toolchain ~token_handles
  in
  let cache_root = Cache.root_s env.cache in
  let build_prefix = cache_root / "build" / "prefix" in
  let d10 =
    Pipeline.d10 ~sys:env.sys ~fs:env.fs
      ~clock:(env.clock :> D10.Config.clk)
      ~cache:env.cache ~os_key:env.os_key
  in
  let token_groups =
    expand_targets ~fs:env.fs ~sys:env.sys ~reporepo_path ~reporepo_url
      req.targets
  in
  let groups =
    solve_each_group ~env ~reporter ~conf ~toolchain ~req ~global_handles
      ~base_pkgs_dirs ~pin_dir ~reporepo_path ~build_prefix ~cache_root ~d10
      ~with_test:req.with_test token_groups
  in
  let merged = merge_group_recipes groups in
  { groups; merged; toolchain }

(* Cache-wrapped public entry point. [request.refresh = true]
   forces a fresh solve (skips lookup) but still updates the cache
   so the next run is fast. Lookup is skipped entirely when
   [cache_key] returns [None] (no git HEAD to anchor staleness on)
   — there's nothing safe to store against, so storing is also
   skipped in that case. *)
(* Cross-process serialisation against [oi repo bump] (and against
   other [oi]s) is provided by {!Oi.Lock.acquire_global} in
   {!Cmd.Harness.bootstrap}. *)
let solve env ?(reporter = Build_progress.null) ?aux_installer (req : request) :
    solved =
  let reporepo_path = Source.Reporepo.env_path () in
  let cache_root = Cache.root_s env.cache in
  let key_opt = cache_key ~sys:env.sys ~reporepo_path req in
  let cached =
    match key_opt with
    | None -> None
    | Some _ when req.refresh -> None
    | Some key -> cache_lookup ~cache_root ~key
  in
  match cached with
  | Some s ->
      let key = Stdlib.Option.get key_opt in
      Log.info (fun m ->
          m "solve cache hit %s (%d groups)" (String.sub key 0 12)
            (List.length s.groups));
      Fmt.kstr
        (fun s -> reporter.Build_progress.event (Status s))
        "Solve cache hit (%d groups)" (List.length s.groups);
      (* Even on a cache hit, the aux prefix may need populating —
         the solve cache only covers the solver state, not the
         on-disk install of the toolchain's aux packages. *)
      (match (s.toolchain, aux_installer) with
      | Some i, Some f -> f ~env ?reporter:(Some reporter) i
      | _ -> ());
      s
  | None ->
      let s = solve_uncached env ~reporter ?aux_installer req in
      (* Only cache successful solves. A solve where every group
         failed produces a [merged = None] struct that's not worth
         re-loading. *)
      let any_ok =
        List.exists (fun (gr : group_result) -> Result.is_ok gr.error) s.groups
      in
      (match key_opt with
      | Some key when any_ok -> cache_store ~fs:env.fs ~cache_root ~key s
      | _ -> ());
      s

(* -- Post-merge fetch / archive prefetch / Direct.run --------------------- *)

type build_inputs = {
  solved : solved;
  layer_remote : D10.Layer.remote option;
  source_remote : D10.Layer.remote option;
  jobs : int option;
  upload_archive_url : string option;
  archive_sources : bool;
  snapshot_reporepo : bool;
  install_to : string option;
}

(* -- Upload primitives ------------------------------------------------ *)

(* When [OI_S3CFG] points at a readable file, pass [-c <path>] so s3cmd
   uses that config instead of [~/.s3cfg]. Lets the [oi docker --all]
   templates write a synthesised config under [/tmp] and have streaming
   uploads from inside [oi build] pick it up without depending on the
   container user's HOME layout. *)
let s3cmd_argv args =
  match Sys.getenv_opt "OI_S3CFG" with
  | Some path when path <> "" -> "s3cmd" :: "-c" :: path :: args
  | _ -> "s3cmd" :: args

let s3_put_quiet ~sys ~src ~dst =
  try D10.Sysops.Cmd.run sys (s3cmd_argv [ "put"; "--quiet"; src; dst ])
  with exn ->
    Logs.warn (fun m -> m "upload-archive: %s: %s" dst (Printexc.to_string exn))

let stat_size_of path =
  try Some (Unix.stat path).Unix.st_size with Unix.Unix_error _ -> None

let sha256_of_file_quiet path =
  try Some (OpamHash.contents (OpamHash.compute ~kind:`SHA256 path))
  with Sys_error _ | Failure _ | Unix.Unix_error _ -> None

(* Once a layer's [.tar.zst] is staged, fill in [tarball.{sha256,size,key}]
   on the existing layer manifest (which P2 wrote with placeholder zeros)
   so the uploaded manifest accurately describes the blob it accompanies. *)
let patch_layer_manifest_tarball ~fs ~cache_root ~os_key ~hash ~tar_path =
  let manifest_path = Manifest_layer.path_for ~cache_root ~os_key ~hash in
  if not (Sys.file_exists manifest_path) then ()
  else
    match Manifest_layer.try_read ~path:manifest_path with
    | None -> ()
    | Some m -> (
        let sha = sha256_of_file_quiet tar_path in
        let size = stat_size_of tar_path in
        match (sha, size) with
        | Some sha256, Some size ->
            let tarball : Manifest_layer.tarball =
              {
                sha256;
                size;
                key = Some (os_key ^ "/layers/" ^ hash ^ ".tar.zst");
              }
            in
            let m' = { m with tarball = Some tarball } in
            Manifest_layer.write ~fs ~cache_root m'
        | _ -> ())

(* Fold the build-log tail into the layer manifest's [log] field so the
   registry's only per-layer artefact is the [<hash>.json] (no sibling
   [<hash>.log]). Mirrors [patch_layer_manifest_tarball]: read the
   already-written manifest, splice in the tail, write back. Best-effort
   — a missing/short log just leaves [log = None]. *)
let log_tail_of_file ~path : Manifest_layer.log_tail option =
  match Audit.tail_of_file ~lines:200 ~path () with
  | None | Some "" -> None
  | Some text ->
      let n = List.length (String.split_on_char '\n' text) in
      Some { lines = n; truncated = n >= 200; text }

let patch_layer_manifest_log ~fs ~cache_root ~os_key ~hash ~log_path =
  let manifest_path = Manifest_layer.path_for ~cache_root ~os_key ~hash in
  if not (Sys.file_exists manifest_path && Sys.file_exists log_path) then ()
  else
    match Manifest_layer.try_read ~path:manifest_path with
    | None -> ()
    | Some m -> (
        match log_tail_of_file ~path:log_path with
        | None -> ()
        | Some _ as log -> Manifest_layer.write ~fs ~cache_root { m with log })

(* Upload one freshly built layer: stage the .tar.zst, patch the local
   manifest with its real sha256+size, then PUT the tarball and the JSON
   manifest to <url_base>/<os_key>/layers/<hash>.{tar.zst,json}. The
   build-log tail rides inside the .json (folded in by
   [patch_layer_manifest_log] before this runs) — there is no separate
   <hash>.log object, so the Clickhouse indexer that scans
   */layers/*.json picks the log up for free. *)
let upload_one_layer ~fs ~sys ~(d10 : D10.Config.t) ~staging ~cache_root
    ~url_base hash =
  let os_key = d10.os_key in
  let staged =
    try D10.Layer.export d10 ~hash ~dst:Eio.Path.(d10.fs / staging)
    with Eio.Exn.Io _ | Sys_error _ -> false
  in
  ignore (staged : bool);
  let tar_src =
    Filename.concat (Filename.concat staging os_key) (hash ^ ".tar.zst")
  in
  patch_layer_manifest_tarball ~fs ~cache_root ~os_key ~hash ~tar_path:tar_src;
  if Sys.file_exists tar_src then
    s3_put_quiet ~sys ~src:tar_src
      ~dst:(Fmt.str "%s%s/layers/%s.tar.zst" url_base os_key hash);
  let manifest_src = Manifest_layer.path_for ~cache_root ~os_key ~hash in
  if Sys.file_exists manifest_src then
    s3_put_quiet ~sys ~src:manifest_src
      ~dst:(Fmt.str "%s%s/layers/%s.json" url_base os_key hash)

(* PUT only the [<hash>.json] sidecar (no [.tar.zst]). Used for every
   non-success outcome — build failure, dep-skip, solve/cycle/elaborate/
   emit failure — none of which produced a layer or tarball. Keeping
   this distinct from [upload_one_layer] avoids that function's pointless
   [D10.Layer.export] + tarball-sha probe for layers that never existed. *)
let put_layer_json ~sys ~url_base ~cache_root ~os_key ~hash =
  let src = Manifest_layer.path_for ~cache_root ~os_key ~hash in
  if Sys.file_exists src then
    s3_put_quiet ~sys ~src
      ~dst:(Fmt.str "%s%s/layers/%s.json" url_base os_key hash)

(* Provenance is written here rather than inside [D10ir.Direct] because the
   richer [Plan.package_plan] (opam_path, pkgs_dir, source, depexts, dep_layers
   with Identity.dep shape) only exists at the oi level — the d10ir runtime
   sees a stripped [Plan.node]. Idempotent: skips when the file already exists,
   so re-runs (and previously cached layers seen across [oi build] passes)
   converge to a written sidecar without re-encoding on every invocation. *)
let source_kind_of_url url =
  if url = "" then ""
  else if String.starts_with ~prefix:"git+" url then "git"
  else if String.starts_with ~prefix:"git://" url then "git"
  else if String.starts_with ~prefix:"hg+" url then "hg"
  else if String.starts_with ~prefix:"darcs+" url then "darcs"
  else if String.starts_with ~prefix:"file:" url then "local"
  else if String.starts_with ~prefix:"/" url then "local"
  else "tar"

let provenance_of_package_plan ~os_key ~ocaml_version ~built_at
    (pp : Plan.package_plan) : Provenance.t =
  let pkg = Identity.of_string pp.pkg in
  let origin =
    match (pp.opam_path, pp.pkgs_dir) with
    | Some _opam_path, Some pkgs_dir ->
        Origin.of_packages_dir ~pkgs_dir ~name:pkg.name ~full:pp.pkg
    | _ -> { Origin.kind = Local; overlay = pp.overlay; path_in_repo = "" }
  in
  let opam_sha256 =
    match pp.opam_path with
    | Some p -> Provenance.hash_opam_file ~path:p
    | None -> ""
  in
  let source =
    Option.map
      (fun (s : Plan.source_info) : Provenance.source_info ->
        {
          url = s.url;
          kind = source_kind_of_url s.url;
          checksums = s.checksums;
        })
      pp.source
  in
  {
    schema = 1;
    layer_hash = pp.layer_hash;
    os_key;
    pkg;
    method_ = pp.method_;
    built_at;
    duration_s = 0.;
    phases = { fetch = None; build = None; install = None; restore = None };
    opam = { sha256 = opam_sha256; origin };
    source;
    deps = pp.dep_layers;
    depexts_declared = pp.depexts;
    build_env = { ocaml_version };
  }

let string_of_method = function
  | Identity.Source -> "Source"
  | Identity.Binary -> "Binary"

let dep_of_identity (d : Identity.dep) : Manifest_layer.dep =
  { name = d.id.name; version = d.id.version; hash = d.hash }

(* Look up the d10ir node that produced [hash] in [merged].
   Used to fold the build "recipe" (script + env + substs) into the
   unified layer manifest sidecar so the cache no longer needs a
   separate recipe.json. *)
let recipe_of_d10ir_node (n : D10ir.Plan.node) : Manifest_layer.recipe =
  let subst_vars =
    List.filter_map
      (fun s ->
        match String.index_opt s '=' with
        | None -> None
        | Some i ->
            Some
              (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1)))
      n.subst_vars
  in
  { script = n.script; env = n.env; substs = n.substs; subst_vars }

let recipe_for_hash nodes_by_hash hash : Manifest_layer.recipe option =
  match Hashtbl.find_opt nodes_by_hash hash with
  | None -> None
  | Some n -> Some (recipe_of_d10ir_node n)

let build_nodes_by_hash (s : solved) =
  let tbl = Hashtbl.create 64 in
  (match s.merged with
  | None -> ()
  | Some (m : D10ir.Plan.t) ->
      List.iter
        (fun (n : D10ir.Plan.node) ->
          let h = D10ir.Layer_hash.to_string n.layer_hash in
          Hashtbl.replace tbl h n)
        m.nodes);
  tbl

(* Layer-hash → [Plan.package_plan] index, used by the streaming
   uploader to recover the per-layer manifest inputs (recipe, deps,
   depexts, source_archive sha) at [Node_built] time. Multiple groups
   can solve the same package; later entries overwrite earlier ones —
   but the package_plan is keyed by the layer hash, so duplicates are
   value-equivalent. *)
let build_pp_by_hash (s : solved) =
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun (gr : group_result) ->
      match gr.exec_plan with
      | None -> ()
      | Some (ep : Plan.t) ->
          List.iter
            (fun (pp : Plan.package_plan) ->
              Hashtbl.replace tbl pp.layer_hash (pp, ep.ocaml_version))
            ep.packages)
    s.groups;
  tbl

let split_pkg pkg =
  match String.index_opt pkg '.' with
  | None -> (pkg, "")
  | Some i ->
      (String.sub pkg 0 i, String.sub pkg (i + 1) (String.length pkg - i - 1))

let binaries_of_files files =
  List.filter_map
    (fun (f : Manifest_layer.file_entry) ->
      if
        f.kind <> "dir"
        && String.length f.path > 4
        && String.sub f.path 0 4 = "bin/"
      then Some (String.sub f.path 4 (String.length f.path - 4))
      else None)
    files

let findlib_of_fs ~(d10 : D10.Config.t) fs_dir :
    Manifest_layer.findlib_entry list =
  D10.Index.scan_meta ~fs:d10.fs fs_dir
  |> List.map (fun (package_dir, findlib_pkg, archive) ->
      { Manifest_layer.package_dir; findlib_pkg; archive })

let source_archive_of (pp : Plan.package_plan) :
    Manifest_layer.source_archive option =
  Stdlib.Option.map
    (fun sha ->
      {
        Manifest_layer.sha256 = sha;
        key = Some ("d10ir-archives/" ^ sha ^ ".tar.zst");
      })
    pp.d10_archive

let overlay_handle_of pp =
  Stdlib.Option.map (fun (o : D10.Overlay.t) -> o.handle) pp.Plan.overlay

let overlay_version_of pp =
  Stdlib.Option.bind pp.Plan.overlay (fun (o : D10.Overlay.t) ->
      if o.version = "" then None else Some o.version)

(* Placeholder tarball — the .tar.zst sha is computed and patched into the
   sidecar at staging time, since the layer hash differs from the tarball
   hash. *)
let placeholder_tarball : Manifest_layer.tarball =
  { sha256 = ""; size = 0; key = None }

let layer_manifest_of_package ~os_key ~ocaml_version ~built_at ~recipe
    (d10 : D10.Config.t) (pp : Plan.package_plan) : Manifest_layer.t =
  let layer_dir = D10.Layer.dir d10 ~hash:pp.layer_hash in
  let fs_dir = Eio.Path.native_exn Eio.Path.(layer_dir / "fs") in
  let files = Manifest_layer.files_of_fs_dir fs_dir in
  let provenance =
    provenance_of_package_plan ~os_key ~ocaml_version ~built_at pp
  in
  let name, version = split_pkg pp.pkg in
  Manifest_layer.success ~hash:pp.layer_hash ~os_key ~package:pp.pkg
    ~package_name:name ~package_ver:version
    ~method_:(string_of_method pp.method_)
    ?overlay_handle:(overlay_handle_of pp)
    ?overlay_version:(overlay_version_of pp) ~tarball:placeholder_tarball ~files
    ~binaries:(binaries_of_files files)
    ~findlib:(findlib_of_fs ~d10 fs_dir)
    ~exit_status:0 ~provenance ?recipe ?source_archive:(source_archive_of pp)
    ~deps:(List.map dep_of_identity pp.dep_layers)
    ~depexts_declared:pp.depexts ~build_env_ocaml_version:ocaml_version ()

let write_layer_manifest_for_package ~fs ~cache_root ~os_key ~ocaml_version
    ~built_at ~nodes_by_hash (d10 : D10.Config.t) (pp : Plan.package_plan) =
  if not (D10.Layer.succeeded d10 ~hash:pp.layer_hash) then ()
  else
    let recipe = recipe_for_hash nodes_by_hash pp.layer_hash in
    let m =
      layer_manifest_of_package ~os_key ~ocaml_version ~built_at ~recipe d10 pp
    in
    (* Preserve any [tarball] field a streaming-upload reporter has
       already patched on the on-disk manifest — without this guard
       the post-build [write_provenance_for_solved] would clobber the
       sha256+size we just computed when the .tar.zst was staged. *)
    let m =
      let existing_path =
        Manifest_layer.path_for ~cache_root ~os_key ~hash:pp.layer_hash
      in
      match Manifest_layer.try_read ~path:existing_path with
      | Some existing -> (
          match existing.tarball with
          | Some t when t.sha256 <> "" -> { m with tarball = Some t }
          | _ -> m)
      | None -> m
    in
    Manifest_layer.write ~fs ~cache_root m

(* Failure counterpart of [layer_manifest_of_package]. A failed build
   stored no layer (no [fs/], no [.tar.zst]), so files/binaries/findlib
   are empty and the build-log tail rides in [failure.log]; everything
   else (package, overlay, deps, recipe, provenance, source-archive
   pointer) mirrors the success manifest so the registry UI can show a
   failed layer with the same depth as a successful one. *)
let layer_failure_manifest_of_package ~os_key ~ocaml_version ~built_at ~recipe
    ~phase ~duration_s ~log (_ : D10.Config.t) (pp : Plan.package_plan) :
    Manifest_layer.t =
  let provenance =
    provenance_of_package_plan ~os_key ~ocaml_version ~built_at pp
  in
  let name, version = split_pkg pp.pkg in
  Manifest_layer.failure ~hash:pp.layer_hash ~os_key ~package:pp.pkg
    ~package_name:name ~package_ver:version
    ~method_:(string_of_method pp.method_)
    ?overlay_handle:(overlay_handle_of pp)
    ?overlay_version:(overlay_version_of pp) ~phase ~duration_s ?log ~provenance
    ?recipe ?source_archive:(source_archive_of pp)
    ~deps:(List.map dep_of_identity pp.dep_layers)
    ~depexts_declared:pp.depexts ~build_env_ocaml_version:ocaml_version ()

let write_layer_failure_manifest ~fs ~cache_root ~os_key ~ocaml_version
    ~built_at ~nodes_by_hash ~phase ~duration_s ~log (d10 : D10.Config.t)
    (pp : Plan.package_plan) =
  let recipe = recipe_for_hash nodes_by_hash pp.layer_hash in
  let m =
    layer_failure_manifest_of_package ~os_key ~ocaml_version ~built_at ~recipe
      ~phase ~duration_s ~log d10 pp
  in
  Manifest_layer.write ~fs ~cache_root m

(* Write a layer manifest sidecar for every successfully built layer.
   The sidecar at [<cache>/layers/<os_key>/<hash>.json] now
   carries everything that used to live in the per-layer-dir
   [provenance.json] and [recipe.json] — those are no longer written.
   Local d10 callers that need provenance/recipe data read it from
   this sidecar via [Provenance.read_one] (which falls back to the
   sidecar's [provenance] field). *)
let write_provenance_for_solved ~fs ~cache_root ~os_key ~(d10 : D10.Config.t)
    (s : solved) =
  let built_at = Unix.gettimeofday () in
  let nodes_by_hash = build_nodes_by_hash s in
  List.iter
    (fun (gr : group_result) ->
      match gr.exec_plan with
      | None -> ()
      | Some (ep : Plan.t) ->
          List.iter
            (fun pp ->
              write_layer_manifest_for_package ~fs ~cache_root ~os_key
                ~ocaml_version:ep.ocaml_version ~built_at ~nodes_by_hash d10 pp)
            ep.packages)
    s.groups

(* Probe the registry index once and figure out which layers we need to fetch
   and how many bytes that totals. *)
let plan_remote_fetches ~d10 ~session ~layer_remote (merged : D10ir.Plan.t) =
  let merged_layer_index =
    match layer_remote with
    | None -> None
    | Some r -> Some (r, D10.Remote_index.fetch d10 ~session ~remote:r)
  in
  let needed_fetches, needed_fetch_bytes =
    match merged_layer_index with
    | None -> ([], 0L)
    | Some (_, index) ->
        List.fold_left
          (fun (hs, bytes) (n : D10ir.Plan.node) ->
            let h = D10ir.Layer_hash.to_string n.layer_hash in
            match Hashtbl.find_opt index h with
            | Some (e : D10.Layer.index_entry)
              when not (D10.Layer.succeeded d10 ~hash:h) ->
                (h :: hs, Int64.add bytes e.size)
            | _ -> (hs, bytes))
          ([], 0L) merged.nodes
  in
  (merged_layer_index, needed_fetches, needed_fetch_bytes)

let d10_cfg_of_env ~env ~cache_root : D10.Config.t =
  {
    sys = env.sys;
    fs = env.fs;
    clock = (env.clock :> D10.Config.clk);
    root = Eio.Path.(env.fs / cache_root);
    os_key = env.os_key;
  }

let direct_cfg_with_jobs ~jobs =
  let base = D10ir.Config.default in
  let base = D10ir.Config.with_env_overrides base in
  {
    base with
    build_parallelism =
      (match jobs with Some j -> j | None -> base.build_parallelism);
  }

(* Forward d10ir's [Direct] events to the outer [Build_progress.reporter],
   and (when [stream] is set) PUT each freshly built layer to S3 as soon
   as its [Node_built] event fires — instead of accumulating hashes for a
   trailing batch upload. The streamed upload covers the .tar.zst, the
   patched JSON sidecar, and the per-package build log. *)
type stream_ctx = {
  env : env;
  cache_root : string;
  d10_cfg : D10.Config.t;
  url_base : string;
  staging : string;
  pp_by_hash : (string, Plan.package_plan * string) Hashtbl.t;
  nodes_by_hash : (string, D10ir.Plan.node) Hashtbl.t;
  built_at : float;
}

let stream_upload_built_layer (ctx : stream_ctx) ~hash ~log_path =
  let os_key = ctx.env.os_key in
  (match Hashtbl.find_opt ctx.pp_by_hash hash with
  | None -> ()
  | Some (pp, ocaml_version) ->
      write_layer_manifest_for_package ~fs:ctx.env.fs ~cache_root:ctx.cache_root
        ~os_key ~ocaml_version ~built_at:ctx.built_at
        ~nodes_by_hash:ctx.nodes_by_hash ctx.d10_cfg pp);
  (match log_path with
  | Some lp ->
      patch_layer_manifest_log ~fs:ctx.env.fs ~cache_root:ctx.cache_root ~os_key
        ~hash ~log_path:lp
  | None -> ());
  upload_one_layer ~fs:ctx.env.fs ~sys:ctx.env.sys ~d10:ctx.d10_cfg
    ~staging:ctx.staging ~cache_root:ctx.cache_root ~url_base:ctx.url_base hash

(* Failure counterpart: write the failure sidecar (no [.tar.zst] —
   the build never produced a layer) with the build-log tail folded
   into [failure.log], then PUT just the [<hash>.json].
   [upload_one_layer] already tolerates the missing tarball (it skips
   the [.tar.zst] PUT when the staged file is absent), so the registry
   ends up with a queryable record of the failure for the UI. *)
let stream_upload_failed_layer (ctx : stream_ctx) ~hash ~phase ~log_path
    ~duration_s =
  match Hashtbl.find_opt ctx.pp_by_hash hash with
  | None -> ()
  | Some (pp, ocaml_version) ->
      let log = log_tail_of_file ~path:log_path in
      write_layer_failure_manifest ~fs:ctx.env.fs ~cache_root:ctx.cache_root
        ~os_key:ctx.env.os_key ~ocaml_version ~built_at:ctx.built_at
        ~nodes_by_hash:ctx.nodes_by_hash ~phase ~duration_s ~log ctx.d10_cfg pp;
      put_layer_json ~sys:ctx.env.sys ~url_base:ctx.url_base
        ~cache_root:ctx.cache_root ~os_key:ctx.env.os_key ~hash

(* A node the scheduler never ran because an upstream dependency failed.
   It still has a real layer hash + package, so publish a sidecar so the
   UI shows it as dep-failed (with the scheduler's reason as the "log")
   rather than silently missing. *)
let stream_upload_skipped_layer (ctx : stream_ctx) ~hash ~reason =
  match Hashtbl.find_opt ctx.pp_by_hash hash with
  | None -> ()
  | Some (pp, ocaml_version) ->
      let log : Manifest_layer.log_tail option =
        Some { lines = 1; truncated = false; text = "skipped: " ^ reason }
      in
      write_layer_failure_manifest ~fs:ctx.env.fs ~cache_root:ctx.cache_root
        ~os_key:ctx.env.os_key ~ocaml_version ~built_at:ctx.built_at
        ~nodes_by_hash:ctx.nodes_by_hash ~phase:"dep_failed" ~duration_s:0. ~log
        ctx.d10_cfg pp;
      put_layer_json ~sys:ctx.env.sys ~url_base:ctx.url_base
        ~cache_root:ctx.cache_root ~os_key:ctx.env.os_key ~hash

let direct_reporter_tracking ~reporter ?stream () : D10ir.Direct.reporter =
  {
    event =
      (fun e ->
        (match (e, stream) with
        | D10ir.Direct.Node_built { node; log_path; _ }, Some ctx ->
            let hash = D10ir.Layer_hash.to_string node.layer_hash in
            stream_upload_built_layer ctx ~hash ~log_path:(Some log_path)
        | ( D10ir.Direct.Node_failed { node; phase; log_path; duration_s; _ },
            Some ctx ) ->
            let hash = D10ir.Layer_hash.to_string node.layer_hash in
            stream_upload_failed_layer ctx ~hash
              ~phase:(D10ir.Direct.string_of_phase phase)
              ~log_path ~duration_s
        | D10ir.Direct.Node_skipped { node; reason }, Some ctx ->
            let hash = D10ir.Layer_hash.to_string node.layer_hash in
            stream_upload_skipped_layer ctx ~hash ~reason
        | _ -> ());
        reporter.Build_progress.event (Build e));
  }

(* Pull every needed remote layer in one go via the unified fetcher. *)
let fetch_needed_layers ~env ~reporter ~jobs ~d10_cfg (merged : D10ir.Plan.t)
    merged_layer_index needed_fetches =
  match merged_layer_index with
  | None -> ()
  | Some (r, index) ->
      if needed_fetches <> [] then (
        let pkg_of : (string, string) Hashtbl.t =
          Hashtbl.create (List.length merged.nodes)
        in
        List.iter
          (fun (n : D10ir.Plan.node) ->
            let h = D10ir.Layer_hash.to_string n.layer_hash in
            let label = Fmt.str "%s.%s" n.package.name n.package.version in
            Hashtbl.replace pkg_of h label)
          merged.nodes;
        Pipeline.fetch_layer_hashes ~reporter ?jobs ~session:env.http_session
          ~remote:r ~d10:d10_cfg ~index ~hashes:needed_fetches ~pkg_of ())

let archive_path_for ~cache_root sha =
  Filename.concat cache_root "d10ir" |> fun p ->
  Filename.concat p "archives" |> fun p -> Filename.concat p (sha ^ ".tar.zst")

let needs_archive ~d10_cfg ~cache_root (n : D10ir.Plan.node) =
  let h = D10ir.Layer_hash.to_string n.layer_hash in
  (not (D10.Layer.succeeded d10_cfg ~hash:h))
  && not (Sys.file_exists (archive_path_for ~cache_root n.archive.sha256))

let pkg_archive_summary nodes =
  List.map
    (fun (n : D10ir.Plan.node) ->
      Fmt.str "%s.%s (%s)" n.package.name n.package.version
        (String.sub n.archive.sha256 0
           (min 12 (String.length n.archive.sha256))))
    nodes
  |> String.concat ", "

(* Archive prefetch from the [d10ir-archives/] tree on the remote (if any),
   then a hard-error for anything still missing locally. Every source
   archive must come from a [oi repo bump] pass — there's no inline-bake
   fallback. *)
let ensure_archives_local ~env ~cache_root ~source_remote ~d10_cfg
    (merged : D10ir.Plan.t) =
  let missing_locally =
    List.filter (needs_archive ~d10_cfg ~cache_root) merged.nodes
  in
  if missing_locally = [] then ()
  else begin
    (match source_remote with
    | Some (`Http_remote registry) ->
        let shas =
          List.map
            (fun (n : D10ir.Plan.node) -> n.archive.sha256)
            missing_locally
        in
        let _ : D10ir.Registry.prefetch_summary =
          D10ir.Registry.prefetch
            ~clock:(env.clock :> _ Eio.Time.clock_ty Eio.Resource.t)
            ~fs:env.fs ~session:env.http_session ~cache_root
            ~remote:(`Http_remote registry) shas
        in
        ()
    | None -> ());
    let still_missing =
      List.filter (needs_archive ~d10_cfg ~cache_root) missing_locally
    in
    if still_missing <> [] then
      Error.fail_config_error
        "%d source archive(s) missing locally and not on the registry: %s.\n\
         Run [oi repo bump] on the offending overlay to bake the archives, or \
         point [--registry] at a registry that publishes \
         [d10ir-archives/<sha>.tar.zst] for these shas."
        (List.length still_missing)
        (pkg_archive_summary still_missing)
  end

(* Pre-flight validation: hoists [oi ir lint]'s plan check so [oi run] /
   [oi build] surface plan-structure bugs (e.g. a dep_layer_hash with no
   producer and no d10 entry) as a clear error instead of letting [Direct.run]
   cascade-skip every node. *)
let validate_plan_or_fail ~d10_cfg ~fs ~plan_dir merged =
  match D10ir.Plan.validate ~d10:d10_cfg ~fs ~plan_dir merged with
  | Ok () -> ()
  | Error err ->
      Error.fail_config_error "d10ir recipe failed validation before build: %a"
        D10ir.Plan.pp_validate_error err

(* Mirror freshly built layers to S3 (or any [s3cmd put]-reachable URL
   prefix) when [--upload-archive=URL] is set. Sequential uploads keep
   chatty output predictable and avoid stampeding the remote. *)
(* PUT the d10ir source-archive sidecar (always) and the tarball
   (only when [archive_sources] is true) for one [<sha>]. The sidecar
   is what powers source-to-binary stitching for an indexer; the
   tarball makes the bucket fully self-contained against upstream
   deletion.

   The remote prefix is [d10ir-archives/], NOT the local cache's
   [d10ir/archives/] dir name: that is exactly the relative shape
   {!D10ir.Registry.url_of} / {!D10ir.Registry.sidecar_url_of} fetch
   from ([<base>/d10ir-archives/<sha>.{tar.zst,json}]). Keeping these
   in lock-step is what makes an upload-only bucket usable by the
   source-archive prefetch path. *)
let upload_one_d10ir ~sys ~cache_root ~url_base ~archive_sources sha =
  let archives_dir =
    Filename.concat cache_root (Filename.concat "d10ir" "archives")
  in
  let json_src = Filename.concat archives_dir (sha ^ ".json") in
  let tar_src = Filename.concat archives_dir (sha ^ ".tar.zst") in
  if Sys.file_exists json_src then
    s3_put_quiet ~sys ~src:json_src
      ~dst:(Fmt.str "%sd10ir-archives/%s.json" url_base sha);
  if archive_sources && Sys.file_exists tar_src then
    s3_put_quiet ~sys ~src:tar_src
      ~dst:(Fmt.str "%sd10ir-archives/%s.tar.zst" url_base sha)

let iter_solved_packages (s : solved) f =
  List.iter
    (fun (gr : group_result) ->
      match gr.exec_plan with
      | None -> ()
      | Some (ep : Plan.t) -> List.iter (fun pp -> f ep pp) ep.packages)
    s.groups

let unique_d10ir_shas_of_solved (s : solved) =
  let tbl = Hashtbl.create 64 in
  iter_solved_packages s (fun _ep (pp : Plan.package_plan) ->
      match pp.d10_archive with
      | None -> ()
      | Some sha -> Hashtbl.replace tbl sha ());
  Hashtbl.fold (fun k () acc -> k :: acc) tbl []

(* Capture the reporepo HEAD commit if a clone is present locally.
   Used as the "universe pin" in the build manifest. Best-effort: any
   shell-out failure yields None and the manifest omits the field. *)
let head_commit_of_reporepo ~sys =
  try
    let path = Source_reporepo.env_path () in
    if not (Sys.file_exists (Filename.concat path ".git")) then None
    else
      let out =
        D10.Sysops.Cmd.run_out sys [ "git"; "-C"; path; "rev-parse"; "HEAD" ]
      in
      let s = String.trim out in
      if String.length s = 40 then Some s else None
  with Eio.Io _ | Sys_error _ | Failure _ -> None

let snapshot_reporepo ~sys ~cache_root ~url_base ~commit =
  try
    let path = Source_reporepo.env_path () in
    if not (Sys.file_exists path) then ()
    else
      let staging =
        Filename.concat cache_root (Filename.concat "upload-staging" "reporepo")
      in
      (try Unix.mkdir staging 0o755 with Unix.Unix_error _ -> ());
      let tar_dst = Filename.concat staging (commit ^ ".tar.zst") in
      if not (Sys.file_exists tar_dst) then
        D10.Sysops.Cmd.run sys
          [
            "tar";
            "--zstd";
            "--exclude=.git/objects/pack/tmp_pack_*";
            "-cf";
            tar_dst;
            "-C";
            Filename.dirname path;
            Filename.basename path;
          ];
      s3_put_quiet ~sys ~src:tar_dst
        ~dst:(Fmt.str "%sreporepo/%s.tar.zst" url_base commit)
  with exn ->
    Logs.warn (fun m -> m "snapshot-reporepo: %s" (Printexc.to_string exn))

let url_base_of raw_url =
  if String.length raw_url = 0 || raw_url.[String.length raw_url - 1] = '/' then
    raw_url
  else raw_url ^ "/"

(* Post-build PUT of artefacts that aren't tied to a single d10ir node:
   d10ir source-manifest sidecars (and optional source tarballs) and an
   optional snapshot of the reporepo at solve-time HEAD. Per-layer
   .tar.zst / .json / .log mirroring is no longer done here — those
   stream during the build via {!direct_reporter_tracking}. *)
let upload_post_build_meta ~env ~cache_root ~upload_archive_url ~archive_sources
    ~snapshot_reporepo:do_snapshot solved =
  match upload_archive_url with
  | None -> ()
  | Some raw_url -> (
      let url_base = url_base_of raw_url in
      let d10ir_shas = unique_d10ir_shas_of_solved solved in
      if d10ir_shas <> [] then begin
        Say.step "Publishing %d source manifest(s)%s to %s"
          (List.length d10ir_shas)
          (if archive_sources then " (with tarballs)" else "")
          url_base;
        List.iter
          (upload_one_d10ir ~sys:env.sys ~cache_root ~url_base ~archive_sources)
          d10ir_shas
      end;
      if do_snapshot then
        match head_commit_of_reporepo ~sys:env.sys with
        | None -> ()
        | Some commit ->
            Say.step "Snapshotting reporepo @ %s to %s" (String.sub commit 0 12)
              url_base;
            snapshot_reporepo ~sys:env.sys ~cache_root ~url_base ~commit)

(* PUT a single build manifest JSON. Called from [build] after
   [finalize_build_manifest]. The remote key drops the [layers/] prefix
   so the upload mirrors the export shape: builds at
   [<URL>/<os_key>/builds/<YYYY>/<MM>/...json]. *)
let upload_build_manifest ~env ~cache_root ~url_base ~os_key ~invocation_id
    ~started_at =
  let path =
    Manifest_build.path_for ~cache_root ~os_key ~ts:started_at ~invocation_id
  in
  if Sys.file_exists path then
    let layers_prefix = Filename.concat cache_root "layers/" in
    let n = String.length layers_prefix in
    if String.length path > n && String.sub path 0 n = layers_prefix then
      let rel = String.sub path n (String.length path - n) in
      s3_put_quiet ~sys:env.sys ~src:path ~dst:(Fmt.str "%s%s" url_base rel)

let overlay_pin_of (o : D10.Overlay.t) : Manifest_build.overlay_pin =
  { handle = o.handle; version = o.version; commit = None; url = None }

let unique_overlays_of_solved (s : solved) : Manifest_build.overlay_pin list =
  let tbl = Hashtbl.create 4 in
  iter_solved_packages s (fun _ep (pp : Plan.package_plan) ->
      match pp.overlay with
      | None -> ()
      | Some (o : D10.Overlay.t) ->
          let key = o.handle ^ "@" ^ o.version in
          if not (Hashtbl.mem tbl key) then
            Hashtbl.add tbl key (overlay_pin_of o));
  Hashtbl.fold (fun _ v acc -> v :: acc) tbl []

let resolved_deps_of_solved (s : solved) : Manifest_layer.dep list =
  let tbl = Hashtbl.create 64 in
  iter_solved_packages s (fun _ep (pp : Plan.package_plan) ->
      if not (Hashtbl.mem tbl pp.layer_hash) then
        let name, version = split_pkg pp.pkg in
        Hashtbl.add tbl pp.layer_hash
          { Manifest_layer.name; version; hash = pp.layer_hash });
  Hashtbl.fold (fun _ v acc -> v :: acc) tbl []

let finalize_build_manifest ~env ~started_at ~finished_at ?targets
    (solved : solved) =
  let invocation_id = Audit.invocation_id () in
  let cache_root = Cache.root_s env.cache in
  let os_key = env.os_key in
  let events = Audit.read_staged ~cache_root ~invocation_id in
  let reporepo =
    match head_commit_of_reporepo ~sys:env.sys with
    | None -> None
    | Some commit ->
        Some
          ({
             Manifest_build.url = None;
             commit = Some commit;
             snapshot_key = None;
           }
            : Manifest_build.reporepo)
  in
  let overlays = unique_overlays_of_solved solved in
  let context = Audit.default_context () in
  let solve : Manifest_build.solve option =
    Some
      {
        solve_key = None;
        schema = None;
        from_cache = false;
        resolved = resolved_deps_of_solved solved;
      }
  in
  let m =
    Manifest_build.v ~invocation_id ~os_key ~started_at ~finished_at ?reporepo
      ~overlays ~context ?targets ?solve ~events ()
  in
  Manifest_build.write ~fs:env.fs ~cache_root m;
  Manifest_registry.ensure ~fs:env.fs ~cache_root ~os_key ~wrote_by:"oi";
  Audit.delete_staged ~cache_root ~invocation_id

let mk_stream_ctx ~env ~cache_root ~d10_cfg ~started_at (inp : build_inputs) =
  match inp.upload_archive_url with
  | None -> None
  | Some raw_url ->
      Some
        {
          env;
          cache_root;
          d10_cfg;
          url_base = url_base_of raw_url;
          staging = Filename.concat cache_root "upload-staging";
          pp_by_hash = build_pp_by_hash inp.solved;
          nodes_by_hash = build_nodes_by_hash inp.solved;
          built_at = started_at;
        }

let emit_total_estimate ~reporter ~merged needed_fetches needed_fetch_bytes =
  reporter.Build_progress.event
    (Total_estimate
       {
         fetches = List.length needed_fetches;
         builds = List.length merged.D10ir.Plan.nodes;
         fetch_bytes = needed_fetch_bytes;
       })

(* Tail PUT after the build manifest + registry pointer exist on disk:
   ordered last so a remote indexer that races the upload sees layer
   manifests before their containing build manifest. *)
let upload_build_manifest_and_pointer ~env ~cache_root ~started_at
    (inp : build_inputs) =
  match inp.upload_archive_url with
  | None -> ()
  | Some raw_url ->
      let url_base = url_base_of raw_url in
      let registry_path =
        Manifest_registry.path_for ~cache_root ~os_key:env.os_key
      in
      if Sys.file_exists registry_path then
        s3_put_quiet ~sys:env.sys ~src:registry_path
          ~dst:(Fmt.str "%s%s/registry.json" url_base env.os_key);
      upload_build_manifest ~env ~cache_root ~url_base ~os_key:env.os_key
        ~invocation_id:(Audit.invocation_id ()) ~started_at

(* Solve-time group failures (Solve_failed / Cycle / Elaborate_failed /
   Emit_failed) never reach the d10ir scheduler, so they have no layer
   hash and no Node_* event. Synthesize a stable hash from the group's
   request (so re-runs overwrite the same object) and publish a failure
   sidecar so the UI renders these too. [Empty_after_strip] is a benign
   "nothing to build once toolchain-provided roots are dropped" (e.g.
   `oi run ocaml`), not a failure — deliberately not published. *)
let group_failure_phase_log e : string * Manifest_layer.log_tail option =
  let tail text =
    let n = List.length (String.split_on_char '\n' text) in
    Some { Manifest_layer.lines = n; truncated = false; text }
  in
  match e with
  | Solve_failed { msg; log_path } ->
      let log =
        match log_tail_of_file ~path:log_path with
        | Some _ as l -> l
        | None -> tail msg
      in
      ("solve", log)
  | Cycle cycles ->
      let one c = String.concat " -> " (List.map OpamPackage.to_string c) in
      ("cycle", tail (String.concat "\n" (List.map one cycles)))
  | Elaborate_failed { msg } -> ("elaborate", tail msg)
  | Emit_failed { msg } -> ("emit", tail msg)
  | Empty_after_strip -> ("empty", None)

let upload_group_failures ~env ~cache_root ~url_base (s : solved) =
  List.iter
    (fun (gr : group_result) ->
      match gr.error with
      | Ok () | Error Empty_after_strip -> ()
      | Error e ->
          let g = gr.group in
          let pkg = match g.tokens with t :: _ -> t | [] -> g.label in
          let hash =
            Digest.to_hex
              (Digest.string
                 (env.os_key ^ "\000" ^ g.label ^ "\000"
                 ^ String.concat "," (List.sort compare g.tokens)))
          in
          let phase, log = group_failure_phase_log e in
          let m =
            Manifest_layer.failure ~hash ~os_key:env.os_key ~package:pkg
              ~package_name:pkg ~package_ver:"" ~method_:"" ~phase
              ~duration_s:0. ?log ~deps:[] ~depexts_declared:[] ()
          in
          Manifest_layer.write ~fs:env.fs ~cache_root m;
          put_layer_json ~sys:env.sys ~url_base ~cache_root ~os_key:env.os_key
            ~hash)
    s.groups

(* Non-relocatable toolchain builds (oxcaml today) bake the host's
   [<XDG_CACHE_HOME>/oi/toolchains/<id>] into bytecode shebangs, [findlib.conf],
   stub-library rpaths, etc. The toolchain hash is deterministic across hosts
   but [XDG_CACHE_HOME] is not, so a layer built on one host won't run on
   another even though their layer hashes agree. Force every registry channel
   off for those builds — both directions: no fetch and no upload. Local
   d10-cache reuse still works because the local toolchain prefix matches at
   build time and at consume time. *)
let registry_io_for_inputs ~reporter (inp : build_inputs) =
  match inp.solved.toolchain with
  | Some i when not i.relocatable ->
      if inp.layer_remote <> None || inp.upload_archive_url <> None then
        Fmt.kstr
          (fun s -> reporter.Build_progress.event (Status s))
          "Non-relocatable toolchain %s: layer cache is local-only (registry \
           fetch + upload disabled)"
          i.handle;
      (None, None)
  | _ -> (inp.layer_remote, inp.upload_archive_url)

let build env ?(reporter = Build_progress.null) (inp : build_inputs) :
    D10ir.Direct.result option =
  let started_at = Unix.gettimeofday () in
  let cache_root = Cache.root_s env.cache in
  let layer_remote, upload_archive_url = registry_io_for_inputs ~reporter inp in
  let inp = { inp with layer_remote; upload_archive_url } in
  (match inp.upload_archive_url with
  | None -> ()
  | Some raw_url ->
      (* Publish solve/cycle/elaborate/emit failures even when the whole
         solve produced no plan ([merged = None] → early return below),
         so a fully-failed run still lands in the registry UI. *)
      upload_group_failures ~env ~cache_root ~url_base:(url_base_of raw_url)
        inp.solved);
  match inp.solved.merged with
  | None -> None
  | Some (merged : D10ir.Plan.t) ->
      let d10 =
        Pipeline.d10 ~sys:env.sys ~fs:env.fs
          ~clock:(env.clock :> D10.Config.clk)
          ~cache:env.cache ~os_key:env.os_key
      in
      let merged_layer_index, needed_fetches, needed_fetch_bytes =
        plan_remote_fetches ~d10 ~session:env.http_session
          ~layer_remote:inp.layer_remote merged
      in
      emit_total_estimate ~reporter ~merged needed_fetches needed_fetch_bytes;
      let d10_cfg = d10_cfg_of_env ~env ~cache_root in
      let direct_cfg = direct_cfg_with_jobs ~jobs:inp.jobs in
      let plan_dir = Eio.Path.native_exn d10_cfg.root in
      let stream = mk_stream_ctx ~env ~cache_root ~d10_cfg ~started_at inp in
      let direct_reporter = direct_reporter_tracking ~reporter ?stream () in
      reporter.event
        (Phase_started { phase = Fetching; label = "build inputs" });
      fetch_needed_layers ~env ~reporter ~jobs:inp.jobs ~d10_cfg merged
        merged_layer_index needed_fetches;
      ensure_archives_local ~env ~cache_root ~source_remote:inp.source_remote
        ~d10_cfg merged;
      reporter.event (Plan_ready merged);
      reporter.event (Phase_started { phase = Building; label = "build" });
      validate_plan_or_fail ~d10_cfg ~fs:env.fs ~plan_dir merged;
      let result =
        D10ir.Direct.run ~config:direct_cfg ~d10:d10_cfg ~fs:env.fs
          ~proc_mgr:env.proc_mgr
          ~clock:(env.clock :> D10.Config.clk)
          ~reporter:direct_reporter ~plan_dir ?install_to:inp.install_to merged
      in
      write_provenance_for_solved ~fs:env.fs ~cache_root ~os_key:env.os_key
        ~d10:d10_cfg inp.solved;
      upload_post_build_meta ~env ~cache_root
        ~upload_archive_url:inp.upload_archive_url
        ~archive_sources:inp.archive_sources
        ~snapshot_reporepo:inp.snapshot_reporepo inp.solved;
      reporter.event (Build_summary result);
      let finished_at = Unix.gettimeofday () in
      finalize_build_manifest ~env ~started_at ~finished_at inp.solved;
      upload_build_manifest_and_pointer ~env ~cache_root ~started_at inp;
      Some result

let layer_hashes (s : solved) : string list =
  match
    List.find_opt (fun (gr : group_result) -> Result.is_ok gr.error) s.groups
  with
  | None -> []
  | Some gr -> (
      match gr.exec_plan with
      | None -> []
      | Some p ->
          List.map (fun (pp : Plan.package_plan) -> pp.layer_hash) p.packages)

let pkg_matches_root_name ~wanted (pp : Plan.package_plan) =
  match OpamPackage.of_string_opt pp.pkg with
  | Some p -> List.mem (OpamPackage.Name.to_string (OpamPackage.name p)) wanted
  | None -> false

let root_layer_hashes_of_group (gr : group_result) =
  match gr.exec_plan with
  | None -> []
  | Some (ep : Plan.t) ->
      let wanted =
        List.map OpamPackage.Name.to_string gr.group.names
        |> List.sort_uniq String.compare
      in
      List.filter_map
        (fun (pp : Plan.package_plan) ->
          if pkg_matches_root_name ~wanted pp then Some pp.layer_hash else None)
        ep.packages

let root_layer_hashes (s : solved) : string list =
  List.concat_map root_layer_hashes_of_group s.groups
  |> List.sort_uniq String.compare
