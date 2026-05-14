(* Cmdliner terms for the d10ir-only subcommands (validate, merge, show,
   run). Kept separate from [oi]'s main cmd library so the terms stay
   self-contained — they only need the [d10ir] runtime, not the wider
   oi pipeline. [oi]'s [Ir] module composes these with its own [emit]
   subcommand into the [oi ir] group. *)

open Cmdliner

let ( / ) = Filename.concat

(* -- Shared helpers ------------------------------------------------------ *)

let load_recipe_file path =
  if not (Sys.file_exists path) then Fmt.failwith "no such file: %s" path;
  let s =
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  match D10ir.Plan.of_string s with
  | Ok plan -> plan
  | Error e -> Fmt.failwith "decoding %s: %s" path e

let load_recipe_dir dir =
  let path = dir / "recipe.json" in
  if not (Sys.file_exists path) then Fmt.failwith "no recipe.json in %s" dir;
  load_recipe_file path

(* Default cache root, matching oi's choice. Used by commands that need
   a [D10.Config.t] (validate's [~d10] arg, [run]'s layer store). *)
let default_cache_dir =
  match Sys.getenv_opt "OI_CACHE_DIR" with
  | Some v when v <> "" -> v
  | _ -> (
      match Sys.getenv_opt "XDG_CACHE_HOME" with
      | Some v when v <> "" -> v / "oi"
      | _ -> (
          match Sys.getenv_opt "HOME" with
          | Some h -> h / ".cache" / "oi"
          | None -> "/tmp/oi-cache"))

let cache_dir_term =
  Arg.(
    value
    & opt string default_cache_dir
    & info ~docv:"DIR"
        ~doc:
          "Cache root used to resolve d10 layers and source archives. Defaults \
           to $(b,\\$OI_CACHE_DIR) or $(b,\\$XDG_CACHE_HOME/oi)."
        [ "cache-dir"; "c" ])

let dir_term ~doc =
  Arg.(required & pos 0 (some string) None & info ~docv:"DIR" ~doc [])

let recipe_inputs_term =
  Arg.(
    non_empty
    & pos_left ~rev:true 0 string []
    & info ~docv:"RECIPE" ~doc:"Input d10ir recipe file" [])

let recipe_out_term =
  Arg.(
    required
    & pos ~rev:true 0 (some string) None
    & info ~docv:"OUT" ~doc:"Output path for the merged recipe" [])

(* Eio root used by every command that touches the filesystem. The
   posix backend matches oi's main harness — avoids io_uring on Linux
   so subprocess / signal behaviour is identical everywhere. *)
let with_eio f =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> f ~sw env

let d10 ~sys ~fs ~clock ~cache_dir ~os_key : D10.Config.t =
  { sys; fs; clock; root = Eio.Path.(fs / cache_dir); os_key }

let detect_os_key ~proc_mgr ~fs =
  D10.Os_key.(to_string (of_platform (Osrel.detect ~proc_mgr ~fs)))

(* -- validate ------------------------------------------------------------ *)

let validate_run cache_dir dir =
  with_eio @@ fun ~sw:_ env ->
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let sys = D10.Sysops.v ~proc_mgr ~fs ~net ~clock () in
  let os_key = detect_os_key ~proc_mgr ~fs in
  let d10 = d10 ~sys ~fs ~clock:(clock :> D10.Config.clk) ~cache_dir ~os_key in
  let plan = load_recipe_dir dir in
  match D10ir.Plan.validate ~d10 ~fs ~plan_dir:dir plan with
  | Ok () ->
      Fmt.pr "ok  recipe valid: %d nodes@." (List.length plan.nodes);
      0
  | Error e ->
      Fmt.epr "error  %a@." D10ir.Plan.pp_validate_error e;
      1

let validate_cmd =
  let term =
    Term.(
      const (fun cache_dir d ->
          let code = validate_run cache_dir d in
          if code <> 0 then exit code)
      $ cache_dir_term
      $ dir_term ~doc:"Recipe directory containing $(b,recipe.json)")
  in
  Cmd.v (Cmd.info "validate" ~doc:"Validate a d10ir recipe") term

(* -- merge --------------------------------------------------------------- *)

let merge_run inputs out =
  let plans = List.map load_recipe_file inputs in
  match D10ir.Plan.merge plans with
  | Error msg ->
      Fmt.epr "error  %s@." msg;
      1
  | Ok merged ->
      with_eio @@ fun ~sw:_ env ->
      let fs = Eio.Stdenv.fs env in
      D10ir.Plan.save Eio.Path.(fs / out) merged;
      Fmt.pr "ok  merged %d recipe(s) → %s (%d nodes, %d roots)@."
        (List.length inputs) out (List.length merged.nodes)
        (List.length merged.roots);
      0

let merge_cmd =
  let term =
    Term.(
      const (fun ins out ->
          let code = merge_run ins out in
          if code <> 0 then exit code)
      $ recipe_inputs_term $ recipe_out_term)
  in
  Cmd.v
    (Cmd.info "merge" ~doc:"Merge d10ir recipes into one batched recipe"
       ~man:
         [
           `S Manpage.s_description;
           `P
             "Fold each $(i,RECIPE) into a single batched recipe at $(i,OUT). \
              Nodes are deduplicated by layer hash; roots are unioned.";
           `P
             "Fails if inputs disagree on schema version, $(b,os_key), \
              $(b,toolchain.base_layer), or $(b,archive_root).";
           `S Manpage.s_examples;
           `Pre "  d10ir merge a.d10ir.json b.d10ir.json -o merged.d10ir.json";
         ])
    term

(* -- show ---------------------------------------------------------------- *)

let short_hash h =
  let s = D10ir.Layer_hash.to_string h in
  if String.length s > 12 then String.sub s 0 12 else s

(* Indented unicode tree of layer-hash dependencies. First occurrence of
   each hash expands; later occurrences render as a dim back-reference
   (the leading [\u{21B0}] arrow) with no children, so the tree stays
   small even with shared deps. *)
let branch_str ~is_root ~is_last =
  if is_root then "" else if is_last then "└── " else "├── "

let node_label ~repeated (n : D10ir.Plan.node) =
  if repeated then Fmt.str "\u{21B0} %s.%s" n.package.name n.package.version
  else
    Fmt.str "%s.%s  %s" n.package.name n.package.version
      (short_hash n.layer_hash)

let walk_kids ~parent_indent ~is_root ~is_last ~walk kids =
  let n_kids = List.length kids in
  let child_indent =
    if is_root then "" else parent_indent ^ if is_last then "    " else "│   "
  in
  List.iteri
    (fun i child ->
      walk ~parent_indent:child_indent
        ~is_last:(i = n_kids - 1)
        ~is_root:false child)
    kids

let render_dep_tree (plan : D10ir.Plan.t) =
  let producers = D10ir.Plan.producers_table plan in
  let expanded : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  let kids_of (n : D10ir.Plan.node) =
    List.filter_map (fun h -> Hashtbl.find_opt producers h) n.dep_layer_hashes
  in
  let rec walk ~parent_indent ~is_last ~is_root (n : D10ir.Plan.node) =
    let key = D10ir.Layer_hash.to_string n.layer_hash in
    let repeated = Hashtbl.mem expanded key in
    let branch = branch_str ~is_root ~is_last in
    let label = node_label ~repeated n in
    Fmt.pr "%s%s%s@." parent_indent branch label;
    if not repeated then begin
      Hashtbl.add expanded key ();
      walk_kids ~parent_indent ~is_root ~is_last ~walk (kids_of n)
    end
  in
  let roots =
    List.filter_map (fun h -> Hashtbl.find_opt producers h) plan.roots
  in
  let render_roots rs =
    let n_roots = List.length rs in
    let render_one i r =
      walk ~parent_indent:"" ~is_last:true ~is_root:true r;
      if i < n_roots - 1 then Fmt.pr "@."
    in
    List.iteri render_one rs
  in
  match roots with [] -> Fmt.pr "  (no roots)@." | rs -> render_roots rs

let render_mount (m : D10ir.Plan.mount) =
  let mode = match m.mode with `Ro -> "ro" | `Rw -> "rw" in
  Fmt.pr "  %s [%s]@.    source : %s@.    target : %s@." m.name mode m.source
    m.target;
  if m.env <> [] then begin
    Fmt.pr "    env    :@.";
    List.iter (fun e -> Fmt.pr "      %s@." e) m.env
  end

let render_node (n : D10ir.Plan.node) =
  Fmt.pr "  %s.%s @.    layer    : %s@.    deps     : %d@." n.package.name
    n.package.version (short_hash n.layer_hash)
    (List.length n.dep_layer_hashes);
  Fmt.pr "    archive  : %s (sha=%s..)@." n.archive.path
    (String.sub n.archive.sha256 0 8)

let render_recipe_header (plan : D10ir.Plan.t) =
  Fmt.pr "Recipe@.@.";
  Fmt.pr "  schema_version : %d@." plan.schema_version;
  Fmt.pr "  os_key         : %s@." plan.os_key;
  Fmt.pr "  toolchain      : %s (%s)@." plan.toolchain.name
    (short_hash plan.toolchain.base_layer);
  Fmt.pr "  archive_root   : %s@." plan.archive_root;
  Fmt.pr "  nodes          : %d@." (List.length plan.nodes);
  Fmt.pr "  roots          : %d@." (List.length plan.roots);
  Fmt.pr "  mounts         : %d@." (List.length plan.mounts)

let render_mounts_section mounts =
  if mounts <> [] then begin
    Fmt.pr "@.Mounts@.@.";
    List.iter render_mount mounts
  end

let render_recipe_summary (plan : D10ir.Plan.t) =
  render_recipe_header plan;
  render_mounts_section plan.mounts;
  Fmt.pr "@.Nodes@.@.";
  List.iter render_node plan.nodes

let show_run dir plan_view =
  let plan = load_recipe_dir dir in
  if plan_view then render_recipe_summary plan
  else begin
    Fmt.pr "Layer dependency tree@.@.";
    render_dep_tree plan;
    Fmt.pr
      "@.%d nodes, %d root(s); \u{21B0} marks a back-reference to a layer \
       expanded earlier in the tree@."
      (List.length plan.nodes) (List.length plan.roots)
  end;
  0

let show_cmd =
  let plan_view =
    Arg.(
      value & flag
      & info
          ~doc:
            "Dump full recipe details (schema, mounts, every node) instead of \
             the dependency tree."
          [ "plan"; "details" ])
  in
  let term =
    Term.(
      const (fun d p ->
          let code = show_run d p in
          if code <> 0 then exit code)
      $ dir_term ~doc:"Recipe directory"
      $ plan_view)
  in
  Cmd.v
    (Cmd.info "show"
       ~doc:"Show a d10ir recipe's dependency tree or plan details")
    term

(* -- run ----------------------------------------------------------------- *)

(* Minimal Logs-backed reporter. Each [D10ir.Direct.event] becomes one
   log line; richer UI (progress bars, sparklines) is the caller's job
   if it wants more — this library aims for the [oi]-free baseline. *)
let stderr_reporter : D10ir.Direct.reporter =
  let phase = D10ir.Direct.string_of_phase in
  let pkg (n : D10ir.Plan.node) =
    Fmt.str "%s.%s" n.package.name n.package.version
  in
  let on_event (e : D10ir.Direct.event) =
    match e with
    | Plan_started { total } -> Fmt.epr "[d10ir] plan: %d node(s)@." total
    | Node_started { node } -> Fmt.epr "[d10ir] %s: building@." (pkg node)
    | Node_phase { node; phase = p } ->
        Fmt.epr "[d10ir] %s: %s@." (pkg node) (phase p)
    | Node_cached { node } -> Fmt.epr "[d10ir] %s: cached@." (pkg node)
    | Node_built { node; duration_s; _ } ->
        Fmt.epr "[d10ir] %s: built (%.1fs)@." (pkg node) duration_s
    | Node_failed { node; phase = p; error; _ } ->
        Fmt.epr "[d10ir] %s: FAILED in %s — %s@." (pkg node) (phase p) error
    | Node_skipped { node; reason } ->
        Fmt.epr "[d10ir] %s: skipped (%s)@." (pkg node) reason
    | Plan_done { built; cached; failed; skipped } ->
        Fmt.epr "[d10ir] done: built=%d cached=%d failed=%d skipped=%d@." built
          cached failed skipped
    | Node_queued _ -> ()
  in
  { event = on_event }

let run_run cache_dir dir parallel keep_staging =
  with_eio @@ fun ~sw:_ env ->
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let sys = D10.Sysops.v ~proc_mgr ~fs ~net ~clock () in
  let os_key = detect_os_key ~proc_mgr ~fs in
  let d10 = d10 ~sys ~fs ~clock:(clock :> D10.Config.clk) ~cache_dir ~os_key in
  let plan = load_recipe_dir dir in
  (match D10ir.Plan.validate ~d10 ~fs ~plan_dir:dir plan with
  | Ok () -> ()
  | Error e ->
      Fmt.epr "error  %a@." D10ir.Plan.pp_validate_error e;
      exit 1);
  let cfg =
    let base = D10ir.Config.default in
    let base = D10ir.Config.with_env_overrides base in
    {
      base with
      build_parallelism =
        (match parallel with Some p -> p | None -> base.build_parallelism);
      keep_staging = base.keep_staging || keep_staging;
    }
  in
  let result =
    D10ir.Direct.run ~config:cfg ~d10 ~fs ~proc_mgr
      ~clock:(clock :> D10.Config.clk)
      ~reporter:stderr_reporter ~plan_dir:dir plan
  in
  if result.failed > 0 then
    Fmt.epr "@.%a@.@." D10ir.Direct.pp_failures result.failures;
  Fmt.pr "ok %d  cached %d  failed %d  skipped %d@." result.built result.cached
    result.failed result.skipped;
  if result.failed > 0 then 31 else 0

let run_cmd =
  let parallel =
    Arg.(
      value
      & opt (some int) None
      & info ~docv:"N"
          ~doc:
            "Build $(i,N) layers in parallel; overrides \
             $(b,OI_BUILD_PARALLELISM)."
          [ "j"; "jobs" ])
  in
  let keep =
    Arg.(
      value & flag
      & info ~doc:"Keep per-node staging directories after build (debug)"
          [ "keep-staging" ])
  in
  let term =
    Term.(
      const (fun cache_dir d j k ->
          let code = run_run cache_dir d j k in
          if code <> 0 then exit code)
      $ cache_dir_term
      $ dir_term ~doc:"Recipe directory"
      $ parallel $ keep)
  in
  Cmd.v (Cmd.info "run" ~doc:"Execute a d10ir recipe") term
