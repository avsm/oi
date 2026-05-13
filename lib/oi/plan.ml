[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

let log_src = Logs.Src.create "oi.plan"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

(* -- Action graph -------------------------------------------------------- *)

type node = {
  pkg : OpamPackage.t;
  opam : OpamFile.OPAM.t;
  method_ : Identity.method_;
  deps : OpamPackage.Name.t list;
  layer_hash : string;
}

type graph = {
  nodes_by_name : node OpamPackage.Name.Map.t;
  topo_order : OpamPackage.Name.t list;
}

exception Cycle of OpamPackage.t list list

let pp_cycle ppf cycle =
  Fmt.pf ppf "  %s → %s"
    (String.concat " → " (List.map OpamPackage.to_string cycle))
    (OpamPackage.to_string (List.hd cycle))

let pp_cycles ppf cycles =
  Fmt.pf ppf "@[<v>%a@]" (Fmt.list ~sep:Fmt.cut pp_cycle) cycles

(* Tarjan's strongly-connected-components algorithm restricted to
   non-trivial SCCs — i.e. dependency cycles. A single-node SCC with
   no self-edge is just a normal package and is filtered out. Self-
   edges (a package that lists itself as a dep, however unlikely) are
   reported as a length-1 cycle.

   Each returned inner list is one cycle in discovery order. *)
(* Pop the current SCC off [stack] up to and including [v]. *)
let pop_scc ~stack ~on_stack v =
  let module N = OpamPackage.Name in
  let scc = ref [] in
  let rec pop () =
    let w = Stack.pop stack in
    Hashtbl.replace on_stack w false;
    scc := w :: !scc;
    if N.compare w v <> 0 then pop ()
  in
  pop ();
  !scc

let names_form_cycle ~deps_of names =
  let module N = OpamPackage.Name in
  List.length names > 1
  ||
  match names with
  | [ n ] -> List.exists (fun d -> N.compare d n = 0) (deps_of n)
  | _ -> false

let scc_cycles g =
  let module N = OpamPackage.Name in
  let index : (N.t, int) Hashtbl.t = Hashtbl.create 16 in
  let lowlink : (N.t, int) Hashtbl.t = Hashtbl.create 16 in
  let on_stack : (N.t, bool) Hashtbl.t = Hashtbl.create 16 in
  let stack : N.t Stack.t = Stack.create () in
  let counter = ref 0 in
  let cycles = ref [] in
  let pkg_of n = (N.Map.find n g.nodes_by_name).pkg in
  let deps_of n = (N.Map.find n g.nodes_by_name).deps in
  let rec strongconnect v =
    Hashtbl.replace index v !counter;
    Hashtbl.replace lowlink v !counter;
    incr counter;
    Stack.push v stack;
    Hashtbl.replace on_stack v true;
    List.iter
      (fun w ->
        if not (Hashtbl.mem index w) then begin
          strongconnect w;
          let lv = Hashtbl.find lowlink v in
          let lw = Hashtbl.find lowlink w in
          Hashtbl.replace lowlink v (min lv lw)
        end
        else if Stdlib.Option.value (Hashtbl.find_opt on_stack w) ~default:false
        then begin
          let lv = Hashtbl.find lowlink v in
          let iw = Hashtbl.find index w in
          Hashtbl.replace lowlink v (min lv iw)
        end)
      (deps_of v);
    if Hashtbl.find lowlink v = Hashtbl.find index v then
      let names = pop_scc ~stack ~on_stack v in
      if names_form_cycle ~deps_of names then
        cycles := List.map pkg_of names :: !cycles
  in
  N.Map.iter
    (fun v _ -> if not (Hashtbl.mem index v) then strongconnect v)
    g.nodes_by_name;
  List.rev !cycles

(* Iteratively peel leaves (alphabetical within each level via
   OpamPackage.Map ordering). *)
let peel_level m =
  let installable, remainder =
    OpamPackage.Map.partition
      (fun _ deps -> OpamPackage.Set.is_empty deps)
      m
  in
  if OpamPackage.Map.is_empty installable then ([], remainder)
  else
    let installable_list =
      OpamPackage.Map.bindings installable |> List.map fst
    in
    let remainder =
      OpamPackage.Map.map
        (fun deps ->
          List.fold_left
            (fun acc p -> OpamPackage.Set.remove p acc)
            deps installable_list)
        remainder
    in
    (installable_list, remainder)

let rec topo_sort_dag m =
  if OpamPackage.Map.is_empty m then []
  else
    match peel_level m with
    | [], _ -> []
    | level, remainder -> level @ topo_sort_dag remainder

(* Build the sub-DAG of [pkg]'s transitive closure (keyed by pkg, value =
   transitive-dep set), topo-sort, reverse so the root comes first, then drop
   the root itself. Output order must match day10's or the layer_hash
   diverges. *)
let topo_sorted_dep_pkgs ~nodes ~transitive_deps pkg trans =
  let pkg_of_name n =
    OpamPackage.Name.Map.find_opt n nodes
    |> Stdlib.Option.map (fun node -> node.pkg)
  in
  let names_to_pkg_set s =
    OpamPackage.Name.Set.elements s
    |> List.filter_map pkg_of_name
    |> OpamPackage.Set.of_list
  in
  let dag =
    OpamPackage.Name.Set.fold
      (fun dep_name acc ->
        match pkg_of_name dep_name with
        | None -> acc
        | Some dep_pkg ->
            let dep_trans =
              Hashtbl.find_opt transitive_deps dep_name
              |> Stdlib.Option.value ~default:OpamPackage.Name.Set.empty
            in
            OpamPackage.Map.add dep_pkg (names_to_pkg_set dep_trans) acc)
      trans OpamPackage.Map.empty
  in
  let dag = OpamPackage.Map.add pkg (names_to_pkg_set trans) dag in
  match topo_sort_dag dag |> List.rev with [] -> [] | _ :: rest -> rest

(* Mix in the toolchain hash for non-relocatable toolchains: they bake their
   [install_prefix] into the binaries they produce, so the layer hash must
   include the toolchain identity or stale binaries with dangling shebangs
   would be reused. *)
let layer_hash_with_toolchain ctx base_hash =
  match Solver.Ctx.toolchain ctx with
  | Some tc when not tc.relocatable ->
      Digest.string (base_hash ^ ":" ^ tc.hash) |> Digest.to_hex
  | _ -> base_hash

let method_for ~d10 layer_hash : Identity.method_ =
  match d10 with
  | Some d10 when D10.Layer.succeeded d10 ~hash:layer_hash -> Binary
  | _ -> Source

let extend_transitive_deps ~transitive_deps deps =
  List.fold_left
    (fun acc dep_name ->
      let acc = OpamPackage.Name.Set.add dep_name acc in
      match Hashtbl.find_opt transitive_deps dep_name with
      | Some s -> OpamPackage.Name.Set.union acc s
      | None -> acc)
    OpamPackage.Name.Set.empty deps

let node_of_pkg ctx ?d10 ~packages_dirs ~in_solution ~transitive_deps ~nodes pkg =
  let name = OpamPackage.name pkg in
  let dep_set =
    Solver.direct_deps_within ~packages_dirs ~conf:(Solver.Ctx.conf ctx) pkg
      in_solution
  in
  let deps =
    dep_set |> OpamPackage.Name.Set.elements
    |> List.filter (fun n -> OpamPackage.Name.Set.mem n in_solution)
  in
  let opam =
    Solver.find_opam_file packages_dirs pkg
    |> Stdlib.Option.value ~default:(OpamFile.OPAM.create pkg)
  in
  let trans = extend_transitive_deps ~transitive_deps deps in
  Hashtbl.replace transitive_deps name trans;
  let all_dep_pkgs = topo_sorted_dep_pkgs ~nodes ~transitive_deps pkg trans in
  let layer_hash = D10.Layer.hash ~packages_dirs (pkg :: all_dep_pkgs) in
  let layer_hash = layer_hash_with_toolchain ctx layer_hash in
  let method_ = method_for ~d10 layer_hash in
  Log.debug (fun m ->
      m "Plan %s: deps=[%s]"
        (OpamPackage.to_string pkg)
        (String.concat ", " (List.map OpamPackage.Name.to_string deps)));
  (name, { pkg; opam; method_; deps; layer_hash })

let of_solution ctx ?d10 ~packages_dirs pkgs =
  let in_solution =
    List.fold_left
      (fun s p -> OpamPackage.Name.Set.add (OpamPackage.name p) s)
      OpamPackage.Name.Set.empty pkgs
  in
  let transitive_deps : (OpamPackage.Name.t, OpamPackage.Name.Set.t) Hashtbl.t =
    Hashtbl.create 64
  in
  let _installed, nodes_by_name, topo_order =
    List.fold_left
      (fun (installed, nodes, order) pkg ->
        let name, node =
          node_of_pkg ctx ?d10 ~packages_dirs ~in_solution ~transitive_deps
            ~nodes pkg
        in
        ( OpamPackage.Name.Set.add name installed,
          OpamPackage.Name.Map.add name node nodes,
          order @ [ name ] ))
      (OpamPackage.Name.Set.empty, OpamPackage.Name.Map.empty, [])
      pkgs
  in
  let g = { nodes_by_name; topo_order } in
  (* Detect cycles before returning. opam-0install will solve a cyclic
     dependency set without complaint (it only checks satisfiability), but
     [D10ir.Direct.run] then deadlocks because each package's fiber awaits a
     dep promise that never resolves. *)
  (match scc_cycles g with [] -> () | cycles -> raise (Cycle cycles));
  g

(* -- Graph accessors ----------------------------------------------------- *)

let node_for g name = OpamPackage.Name.Map.find name g.nodes_by_name
let nodes g = List.map (node_for g) g.topo_order

let layer_hashes g =
  List.map (fun name -> (node_for g name).layer_hash) g.topo_order

(* -- Executable plan ----------------------------------------------------- *)

type source_info = { url : string; checksums : string list }
type patch = { file : string; filter : string option }
type subst = string

type package_plan = {
  pkg : string;
  opam : OpamFile.OPAM.t;
  layer_hash : string;
  method_ : Identity.method_;
  dep_layers : Identity.dep list;
  source : source_info option;
  extra_sources : (string * source_info) list;
  extra_files : (string * string) list;
  patches : patch list;
  substs : subst list;
  subst_vars : (string * string) list;
  build_commands : string list list;
  install_commands : string list list;
  install_file : string;
  env : string array;
  build_dir : string;
  prefix : string;
  overlay : D10.Overlay.t option;
  opam_path : string option;
  pkgs_dir : string option;
  depexts : string list;
  d10_archive : string option;
}

type t = {
  packages : package_plan list;
  cache_root : string;
  os_key : string;
  ocaml_version : string;
  build_prefix : string;
  external_layer_hashes : string list;
      (** Layer hashes of toolchain packages whose binaries the host provides (a
          non-relocatable toolchain switch, currently). They are not in
          {!packages} — they are not built by the d10ir executor and have no d10
          layer of their own — but their hashes still appear in every consumer
          node's [dep_layer_hashes] (so consumer layers invalidate when the
          toolchain changes). The d10ir recipe surfaces them via
          {!D10ir.Plan.external_layers} so the executor knows to treat the
          references as satisfied. *)
}

let find_pkg_source_dir ~packages_dirs pkg =
  let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
  let full = OpamPackage.to_string pkg in
  List.find_opt
    (fun d -> Sys.file_exists (d / name / full / "opam"))
    packages_dirs

(* Resolve overlay attribution by deriving an [Origin.t] from the
   [packages/] directory the opam file was found in, then projecting onto
   its [overlay] field. Pins / local trees yield [None]. *)
let overlay_of_pkg ~packages_dirs pkg =
  match find_pkg_source_dir ~packages_dirs pkg with
  | None -> None
  | Some d ->
      let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
      let full = OpamPackage.to_string pkg in
      (Origin.of_packages_dir ~pkgs_dir:d ~name ~full).overlay

(* Resolve a single graph node into a [package_plan]. Walked in
   topological order from {!resolve}; deps' opam state is marked
   "installed" before each package is resolved so its filter env sees
   the in-plan deps as available. *)
let resolve_dep_layers g (node : node) =
  List.filter_map
    (fun dep_name ->
      match OpamPackage.Name.Map.find_opt dep_name g.nodes_by_name with
      | None -> None
      | Some n -> Some (Identity.dep_of_opam n.pkg ~hash:n.layer_hash))
    node.deps

let source_info_of_opam opam =
  OpamFile.OPAM.url opam
  |> Stdlib.Option.map (fun urlf ->
      let url = OpamUrl.to_string (OpamFile.URL.url urlf) in
      let checksums =
        List.map OpamHash.to_string (OpamFile.URL.checksum urlf)
      in
      { url; checksums })

let extra_sources_of_opam opam =
  List.map
    (fun (basename, urlf) ->
      let name = OpamFilename.Base.to_string basename in
      let url = OpamUrl.to_string (OpamFile.URL.url urlf) in
      let checksums =
        List.map OpamHash.to_string (OpamFile.URL.checksum urlf)
      in
      (name, { url; checksums }))
    (OpamFile.OPAM.extra_sources opam)

let extra_file_path ~pkg_source_dir ~pkg pkg_s basename =
  match pkg_source_dir with
  | None -> None
  | Some d ->
      let src =
        d
        / OpamPackage.Name.to_string (OpamPackage.name pkg)
        / pkg_s / "files" / basename
      in
      if Sys.file_exists src then Some (basename, src) else None

let extra_files_of_opam ~packages_dirs pkg pkg_s opam =
  match OpamFile.OPAM.extra_files opam with
  | None -> []
  | Some xs ->
      let pkg_source_dir = find_pkg_source_dir ~packages_dirs pkg in
      List.filter_map
        (fun (base, _hash) ->
          let basename = OpamFilename.Base.to_string base in
          extra_file_path ~pkg_source_dir ~pkg pkg_s basename)
        xs

(* Filter-evaluate per-OS conditional patches against the solver's platform
   env so [package_plan.patches] reflects only the patches that apply on this
   target. Without this, [{os = "macos"}]-only patches would land in Linux
   archives (and vice versa). *)
let resolve_patches ctx opam =
  let pf_env = Solver.Ctx.platform_env ctx in
  List.filter_map
    (fun (base, filter) ->
      let keep =
        match filter with
        | None -> true
        | Some f -> OpamFilter.eval_to_bool ~default:false pf_env f
      in
      if keep then
        let file = OpamFilename.Base.to_string base in
        let filter_str = Stdlib.Option.map OpamFilter.to_string filter in
        Some { file; filter = filter_str }
      else None)
    (OpamFile.OPAM.patches opam)

let install_commands_of ctx opam =
  let env = Solver.Ctx.resolve ctx opam in
  OpamFilter.commands env (OpamFile.OPAM.install opam)
  |> List.filter_map (function [] -> None | cmd -> Some cmd)

(* Active depexts under the solver's filter env: declared [depexts:] entries
   whose filter evaluates true. *)
let active_depexts ctx opam =
  let env = Solver.Ctx.platform_env ctx in
  List.fold_left
    (fun acc (pkgs, filter) ->
      if OpamFilter.eval_to_bool ~default:false env filter then
        OpamSysPkg.Set.union acc pkgs
      else acc)
    OpamSysPkg.Set.empty
    (OpamFile.OPAM.depexts opam)
  |> OpamSysPkg.Set.elements
  |> List.map OpamSysPkg.to_string

let overlay_of_pkgs_dir ~name_s ~pkg_s = function
  | None -> None
  | Some d -> (Origin.of_packages_dir ~pkgs_dir:d ~name:name_s ~full:pkg_s).overlay

let resolve_node ctx ~packages_dirs ~cache_root ~prefix g (node : node) :
    package_plan =
  let pkg = node.pkg in
  let opam = node.opam in
  let pkg_s = OpamPackage.to_string pkg in
  let name_s = OpamPackage.Name.to_string (OpamPackage.name pkg) in
  let layer_hash = node.layer_hash in
  let short_hash =
    String.sub layer_hash 0 (min 12 (String.length layer_hash))
  in
  let build_dir =
    cache_root / "build" / "_build" / (pkg_s ^ "-" ^ short_hash)
  in
  let pkgs_dir = find_pkg_source_dir ~packages_dirs pkg in
  let build_commands =
    Solver.Ctx.resolve_commands ctx ~test:false ~doc:false ~dev_setup:false
      ~build_dir opam
  in
  (* [x-d10-archive] is a bare string (single sha for all OSes) when present.
     Filtered-list / per-OS variants are deferred — until they land, the
     reporepo should produce one opam variant per OS, which is what
     [oi repo bump] already does. *)
  let d10_archive = Keys.read_string_ext Keys.d10_archive opam in
  {
    pkg = pkg_s;
    opam;
    layer_hash;
    method_ = node.method_;
    dep_layers = resolve_dep_layers g node;
    source = source_info_of_opam opam;
    extra_sources = extra_sources_of_opam opam;
    extra_files = extra_files_of_opam ~packages_dirs pkg pkg_s opam;
    patches = resolve_patches ctx opam;
    substs = List.map OpamFilename.Base.to_string (OpamFile.OPAM.substs opam);
    subst_vars = Solver.Ctx.resolve_substs ctx opam;
    build_commands;
    install_commands = install_commands_of ctx opam;
    install_file = build_dir / (name_s ^ ".install");
    env = Solver.Ctx.compilation_env ctx opam;
    build_dir;
    prefix;
    overlay = overlay_of_pkgs_dir ~name_s ~pkg_s pkgs_dir;
    opam_path =
      Stdlib.Option.map (fun d -> d / name_s / pkg_s / "opam") pkgs_dir;
    pkgs_dir;
    depexts = active_depexts ctx opam;
    d10_archive;
  }

let elaborate ctx ~packages_dirs ~cache_root ~os_key ~ocaml_version
    ?build_prefix ?(reporter = Build_progress.null) g =
  let build_prefix =
    match build_prefix with
    | Some p -> p
    | None -> cache_root / "build" / "prefix"
  in
  reporter.Build_progress.event (Status "Elaborating plan");
  (* Non-relocatable toolchains live at a fixed prefix and are NOT
     built/installed into the consumer prefix — drop them from the
     plan so Execute never tries to restore or re-build them. Keeping
     them in [g] (i.e. in the hash graph) is still important for
     layer_hash correctness: consumer layers must change when the
     toolchain does.

     Relocatable toolchains take the opposite path: their compiler IS
     built into the consumer prefix (matching [Solver.Ctx.create]'s
     [mark_installed] skip), so leave their packages in. *)
  let tc_pkgs =
    match Solver.Ctx.toolchain ctx with
    | None -> OpamPackage.Set.empty
    | Some tc when tc.relocatable -> OpamPackage.Set.empty
    | Some tc -> tc.packages
  in
  let is_toolchain_pkg pkg = OpamPackage.Set.mem pkg tc_pkgs in
  (* Walk the topological order. For each node, resolve it into a
     [package_plan] and immediately mark it installed in the solver's
     opam state so subsequent dependents see its provided variables. *)
  let external_layer_hashes = ref [] in
  let process_node (node : node) =
    if is_toolchain_pkg node.pkg then begin
      (* Keep the toolchain packages' layer hashes so the recipe emitter can
         mark them as host-provided. Otherwise [D10ir.Direct.dep_status] would
         flag the missing producer as [`Failed]. *)
      external_layer_hashes := node.layer_hash :: !external_layer_hashes;
      None
    end
    else
      let p =
        resolve_node ctx ~packages_dirs ~cache_root ~prefix:build_prefix g node
      in
      let config = Solver.Ctx.synthetic_config ctx node.pkg node.opam in
      Solver.Ctx.mark_installed ctx node.pkg node.opam config;
      Some p
  in
  let packages = List.filter_map process_node (nodes g) in
  let external_layer_hashes =
    List.sort_uniq String.compare !external_layer_hashes
  in
  {
    packages;
    cache_root;
    os_key;
    ocaml_version;
    build_prefix;
    external_layer_hashes;
  }

(* -- Pretty-printing ----------------------------------------------------- *)

let pp_commands fmt cmds =
  List.iter (fun cmd -> Fmt.pf fmt "    %s@," (String.concat " " cmd)) cmds

let pp_package ~os_key fmt p =
  let short_hash h = String.sub h 0 (min 12 (String.length h)) in
  let method_s =
    match p.method_ with
    | Identity.Source -> "source"
    | Binary -> "binary (cached)"
  in
  Fmt.pf fmt "@[<v>  %a [%s]@," Style.pp_header_string p.pkg method_s;
  Fmt.pf fmt "    layer: %s/%s@," os_key (short_hash p.layer_hash);
  if p.dep_layers <> [] then begin
    Fmt.pf fmt "    needs:@,";
    List.iter
      (fun (d : Identity.dep) ->
        Fmt.pf fmt "      %s → %s/%s@," (Identity.to_string d.id) os_key
          (short_hash d.hash))
      p.dep_layers
  end;
  (match p.source with
  | None -> ()
  | Some s ->
      Fmt.pf fmt "    source: %s@," s.url;
      List.iter (fun c -> Fmt.pf fmt "      %s@," c) s.checksums);
  List.iter
    (fun (name, s) -> Fmt.pf fmt "    extra-source %s: %s@," name s.url)
    p.extra_sources;
  List.iter
    (fun patch ->
      Fmt.pf fmt "    patch: %s%s@," patch.file
        (match patch.filter with None -> "" | Some f -> Fmt.str " {%s}" f))
    p.patches;
  List.iter (fun s -> Fmt.pf fmt "    subst: %s.in@," s) p.substs;
  if p.build_commands <> [] then begin
    Fmt.pf fmt "    build:@,";
    pp_commands fmt p.build_commands
  end;
  if p.install_commands <> [] then begin
    Fmt.pf fmt "    install:@,";
    pp_commands fmt p.install_commands
  end;
  Fmt.pf fmt "    build-dir: %s@," p.build_dir;
  Fmt.pf fmt "    prefix: %s@," p.prefix;
  Fmt.pf fmt "    env: (%d vars)@," (Array.length p.env);
  if p.subst_vars <> [] then begin
    Fmt.pf fmt "    subst-vars:@,";
    List.iter
      (fun (k, v) ->
        let v_short =
          if String.length v > 60 then String.sub v 0 57 ^ "..." else v
        in
        Fmt.pf fmt "      %s=%s@," k v_short)
      p.subst_vars
  end;
  Fmt.pf fmt "@]"

let pp fmt t =
  let n = List.length t.packages in
  Fmt.pf fmt
    "@[<v>Plan: %d packages for %s (ocaml %s)@,Build root: %s/build/@,@," n
    t.os_key t.ocaml_version t.cache_root;
  List.iter
    (fun p ->
      pp_package ~os_key:t.os_key fmt p;
      Fmt.pf fmt "@,")
    t.packages;
  Fmt.pf fmt "@]"
