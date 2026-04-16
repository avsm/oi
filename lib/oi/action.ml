[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

let log_src = Logs.Src.create "oi.action"

module Log = (val Logs.src_log log_src : Logs.LOG)

type method_ = Source of { is_virtual : bool } | Binary of string

type node = {
  pkg : OpamPackage.t;
  opam : OpamFile.OPAM.t;
  method_ : method_;
  deps : OpamPackage.Name.t list;
}

type t = {
  nodes_by_name : node OpamPackage.Name.Map.t;
  topo_order : OpamPackage.Name.t list;
}

let plan ctx ?d10 ~packages_dirs pkgs =
  let in_solution =
    List.fold_left
      (fun s p -> OpamPackage.Name.Set.add (OpamPackage.name p) s)
      OpamPackage.Name.Set.empty pkgs
  in
  let _installed, nodes_by_name, topo_order =
    List.fold_left
      (fun (installed, nodes, order) pkg ->
        let name = OpamPackage.name pkg in
        let dep_set = Solve.dep_names ~packages_dirs ctx pkg in_solution in
        let deps =
          dep_set |> OpamPackage.Name.Set.elements
          |> List.filter (fun n -> OpamPackage.Name.Set.mem n in_solution)
        in
        let opam =
          match Solve.load_opam packages_dirs pkg with
          | Some o -> o
          | None -> OpamFile.OPAM.create pkg
        in
        let dep_pkgs =
          List.filter_map
            (fun n ->
              OpamPackage.Name.Map.find_opt n nodes
              |> Stdlib.Option.map (fun node -> node.pkg))
            deps
        in
        let method_ =
          match d10 with
          | Some d10 ->
              let hash = D10.Layer.hash ~packages_dirs (pkg :: dep_pkgs) in
              if D10.Layer.succeeded d10 ~hash then Binary hash
              else begin
                let is_virtual =
                  OpamFile.OPAM.build opam = []
                  && OpamFile.OPAM.install opam = []
                  && OpamFile.OPAM.url opam = None
                  && OpamFile.OPAM.extra_sources opam = []
                in
                Source { is_virtual }
              end
          | None ->
              let is_virtual =
                OpamFile.OPAM.build opam = []
                && OpamFile.OPAM.install opam = []
                && OpamFile.OPAM.url opam = None
                && OpamFile.OPAM.extra_sources opam = []
              in
              Source { is_virtual }
        in
        Log.debug (fun m ->
            m "Plan %s: deps=[%s]"
              (OpamPackage.to_string pkg)
              (String.concat ", " (List.map OpamPackage.Name.to_string deps)));
        let node = { pkg; opam; method_; deps } in
        let installed = OpamPackage.Name.Set.add name installed in
        (installed, OpamPackage.Name.Map.add name node nodes, order @ [ name ]))
      (OpamPackage.Name.Set.empty, OpamPackage.Name.Map.empty, [])
      pkgs
  in
  { nodes_by_name; topo_order }

(* -- Accessors ----------------------------------------------------------- *)

let find t name = OpamPackage.Name.Map.find name t.nodes_by_name
let nodes_by_name t = t.nodes_by_name
let topo_order t = t.topo_order
let nodes t = List.map (find t) t.topo_order
let total t = List.length t.topo_order

let roots t =
  List.filter_map
    (fun name ->
      let node = find t name in
      if node.deps = [] then Some node else None)
    t.topo_order

let dependents t name =
  List.filter_map
    (fun n ->
      let node = find t n in
      if List.exists (OpamPackage.Name.equal name) node.deps then Some node
      else None)
    t.topo_order

(* -- Dry-run tree output ------------------------------------------------- *)

(* Compute parallel groups: packages whose deps are all in earlier groups *)
let parallel_groups t =
  let assigned = Hashtbl.create (total t) in
  let groups = ref [] in
  let remaining = ref t.topo_order in
  while !remaining <> [] do
    let ready, rest =
      List.partition
        (fun name ->
          let node = find t name in
          List.for_all (fun dep -> Hashtbl.mem assigned dep) node.deps)
        !remaining
    in
    let ready, rest =
      if ready = [] then
        match rest with [] -> ([], []) | hd :: tl -> ([ hd ], tl)
      else (ready, rest)
    in
    List.iter (fun name -> Hashtbl.replace assigned name true) ready;
    if ready <> [] then groups := ready :: !groups;
    remaining := rest
  done;
  List.rev !groups

let dep_pkgs_of t node =
  List.filter_map
    (fun dep_name ->
      OpamPackage.Name.Map.find_opt dep_name t.nodes_by_name
      |> Stdlib.Option.map (fun n -> n.pkg))
    node.deps

let node_hash ~packages_dirs t node =
  D10.Layer.hash ~packages_dirs (node.pkg :: dep_pkgs_of t node)

let pp_method_short ~remote_has fmt = function
  | Binary _ -> Fmt.pf fmt "%a" Fmt.(styled `Green string) "binary"
  | Source { is_virtual } ->
      if is_virtual then Fmt.pf fmt "%a" Fmt.(styled `Faint string) "virtual"
      else if remote_has then Fmt.pf fmt "%a" Fmt.(styled `Cyan string) "remote"
      else Fmt.pf fmt "%a" Fmt.(styled `Blue string) "source"

let pp_tree ?(remote_has = fun _ -> false) ~packages_dirs fmt t =
  let groups = parallel_groups t in
  let n_groups = List.length groups in
  Fmt.pf fmt "@[<v>";
  List.iteri
    (fun gi names ->
      let is_last_group = gi = n_groups - 1 in
      let cont = if is_last_group then "  " else "│ " in
      let n = List.length names in
      Fmt.pf fmt "%a %a@,"
        Fmt.(styled `Faint string)
        (Fmt.str "stage %d" (gi + 1))
        Fmt.(styled `Faint string)
        (Fmt.str "(%d package%s)" n (if n = 1 then "" else "s"));
      List.iteri
        (fun i name ->
          let node = find t name in
          let pkg_s = OpamPackage.to_string node.pkg in
          let b = if i = n - 1 then "└── " else "├── " in
          let hash = node_hash ~packages_dirs t node in
          Fmt.pf fmt "%s%s%a %a@," cont b
            Fmt.(styled `Bold string)
            pkg_s
            (pp_method_short ~remote_has:(remote_has hash))
            node.method_)
        names)
    groups;
  Fmt.pf fmt "@]"

(* -- Layer hashing ------------------------------------------------------- *)

let layer_hash_for ~packages_dirs t name =
  OpamPackage.Name.Map.find_opt name t.nodes_by_name
  |> Stdlib.Option.map (node_hash ~packages_dirs t)

let layer_hashes ~packages_dirs t =
  List.filter_map
    (fun name ->
      OpamPackage.Name.Map.find_opt name t.nodes_by_name
      |> Stdlib.Option.map (node_hash ~packages_dirs t))
    t.topo_order
