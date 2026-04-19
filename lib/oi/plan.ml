(** Execution plan: resolved build/install commands for all packages. *)

[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

let ( / ) = Filename.concat

type source_info = { url : string; checksums : string list }
type patch = { file : string; filter : string option }
type subst = string
type dep_layer = { pkg : string; hash : string }

type package_plan = {
  pkg : string;
  layer_hash : string;
  method_ : [ `Source | `Binary ];
  dep_layers : dep_layer list;
  source : source_info option;
  extra_sources : (string * source_info) list;
  patches : patch list;
  substs : subst list;
  subst_vars : (string * string) list;
  build_commands : string list list;
  install_commands : string list list;
  install_file : string;
  env : string array;
  build_dir : string;
  prefix : string;
  overlay_handle : string option;
  overlay_version : string option;
}

type group = { stage : int; packages : package_plan list }

type t = {
  groups : group list;
  cache_root : string;
  os_key : string;
  ocaml_version : string;
  total_packages : int;
}

(* Derive [overlay_handle] / [overlay_version] from a [packages/] dir
   that an opam file was sourced from. The reporepo materialiser
   creates [{data_dir}/repos/overlay-<handle>-<version>/packages];
   anything else (pin-depends trees, raw URLs) returns [None]. *)
let overlay_of_packages_dir pkgs_dir =
  let base = Filename.basename (Filename.dirname pkgs_dir) in
  if String.length base > 8 && String.sub base 0 8 = "overlay-" then
    let rest = String.sub base 8 (String.length base - 8) in
    match String.rindex_opt rest '-' with
    | None -> (None, None)
    | Some i ->
        let h = String.sub rest 0 i in
        let v = String.sub rest (i + 1) (String.length rest - i - 1) in
        (Some h, Some v)
  else (None, None)

let find_pkg_source_dir ~packages_dirs pkg =
  let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
  let full = OpamPackage.to_string pkg in
  List.find_opt
    (fun d -> Sys.file_exists (d / name / full / "opam"))
    packages_dirs

let resolve_plan ctx ~packages_dirs ~cache_root ~os_key:_ action_plan =
  let prefix = cache_root / "build" / "prefix" in
  let groups = Action.parallel_groups action_plan in
  (* Track which packages have been "installed" in the synthetic ctx
     so that variables like [ocaml:native] resolve correctly when
     resolving commands for later stages. We mark each stage's packages
     as installed AFTER resolving them (since within a stage they're
     independent of each other). *)
  let mark_stage_installed names =
    List.iter
      (fun name ->
        match
          OpamPackage.Name.Map.find_opt name (Action.nodes_by_name action_plan)
        with
        | None -> ()
        | Some node ->
            let config =
              Opam_ctx.synthetic_config ctx node.Action.pkg node.Action.opam
            in
            Opam_ctx.mark_installed ctx node.Action.pkg node.Action.opam config)
      names
  in
  List.mapi
    (fun i names ->
      let packages =
        List.map
          (fun name ->
            let node = Action.find action_plan name in
            let pkg = node.Action.pkg in
            let opam = node.Action.opam in
            let pkg_s = OpamPackage.to_string pkg in
            let layer_hash = node.Action.layer_hash in
            let method_ =
              match node.Action.method_ with
              | Action.Binary -> `Binary
              | Action.Source -> `Source
            in
            let dep_layers =
              List.filter_map
                (fun dep_name ->
                  match
                    OpamPackage.Name.Map.find_opt dep_name
                      (Action.nodes_by_name action_plan)
                  with
                  | None -> None
                  | Some n ->
                      Some
                        {
                          pkg = OpamPackage.to_string n.Action.pkg;
                          hash = n.Action.layer_hash;
                        })
                node.Action.deps
            in
            let source =
              OpamFile.OPAM.url opam
              |> Stdlib.Option.map (fun urlf ->
                let url = OpamUrl.to_string (OpamFile.URL.url urlf) in
                let checksums =
                  List.map OpamHash.to_string (OpamFile.URL.checksum urlf)
                in
                { url; checksums })
            in
            let extra_sources =
              List.map
                (fun (basename, urlf) ->
                  let name = OpamFilename.Base.to_string basename in
                  let url = OpamUrl.to_string (OpamFile.URL.url urlf) in
                  let checksums =
                    List.map OpamHash.to_string (OpamFile.URL.checksum urlf)
                  in
                  (name, { url; checksums }))
                (OpamFile.OPAM.extra_sources opam)
            in
            let patches =
              List.map
                (fun (base, filter) ->
                  let file = OpamFilename.Base.to_string base in
                  let filter = Stdlib.Option.map OpamFilter.to_string filter in
                  { file; filter })
                (OpamFile.OPAM.patches opam)
            in
            let substs =
              List.map OpamFilename.Base.to_string (OpamFile.OPAM.substs opam)
            in
            let build_commands =
              Opam_ctx.resolve_commands ctx ~test:false ~doc:false
                ~dev_setup:false opam
            in
            let install_commands =
              let env = Opam_ctx.resolve ctx opam in
              OpamFilter.commands env (OpamFile.OPAM.install opam)
              |> List.filter_map (function [] -> None | cmd -> Some cmd)
            in
            let build_dir = cache_root / "build" / "_build" / pkg_s in
            let name_s = OpamPackage.Name.to_string (OpamPackage.name pkg) in
            let install_file = build_dir / (name_s ^ ".install") in
            let env = Opam_ctx.compilation_env ctx opam in
            let subst_vars = Opam_ctx.resolve_substs ctx opam in
            let overlay_handle, overlay_version =
              match find_pkg_source_dir ~packages_dirs pkg with
              | None -> (None, None)
              | Some d -> overlay_of_packages_dir d
            in
            {
              pkg = pkg_s;
              layer_hash;
              method_;
              dep_layers;
              source;
              extra_sources;
              patches;
              substs;
              subst_vars;
              build_commands;
              install_commands;
              install_file;
              env;
              build_dir;
              prefix;
              overlay_handle;
              overlay_version;
            })
          names
      in
      mark_stage_installed names;
      { stage = i + 1; packages })
    groups

let create ctx ~packages_dirs ~cache_root ~os_key ~ocaml_version action_plan =
  let groups =
    resolve_plan ctx ~packages_dirs ~cache_root ~os_key action_plan
  in
  let total_packages =
    List.fold_left (fun acc g -> acc + List.length g.packages) 0 groups
  in
  { groups; cache_root; os_key; ocaml_version; total_packages }

(* -- Pretty-printing ----------------------------------------------------- *)

let pp_commands fmt cmds =
  List.iter (fun cmd -> Fmt.pf fmt "    %s@," (String.concat " " cmd)) cmds

let pp_package ~os_key fmt p =
  let short_hash h = String.sub h 0 (min 12 (String.length h)) in
  let method_s =
    match p.method_ with `Source -> "source" | `Binary -> "binary (cached)"
  in
  Fmt.pf fmt "@[<v>  %a [%s]@," Fmt.(styled `Bold string) p.pkg method_s;
  Fmt.pf fmt "    layer: %s/%s@," os_key (short_hash p.layer_hash);
  if p.dep_layers <> [] then begin
    Fmt.pf fmt "    needs:@,";
    List.iter
      (fun (d : dep_layer) ->
        Fmt.pf fmt "      %s → %s/%s@," d.pkg os_key (short_hash d.hash))
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
  Fmt.pf fmt
    "@[<v>Plan: %d packages for %s (ocaml %s)@,Build root: %s/build/@,@,"
    t.total_packages t.os_key t.ocaml_version t.cache_root;
  List.iter
    (fun g ->
      let n = List.length g.packages in
      Fmt.pf fmt "%a %a@,"
        Fmt.(styled `Faint string)
        (Fmt.str "stage %d" g.stage)
        Fmt.(styled `Faint string)
        (Fmt.str "(%d package%s, parallel)" n (if n = 1 then "" else "s"));
      List.iter (fun p -> pp_package ~os_key:t.os_key fmt p) g.packages;
      Fmt.pf fmt "@,")
    t.groups;
  Fmt.pf fmt "@]"
