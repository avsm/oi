[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

let log_src = Logs.Src.create "oi.solve"

module Log = (val Logs.src_log log_src : Logs.LOG)
module Solver = Opam_0install.Solver.Make (Dir_context)

let ( / ) = Filename.concat

let load_opam_from packages_dir pkg =
  let name_s = OpamPackage.Name.to_string (OpamPackage.name pkg) in
  let pkg_s = OpamPackage.to_string pkg in
  let path = packages_dir / name_s / pkg_s / "opam" in
  if Sys.file_exists path then
    Some (OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path)))
  else None

let load_opam packages_dirs pkg =
  List.find_map (fun d -> load_opam_from d pkg) packages_dirs

(* Env function matching day10's [bin/util.ml] std_env. Provides
   platform variables plus [ocaml:version] and [ocaml:native], and warns
   for anything unrecognised. Shared between the solver (via Dir_context)
   and [dep_names] so both sides resolve variables identically — that is
   load-bearing for [layer_hash] agreement with day10. *)
let std_env ?(ocaml_native = true) ?opam_version (conf : Opam_ctx.conf) v =
  match v with
  | "arch" -> Some (OpamTypes.S conf.arch)
  | "os" -> Some (OpamTypes.S conf.os)
  | "os-distribution" -> Some (OpamTypes.S conf.os_distribution)
  | "os-version" -> Some (OpamTypes.S conf.os_version)
  | "os-family" -> Some (OpamTypes.S conf.os_family)
  | "opam-version" ->
      Some
        (OpamVariable.S
           (Stdlib.Option.value
              ~default:OpamVersion.(to_string current)
              opam_version))
  (* No host compiler is visible at plan time — the toolchain is
     assembled from cached layers. Matches day10. *)
  | "sys-ocaml-arch" | "sys-ocaml-cc" | "sys-ocaml-libc" | "sys-ocaml-system"
  | "sys-ocaml-version" ->
      Some (OpamTypes.S "")
  | "ocaml:native" -> Some (OpamTypes.B ocaml_native)
  | "ocaml:version" -> Some (OpamTypes.S conf.ocaml_version)
  | "enable-ocaml-beta-repository" -> None
  | v ->
      OpamConsole.warning "Unknown variable %S" v;
      None

let ctx_env ctx = std_env (Opam_ctx.conf ctx)

let filter_env (conf : Opam_ctx.conf) v =
  std_env conf (OpamVariable.Full.to_string v)

(** Compute direct dependency names of [pkg] that are in [in_solution],
    including depopts that appear in the solution. *)
let dep_names ~packages_dirs ctx pkg in_solution =
  let env v =
    if List.mem v OpamPackageVar.predefined_depends_variables then None
    else
      match OpamVariable.Full.to_string v with
      | "version" ->
          Some
            (OpamTypes.S
               (OpamPackage.Version.to_string (OpamPackage.version pkg)))
      | x -> ctx_env ctx x
  in
  match load_opam packages_dirs pkg with
  | None -> OpamPackage.Name.Set.empty
  | Some opam ->
      let names_from_formula f =
        let atoms = OpamFormula.atoms f in
        List.fold_left
          (fun s (name, _) ->
            if OpamPackage.Name.Set.mem name in_solution then
              OpamPackage.Name.Set.add name s
            else s)
          OpamPackage.Name.Set.empty atoms
      in
      let deps =
        OpamFile.OPAM.depends opam
        |> OpamFilter.partial_filter_formula env
        |> OpamFilter.filter_deps ~build:true ~post:false ~test:false ~doc:false
             ~dev_setup:false ~dev:false ~default:false
        |> names_from_formula
      in
      let depopts =
        OpamFormula.fold_left
          (fun s (name, _) ->
            if OpamPackage.Name.Set.mem name in_solution then
              OpamPackage.Name.Set.add name s
            else s)
          OpamPackage.Name.Set.empty
          (OpamFile.OPAM.depopts opam)
      in
      OpamPackage.Name.Set.union deps depopts

(* DFS topological sort. Cycles are broken by the visited set. *)
let topo_sort ~packages_dirs ctx pkgs =
  let in_solution =
    List.fold_left
      (fun s p -> OpamPackage.Name.Set.add (OpamPackage.name p) s)
      OpamPackage.Name.Set.empty pkgs
  in
  let pkg_by_name =
    List.fold_left
      (fun m p -> OpamPackage.Name.Map.add (OpamPackage.name p) p m)
      OpamPackage.Name.Map.empty pkgs
  in
  let dep_map =
    List.fold_left
      (fun m p ->
        OpamPackage.Name.Map.add (OpamPackage.name p)
          (dep_names ~packages_dirs ctx p in_solution)
          m)
      OpamPackage.Name.Map.empty pkgs
  in
  let visited = Hashtbl.create (List.length pkgs) in
  let result = ref [] in
  let rec visit name =
    if not (Hashtbl.mem visited name) then begin
      Hashtbl.replace visited name true;
      OpamPackage.Name.Map.find_opt name dep_map
      |> Stdlib.Option.iter (OpamPackage.Name.Set.iter visit);
      OpamPackage.Name.Map.find_opt name pkg_by_name
      |> Stdlib.Option.iter (fun pkg -> result := pkg :: !result)
    end
  in
  List.iter (fun p -> visit (OpamPackage.name p)) pkgs;
  List.rev !result

let solve_with_dir_context ctx ~packages_dirs ~constraints ~ocaml_version names
    =
  let constraints =
    let add name version_s m =
      let n = OpamPackage.Name.of_string name in
      if OpamPackage.Name.Map.mem n m then m
      else
        let v = OpamPackage.Version.of_string version_s in
        OpamPackage.Name.Map.add n (`Eq, v) m
    in
    constraints |> add "ocaml" ocaml_version
    |> add "ocaml-base-compiler" ocaml_version
    |> add "ocaml-compiler" ocaml_version
  in
  Log.info (fun m ->
      m "Solving for %a (ocaml = %s)"
        Fmt.(list ~sep:comma string)
        (List.map OpamPackage.Name.to_string names)
        ocaml_version);
  let dir_ctx =
    Dir_context.create ~env:(ctx_env ctx) ~constraints packages_dirs
  in
  match Solver.solve dir_ctx names with
  | Ok sels ->
      let pkgs = Solver.packages_of_result sels in
      Log.info (fun m -> m "Solution: %d packages to build" (List.length pkgs));
      Ok (pkgs, packages_dirs)
  | Error diag ->
      let msg = Solver.diagnostics diag in
      Log.debug (fun m -> m "No solution: %s" msg);
      Error msg

(* For each selected package, log the packages_dir that provides it.
   Summarise by dir too, so a reader can confirm at a glance that the
   reporepo-cloned overlays are actually contributing to the solution. *)
let log_package_sources ~packages_dirs pkgs =
  let by_dir = Hashtbl.create 8 in
  List.iter
    (fun pkg ->
      let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
      let full = OpamPackage.to_string pkg in
      let src =
        List.find_opt
          (fun d -> Sys.file_exists (d / name / full / "opam"))
          packages_dirs
      in
      let src_s = Stdlib.Option.value src ~default:"<not found>" in
      Log.debug (fun m -> m "  %s <- %s" full src_s);
      let n = Stdlib.Option.value (Hashtbl.find_opt by_dir src_s) ~default:0 in
      Hashtbl.replace by_dir src_s (n + 1))
    pkgs;
  Log.debug (fun m -> m "packages_dirs contribution to solution:");
  (* Iterate packages_dirs in order but print each dir once, even if
     the same dir appears more than once in the list (which happens
     when a user handle's closure overlaps with the base overlays). *)
  let seen = Hashtbl.create 8 in
  List.iter
    (fun d ->
      if not (Hashtbl.mem seen d) then begin
        Hashtbl.add seen d ();
        match Hashtbl.find_opt by_dir d with
        | Some n -> Log.debug (fun m -> m "  %d <- %s" n d)
        | None -> ()
      end)
    packages_dirs

let solve ~fs ~cache_root ctx ~packages_dirs ~constraints names =
  let conf = Opam_ctx.conf ctx in
  let ocaml_version =
    match String.index_opt conf.ocaml_version '+' with
    | Some i -> String.sub conf.ocaml_version 0 i
    | None -> conf.ocaml_version
  in
  let run_solve () =
    match
      solve_with_dir_context ctx ~packages_dirs ~constraints ~ocaml_version
        names
    with
    | Ok (pkgs, _) ->
        let pkgs = topo_sort ~packages_dirs ctx pkgs in
        log_package_sources ~packages_dirs pkgs;
        Ok pkgs
    | Error _ as e -> e
  in
  (* Consult the persistent solve cache before running 0install. Only
     successful solves are cached; failures always re-run so that a
     fix picked up via a changed HEAD (after [git pull] on an opam
     repository, or a new overlay version) is exercised immediately.
     [key] returns [None] if any [packages_dir] isn't under a git
     working tree — we then fall back to an uncached solve. *)
  match Solve_cache.key ~conf ~packages_dirs ~constraints ~names with
  | None -> run_solve ()
  | Some cache_key -> (
      match Solve_cache.lookup ~cache_root ~key:cache_key with
      | Some pkgs ->
          Log.info (fun m ->
              m "solve cache hit %s (%d packages)"
                (String.sub cache_key 0 12)
                (List.length pkgs));
          Ok pkgs
      | None -> (
          match run_solve () with
          | Ok pkgs as r ->
              Solve_cache.store ~fs ~cache_root ~key:cache_key pkgs;
              r
          | Error _ as e -> e))
