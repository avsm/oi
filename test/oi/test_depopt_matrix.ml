(* A wide interaction matrix for opam depopts x installed-variables,
   modelled on real depopt-heavy packages:

   - [fmt]   depopt [cmdliner]     -> [fmt.cli] only if cmdliner present
   - [logs]  depopt [fmt;lwt;js]   -> sublibs gated per provider
   - [reactc] uses a *command filter* [ ... ] {react:installed}
              (filter-gated command inclusion, not just %{} substitution)

   Each case checks the three coupled sets stay consistent:
   S_stage (node.deps), S_hash (layer hash), S_filt (resolved build
   commands), and that results are independent of package list order
   (production runs everything through [Solver.topo_sort] first). *)

let ( / ) = Filename.concat

let rec mkdir_p_rec d =
  if (not (Sys.file_exists d)) && d <> "" && d <> "/" && d <> "." then begin
    mkdir_p_rec (Filename.dirname d);
    try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let conf : Oi.Solver.Ctx.conf =
  {
    arch = "x86_64";
    os = "linux";
    os_distribution = "debian";
    os_version = "13";
    os_family = "debian";
    ocaml_version = "5.2.0";
    jobs = 1;
  }

let pkg ~repo ~name ~version body =
  let d = repo / name / Fmt.str "%s.%s" name version in
  mkdir_p_rec d;
  let oc = open_out (d / "opam") in
  output_string oc ("opam-version: \"2.0\"\n" ^ body);
  close_out oc

let make_repo () =
  let repo = Helpers.fresh_dir ~prefix:"depmx-repo" () in
  pkg ~repo ~name:"ocaml" ~version:"5.2.0" "";
  List.iter
    (fun n -> pkg ~repo ~name:n ~version:"1" "build: [ \"true\" ]\n")
    [ "cmdliner"; "lwt"; "js"; "react"; "base" ];
  (* fmt: single depopt, the canonical jsont/bytesrw shape. *)
  pkg ~repo ~name:"fmt" ~version:"1"
    "build: [ [ \"sh\" \"-c\" \"echo CMDLINER=%{cmdliner:installed}%\" ] ]\n\
     depends: [ \"base\" ]\n\
     depopts: [ \"cmdliner\" ]\n";
  (* logs: multi depopt, sublibs per provider. *)
  pkg ~repo ~name:"logs" ~version:"1"
    "build: [ [ \"sh\" \"-c\" \"echo \
     F=%{fmt:installed}%,L=%{lwt:installed}%,J=%{js:installed}%\" ] ]\n\
     depends: [ \"base\" ]\n\
     depopts: [ \"fmt\" \"lwt\" \"js\" ]\n";
  (* reactc: a build command that is INCLUDED ONLY when react is
     installed (filter-gated command, exercises OpamFilter.commands
     rather than %{} string substitution). *)
  pkg ~repo ~name:"reactc" ~version:"1"
    "build: [\n\
    \  [ \"sh\" \"-c\" \"echo base-step\" ]\n\
    \  [ \"sh\" \"-c\" \"echo REACT-STEP\" ] { react:installed }\n\
     ]\n\
     depends: [ \"base\" ]\n\
     depopts: [ \"react\" ]\n";
  repo

let p s = OpamPackage.of_string s

let pset names =
  List.fold_left
    (fun s n -> OpamPackage.Name.Set.add (OpamPackage.Name.of_string n) s)
    OpamPackage.Name.Set.empty names

let opam_root =
  lazy
    (let r = Helpers.fresh_dir ~prefix:"depmx-root" () in
     Oi.Solver.Ctx.init_opam ~root:r;
     r)

let ctx repo =
  ignore (Lazy.force opam_root);
  Oi.Solver.Ctx.create
    ~prefix:(Helpers.fresh_dir ~prefix:"depmx-prefix" ())
    ~packages_dirs:[ repo ] ~conf ()

(* Topo-sort like [Solver.solve] does, elaborate, return (layer_hash,
   sorted dep names, flattened build command string) for [target]. *)
let plan_for repo target pkgs =
  let pkgs = Oi.Solver.topo_sort ~packages_dirs:[ repo ] ~conf pkgs in
  let c = ctx repo in
  let g = Oi.Plan.of_solution c ~packages_dirs:[ repo ] pkgs in
  let t =
    Oi.Plan.elaborate c ~packages_dirs:[ repo ]
      ~cache_root:(Helpers.fresh_dir ~prefix:"depmx-cache" ())
      ~os_key:"debian~13~x86_64" ~ocaml_version:"5.2.0" g
  in
  let node =
    List.find
      (fun (n : Oi.Plan.node) ->
        OpamPackage.Name.to_string (OpamPackage.name n.pkg) = target)
      (Oi.Plan.nodes g)
  in
  let pp =
    List.find
      (fun (pp : Oi.Plan.package_plan) ->
        let n = String.length target in
        String.length pp.pkg >= n && String.sub pp.pkg 0 n = target)
      t.Oi.Plan.packages
  in
  let cmd = List.concat pp.Oi.Plan.build_commands |> String.concat " " in
  ( node.Oi.Plan.layer_hash,
    List.map OpamPackage.Name.to_string node.Oi.Plan.deps |> List.sort compare,
    cmd )

let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  let rec go i =
    if i + nl > sl then false
    else if String.sub s i nl = needle then true
    else go (i + 1)
  in
  nl = 0 || go 0

let mem x l = List.mem x l

(* -- fmt x cmdliner: single depopt, both polarities + order indep. -- *)
let test_fmt_cmdliner () =
  let repo = make_repo () in
  let h_on, deps_on, cmd_on =
    (* provider deliberately last in the list *)
    plan_for repo "fmt" [ p "base.1"; p "fmt.1"; p "cmdliner.1" ]
  in
  let h_off, deps_off, cmd_off =
    plan_for repo "fmt" [ p "base.1"; p "fmt.1" ]
  in
  Alcotest.(check bool)
    "S_stage on: cmdliner in fmt.deps" true (mem "cmdliner" deps_on);
  Alcotest.(check bool)
    "S_stage off: cmdliner absent" false (mem "cmdliner" deps_off);
  Alcotest.(check bool)
    "S_hash: fmt variant hashes differ" false (String.equal h_on h_off);
  Alcotest.(check bool)
    "S_filt on: CMDLINER=true" true
    (contains ~needle:"CMDLINER=true" cmd_on);
  Alcotest.(check bool)
    "S_filt off: CMDLINER=false" true
    (contains ~needle:"CMDLINER=false" cmd_off);
  (* order independence: shuffle the input list, hashes must match. *)
  let h_on2, _, _ =
    plan_for repo "fmt" [ p "cmdliner.1"; p "fmt.1"; p "base.1" ]
  in
  Alcotest.(check string)
    "S_hash deterministic under input reordering" h_on h_on2

(* -- logs multi-depopt: every subset activates exactly itself. -- *)
let test_logs_subsets () =
  let repo = make_repo () in
  let check label present pkgs =
    let h, deps, cmd = plan_for repo "logs" pkgs in
    List.iter
      (fun (prov, tok) ->
        let want = mem prov present in
        Alcotest.(check bool)
          (Fmt.str "%s: %s edge = %b" label prov want)
          want (mem prov deps);
        Alcotest.(check bool)
          (Fmt.str "%s: %s installed-var = %b" label prov want)
          want (contains ~needle:tok cmd))
      [ ("fmt", "F=true"); ("lwt", "L=true"); ("js", "J=true") ];
    h
  in
  let none = check "none" [] [ p "base.1"; p "logs.1" ] in
  let fmt_only =
    check "fmt-only" [ "fmt" ]
      [ p "base.1"; p "cmdliner.1"; p "fmt.1"; p "logs.1" ]
  in
  let all =
    check "all" [ "fmt"; "lwt"; "js" ]
      [ p "base.1"; p "cmdliner.1"; p "fmt.1"; p "lwt.1"; p "js.1"; p "logs.1" ]
  in
  (* three distinct activation subsets => three distinct layer hashes. *)
  Alcotest.(check bool)
    "logs hashes: none<>fmt_only" false
    (String.equal none fmt_only);
  Alcotest.(check bool)
    "logs hashes: fmt_only<>all" false
    (String.equal fmt_only all);
  Alcotest.(check bool) "logs hashes: none<>all" false (String.equal none all)

(* -- filter-gated command (build step present only when react
      installed): exercises OpamFilter.commands, not %{} substitution. -- *)
let test_react_command_filter () =
  let repo = make_repo () in
  let _, deps_on, cmd_on =
    plan_for repo "reactc" [ p "base.1"; p "reactc.1"; p "react.1" ]
  in
  let _, deps_off, cmd_off =
    plan_for repo "reactc" [ p "base.1"; p "reactc.1" ]
  in
  Alcotest.(check bool)
    "react in deps when in solution" true (mem "react" deps_on);
  Alcotest.(check bool) "react absent otherwise" false (mem "react" deps_off);
  Alcotest.(check bool)
    "filter-gated REACT-STEP present iff react installed" true
    (contains ~needle:"REACT-STEP" cmd_on
    && not (contains ~needle:"REACT-STEP" cmd_off));
  Alcotest.(check bool)
    "base step always present" true
    (contains ~needle:"base-step" cmd_on && contains ~needle:"base-step" cmd_off)

let suite =
  ( "depopt_matrix",
    [
      Alcotest.test_case "fmt x cmdliner (single depopt, order-indep)" `Quick
        test_fmt_cmdliner;
      Alcotest.test_case "logs multi-depopt subsets" `Quick test_logs_subsets;
      Alcotest.test_case "react filter-gated command" `Quick
        test_react_command_filter;
    ] )
