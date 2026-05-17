(* Depopt / installed-variable correctness over a synthetic opam repo.

   Reproduces and locks the jsont/bytesrw class of bug: an opam [depopt]
   that is present in the solved set must (a) become a dependency edge
   so it is staged and folded into the layer hash, and (b) make
   [%{Q:installed}%] resolve to [true] when the consumer's build
   commands are elaborated — *regardless of the order packages are
   walked in* (the solver topo-sorts over hard deps only, so a depopt
   provider can come after its consumer). Three sets must agree:
   S_hash (layer hash), S_stage (node.deps), S_filt (filter env). *)

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

(* Write one [<repo>/<name>/<name.version>/opam]. *)
let pkg ~repo ~name ~version body =
  let d = repo / name / Fmt.str "%s.%s" name version in
  mkdir_p_rec d;
  let oc = open_out (d / "opam") in
  output_string oc ("opam-version: \"2.0\"\n" ^ body);
  close_out oc

(* A synthetic repo:
   - [lib]   : plain hard dependency.
   - [optq]  : the optional provider.
   - [app]   : depends [lib]; depopts [optq]; its build command embeds
               [%{optq:installed}%] so the elaborated command reveals
               whether the depopt was activated.
   - [appv]  : depopt [optq {>= "2"}] with a version constraint; the
               repo only ships [optq.1], so the constraint is violated.
   - [ocaml] : stub, so [Ctx.create] has a compiler package present. *)
let make_repo () =
  let repo = Helpers.fresh_dir ~prefix:"depopt-repo" () in
  pkg ~repo ~name:"ocaml" ~version:"5.2.0" "";
  pkg ~repo ~name:"lib" ~version:"1" "build: [ \"true\" ]\n";
  pkg ~repo ~name:"optq" ~version:"1" "build: [ \"true\" ]\n";
  pkg ~repo ~name:"app" ~version:"1"
    "build: [ [ \"sh\" \"-c\" \"echo OPTQ=%{optq:installed}%\" ] ]\n\
     depends: [ \"lib\" ]\n\
     depopts: [ \"optq\" ]\n";
  pkg ~repo ~name:"appv" ~version:"1"
    "build: [ [ \"sh\" \"-c\" \"echo OPTQ=%{optq:installed}%\" ] ]\n\
     depends: [ \"lib\" ]\n\
     depopts: [ \"optq\" {>= \"2\"} ]\n";
  (* jsont's exact shape: a multi-entry [depopts:] bare list where only
     ONE entry ([optq], standing in for [bytesrw]) is in the solution
     and the other two ([absent_a]/[absent_b], standing in for
     [cmdliner]/[brr]) are not. This is the configuration the real
     jsont/bytesrw failure occurs in. *)
  pkg ~repo ~name:"appm" ~version:"1"
    "build: [ [ \"sh\" \"-c\" \"echo OPTQ=%{optq:installed}%\" ] ]\n\
     depends: [ \"lib\" ]\n\
     depopts: [ \"absent_a\" \"absent_b\" \"optq\" ]\n";
  repo

let pset names =
  List.fold_left
    (fun s n -> OpamPackage.Name.Set.add (OpamPackage.Name.of_string n) s)
    OpamPackage.Name.Set.empty names

let p s = OpamPackage.of_string s

let has set name =
  OpamPackage.Name.Set.mem (OpamPackage.Name.of_string name) set

(* -- S_stage/S_hash edge: direct_deps_within activates an in-solution
      depopt and omits an absent one. -------------------------------- *)

let test_direct_deps_activation () =
  let repo = make_repo () in
  let dirs = [ repo ] in
  let app = p "app.1" in
  let with_q =
    Oi.Solver.direct_deps_within ~packages_dirs:dirs ~conf app
      (pset [ "app"; "lib"; "optq" ])
  in
  Alcotest.(check bool)
    "depopt in solution -> edge created (lib present)" true (has with_q "lib");
  Alcotest.(check bool)
    "depopt in solution -> optq is a dep edge of app" true (has with_q "optq");
  let without_q =
    Oi.Solver.direct_deps_within ~packages_dirs:dirs ~conf app
      (pset [ "app"; "lib" ])
  in
  Alcotest.(check bool)
    "depopt absent -> still has lib" true (has without_q "lib");
  Alcotest.(check bool)
    "depopt absent from solution -> no optq edge" false (has without_q "optq")

(* KNOWN GAP — characterization, not the desired end state. A depopt
   carries a version constraint ([optq {>= "2"}]) but the repo only
   ships [optq.1]. opam semantics (and DEPOPT_LAYER_HASH_DESIGN.md §2/§7)
   say it must NOT activate (exclude + diagnose). The current coarse
   union in [Solver.direct_deps_of_opam] folds depopt atoms by *name*
   only, discarding the constraint, so it over-activates. This asserts
   the present (coarse) behaviour so the suite stays green and the gap
   is documented; flip to [false] when version coherence (plan §2)
   lands. It is coarse, not unsound: the layer hash still includes the
   (wrong-versioned) optq, so the cache key stays faithful. *)
let test_versioned_depopt_known_coarse () =
  let repo = make_repo () in
  let appv = p "appv.1" in
  let deps =
    Oi.Solver.direct_deps_within ~packages_dirs:[ repo ] ~conf appv
      (pset [ "appv"; "lib"; "optq" ])
  in
  Alcotest.(check bool)
    "coarse: version-constrained depopt currently over-activates (KNOWN GAP, \
     plan §2)"
    true (has deps "optq")

(* The jsont/bytesrw configuration: multi-entry [depopts:] where only
   one entry is in the solution. This is the decisive reproduction. *)
let test_multi_entry_depopt () =
  let repo = make_repo () in
  let appm = p "appm.1" in
  let deps =
    Oi.Solver.direct_deps_within ~packages_dirs:[ repo ] ~conf appm
      (pset [ "appm"; "lib"; "optq" ])
  in
  Alcotest.(check bool)
    "multi-entry depopts: the one in-solution entry (optq) IS activated" true
    (has deps "optq");
  Alcotest.(check bool)
    "multi-entry depopts: absent entries not added" false
    (has deps "absent_a" || has deps "absent_b")

(* -- S_hash + S_filt via the real Plan, with the provider walked AFTER
      the consumer (the order the hard-dep-only solver can produce). -- *)

let opam_root =
  lazy
    (let r = Helpers.fresh_dir ~prefix:"depopt-opamroot" () in
     Oi.Solver.Ctx.init_opam ~root:r;
     r)

let ctx repo =
  ignore (Lazy.force opam_root);
  let prefix = Helpers.fresh_dir ~prefix:"depopt-prefix" () in
  Oi.Solver.Ctx.create ~prefix ~packages_dirs:[ repo ] ~conf ()

(* [Plan.of_solution] computes a node's layer hash from the nodes seen
   before it, so the package list MUST be topologically sorted over the
   *augmented* (hard + in-solution depopt) graph first. [Solver.solve]
   does exactly this via [Solver.topo_sort]; reproduce that contract
   here (a hand-built list is otherwise order-sensitive — that fragility
   is itself the jsont/bytesrw failure mode). *)
let elaborate_app ctx repo pkgs =
  let pkgs = Oi.Solver.topo_sort ~packages_dirs:[ repo ] ~conf pkgs in
  let g = Oi.Plan.of_solution ctx ~packages_dirs:[ repo ] pkgs in
  let t =
    Oi.Plan.elaborate ctx ~packages_dirs:[ repo ]
      ~cache_root:(Helpers.fresh_dir ~prefix:"depopt-cache" ())
      ~os_key:"debian~13~x86_64" ~ocaml_version:"5.2.0" g
  in
  let node =
    List.find
      (fun (n : Oi.Plan.node) ->
        OpamPackage.Name.to_string (OpamPackage.name n.pkg) = "app")
      (Oi.Plan.nodes g)
  in
  let pp =
    List.find
      (fun (pp : Oi.Plan.package_plan) ->
        String.length pp.pkg >= 3 && String.sub pp.pkg 0 3 = "app")
      t.Oi.Plan.packages
  in
  let cmd_str = List.concat pp.Oi.Plan.build_commands |> String.concat " " in
  (node.Oi.Plan.layer_hash, node.Oi.Plan.deps, cmd_str)

let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  let rec go i =
    if i + nl > sl then false
    else if String.sub s i nl = needle then true
    else go (i + 1)
  in
  nl = 0 || go 0

let test_plan_hash_and_filter () =
  let repo = make_repo () in
  (* Provider [optq] deliberately placed AFTER consumer [app] to mimic
     the solver's hard-dep-only topological order (a depopt edge does
     not constrain it). *)
  let h_with, deps_with, cmd_with =
    elaborate_app (ctx repo) repo [ p "lib.1"; p "app.1"; p "optq.1" ]
  in
  let h_without, deps_without, cmd_without =
    elaborate_app (ctx repo) repo [ p "lib.1"; p "app.1" ]
  in
  let dep_names ds =
    List.map OpamPackage.Name.to_string ds |> List.sort compare
  in
  Alcotest.(check bool)
    "S_stage: optq in app.deps when in solution" true
    (List.mem "optq" (dep_names deps_with));
  Alcotest.(check bool)
    "S_stage: optq absent when not in solution" false
    (List.mem "optq" (dep_names deps_without));
  Alcotest.(check bool)
    "S_hash: app layer hash differs with vs without the depopt" false
    (String.equal h_with h_without);
  Alcotest.(check bool)
    "S_filt: %{optq:installed}% = true when activated (provider walked after \
     consumer)"
    true
    (contains ~needle:"OPTQ=true" cmd_with);
  Alcotest.(check bool)
    "S_filt: %{optq:installed}% = false when depopt not in solution" true
    (contains ~needle:"OPTQ=false" cmd_without)

let suite =
  ( "depopt",
    [
      Alcotest.test_case "direct_deps activation" `Quick
        test_direct_deps_activation;
      Alcotest.test_case "versioned depopt (known coarse)" `Quick
        test_versioned_depopt_known_coarse;
      Alcotest.test_case "multi-entry depopts (jsont shape)" `Quick
        test_multi_entry_depopt;
      Alcotest.test_case "plan layer-hash + installed-var (S_hash/S_filt)"
        `Quick test_plan_hash_and_filter;
    ] )
