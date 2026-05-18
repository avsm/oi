(* Tests for [oi dist makefile] codegen (Cmd.Makefile_export).

   Builds tiny in-memory {!D10ir.Plan.t} values and asserts the emitted
   Makefile / sidecars: external (toolchain) layers are filtered, the
   transitive dep closure is staged, and a buildable node with no archive
   is a hard error. No solver / network involved. *)

let ( / ) = Filename.concat
let lh = D10ir.Layer_hash.of_string

let read path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  let rec go i =
    if i + nl > sl then false
    else if String.sub s i nl = needle then true
    else go (i + 1)
  in
  nl = 0 || go 0

let node ~name ~hash ~sha ?(deps = []) ?(script = "echo " ^ name) () :
    D10ir.Plan.node =
  {
    package = { name; version = "1.0" };
    layer_hash = lh hash;
    dep_layer_hashes = List.map lh deps;
    archive = { path = name ^ ".tar.zst"; sha256 = sha; strip_components = 1 };
    script;
    env = [ "PATH=/PLAN/bin"; "FOO=bar" ];
    depexts = [];
    prefix = "/PLAN";
    substs = [];
    subst_vars = [ "name=" ^ name ];
    overlay = Some { handle = "default"; version = "" };
    opam_file_sha256 = "";
  }

let plan ~nodes ~roots ~external_layers : D10ir.Plan.t =
  {
    schema_version = D10ir.Plan.current_schema_version;
    os_key = "debian~13~x86_64";
    toolchain = { name = "ocaml-5.4"; base_layer = lh "tc" };
    archive_root = ".";
    nodes;
    roots = List.map lh roots;
    mounts = [];
    external_layers = List.map lh external_layers;
    metadata =
      { oi_version = "test"; generated_at = 0.; cli_invocation = [ "oi" ] };
  }

(* dep -> root, plus an external toolchain layer "tc" that root also
   depends on; "tc" must be filtered out of rules / ROOTS / staging. *)
let sample () =
  let dep = node ~name:"dep" ~hash:"depHASH" ~sha:"aa11" () in
  let root =
    node ~name:"root" ~hash:"rootHASH" ~sha:"bb22" ~deps:[ "depHASH"; "tcHASH" ]
      ~script:"make install" ()
  in
  plan ~nodes:[ dep; root ] ~roots:[ "rootHASH"; "tcHASH" ]
    ~external_layers:[ "tcHASH" ]

let test_emit () =
  let out = Helpers.fresh_dir ~prefix:"mk" () in
  Oi_cmd.Makefile_export.emit (sample ()) ~output:out
    ~registry:"https://example.test" ();
  let mk = read (out / "Makefile") in
  (* External toolchain layer filtered everywhere. *)
  Alcotest.(check bool)
    "ROOTS has root only" true
    (contains ~needle:"ROOTS    := rootHASH\n" mk);
  Alcotest.(check bool)
    "no rule for external tc" false
    (contains ~needle:"/.stamp/layer/tcHASH:" mk);
  Alcotest.(check bool)
    "root rule present" true
    (contains ~needle:"$(SRC)/.stamp/layer/rootHASH: $(SRC)/.stamp/fetch/bb22"
       mk);
  Alcotest.(check bool)
    "scratch dir is src/ not dist/" true
    (contains ~needle:"SRC       := src\n" mk
    && not (contains ~needle:"$(DIST)" mk));
  (* Transitive staging arg list passed to the builder: dep, not tc. *)
  Alcotest.(check bool)
    "root stages dep" true
    (contains ~needle:"./oi-build-node.sh rootHASH bb22 1 depHASH" mk);
  Alcotest.(check bool)
    "registry threaded" true
    (contains ~needle:"REGISTRY ?= https://example.test" mk);
  (* Sidecars. *)
  Alcotest.(check string)
    "root script" "make install"
    (read (out / "recipes" / "rootHASH.sh"));
  Alcotest.(check string)
    "root name" "root"
    (read (out / "recipes" / "rootHASH.name"));
  Alcotest.(check string)
    "root prefix" "/PLAN"
    (read (out / "recipes" / "rootHASH.prefix"));
  (* Helpers emitted and executable. *)
  List.iter
    (fun f ->
      let p = out / f in
      Alcotest.(check bool) (f ^ " exists") true (Sys.file_exists p);
      Alcotest.(check bool)
        (f ^ " executable") true
        ((Unix.stat p).Unix.st_perm land 0o100 <> 0))
    [ "oi-build-node.sh"; "oi-install.sh" ];
  Alcotest.(check bool)
    "plan.json emitted (debug; make ignores it)" true
    (Sys.file_exists (out / "plan.json"))

let test_missing_archive_is_fatal () =
  let bad = node ~name:"bad" ~hash:"badHASH" ~sha:"" () in
  let p = plan ~nodes:[ bad ] ~roots:[ "badHASH" ] ~external_layers:[] in
  let out = Helpers.fresh_dir ~prefix:"mkbad" () in
  Alcotest.check_raises "missing archive raises"
    (Failure
       "oi dist makefile: 1 package(s) have no pre-baked source archive \
        (need x-d10-archive): bad.1.0. Run `oi repo bump` to bake them, or \
        drop them from the target set.") (fun () ->
      Oi_cmd.Makefile_export.emit p ~output:out ~registry:"r" ())

let suite =
  ( "makefile_export",
    [
      Alcotest.test_case "emit" `Quick test_emit;
      Alcotest.test_case "missing archive fatal" `Quick
        test_missing_archive_is_fatal;
    ] )
