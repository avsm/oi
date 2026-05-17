(* Tier 2 coverage for [Oi.Origin.of_packages_dir]:
     - reporepo layout (`v2/<handle>/packages/`) classifies as [Reporepo]
       with the right overlay handle and [path_in_repo].
     - the special `v2/reporepo/packages/` layout normalises overlay
       handle to ["reporepo"].
     - pin-set layout (`pins/sets/<id>/packages/`) classifies as [Pin].
     - everything else falls through to [Local].

   No filesystem operations are needed: [of_packages_dir] is purely
   string manipulation on [pkgs_dir]. *)

module O = Oi.Origin

let pp_kind = function
  | O.Reporepo -> "Reporepo"
  | O.Pin -> "Pin"
  | O.Local -> "Local"

let check_kind ?msg expected actual =
  Alcotest.(check string)
    (Stdlib.Option.value msg ~default:"kind")
    (pp_kind expected) (pp_kind actual)

let test_reporepo_handle () =
  let t =
    O.of_packages_dir ~pkgs_dir:"/cache/data/reporepo/v2/avsm/packages"
      ~name:"foo" ~full:"foo.1.0"
  in
  check_kind O.Reporepo t.kind;
  (match t.overlay with
  | Some { handle; _ } -> Alcotest.(check string) "handle is avsm" "avsm" handle
  | None -> Alcotest.fail "overlay is None");
  Alcotest.(check string)
    "path_in_repo points at v2/avsm/packages/foo/foo.1.0/opam"
    "v2/avsm/packages/foo/foo.1.0/opam" t.path_in_repo

let test_reporepo_v2_reporepo () =
  let t =
    O.of_packages_dir ~pkgs_dir:"/cache/data/reporepo/v2/reporepo/packages"
      ~name:"bar" ~full:"bar.2.0"
  in
  check_kind O.Reporepo t.kind;
  (match t.overlay with
  | Some { handle; _ } ->
      Alcotest.(check string) "handle is reporepo" "reporepo" handle
  | None -> Alcotest.fail "overlay is None");
  Alcotest.(check string)
    "path_in_repo points at v2/reporepo/packages/..."
    "v2/reporepo/packages/bar/bar.2.0/opam" t.path_in_repo

let test_pin () =
  let t =
    O.of_packages_dir ~pkgs_dir:"/cache/pins/sets/abcdef/packages" ~name:"foo"
      ~full:"foo.1.0"
  in
  check_kind O.Pin t.kind;
  Alcotest.(check (option string))
    "no overlay for pin" None
    (Option.map (fun (o : D10.Overlay.t) -> o.handle) t.overlay);
  Alcotest.(check string)
    "path_in_repo prefixed with packages/" "packages/foo/foo.1.0/opam"
    t.path_in_repo

let test_local_fallback () =
  let t =
    O.of_packages_dir ~pkgs_dir:"/some/project/packages" ~name:"foo"
      ~full:"foo.1.0"
  in
  check_kind O.Local t.kind;
  Alcotest.(check (option string))
    "no overlay for local" None
    (Option.map (fun (o : D10.Overlay.t) -> o.handle) t.overlay)

let suite =
  ( "origin",
    [
      Alcotest.test_case "reporepo overlay (v2/<handle>/packages)" `Quick
        test_reporepo_handle;
      Alcotest.test_case "reporepo base (v2/reporepo/packages)" `Quick
        test_reporepo_v2_reporepo;
      Alcotest.test_case "pin set (pins/sets/<id>/packages)" `Quick test_pin;
      Alcotest.test_case "local fallback" `Quick test_local_fallback;
    ] )
