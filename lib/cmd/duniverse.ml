open Cmdliner

let ( / ) = Filename.concat

(* -- absolute path / dir helpers ---------------------------------------- *)

let absolutize_output output =
  if Filename.is_relative output then Filename.concat (Sys.getcwd ()) output
  else output

let mkdir_p d =
  let rec go d =
    if Sys.file_exists d then ()
    else begin
      go (Filename.dirname d);
      try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end
  in
  go d

let prepare_output_dirs ~fs output =
  let output = absolutize_output output in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / output);
  let vendor_dir = output / "vendor" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / vendor_dir);
  (output, vendor_dir)

(* -- dune-buildable detection ------------------------------------------- *)

(* A tree is "dune-buildable" if it has a [dune-project] (preferred) or a
   top-level [dune] file. We check the unpacked source tree directly, so
   the detection is independent of any opam-file build commands. *)
let dir_is_dune_buildable dir =
  Sys.file_exists (dir / "dune-project") || Sys.file_exists (dir / "dune")

(* -- @handle/pkg shorthand --------------------------------------------- *)

(* Strip [@handle/pkg] prefixes the same way [oi build] / [oi run] do:
   the handle joins [with_repos] (overlay shows up in the solve), the
   bare [pkg] takes its place in [targets], and the pkg-with-version
   spec joins [with_deps] for constraint propagation. *)
let split_handle_targets ~with_repos ~with_deps targets =
  let targets, with_repos, with_deps =
    List.fold_left
      (fun (ts, repos, deps) t ->
        match Target.split_handle_prefix t with
        | None -> (t :: ts, repos, deps)
        | Some (h, pkg_spec) ->
            let pkg, _ = OpamFormula.atom_of_string pkg_spec in
            ( OpamPackage.Name.to_string pkg :: ts,
              repos @ [ h ],
              deps @ [ pkg_spec ] ))
      ([], with_repos, with_deps)
      targets
  in
  (List.rev targets, with_repos, with_deps)

(* -- partition + layout ------------------------------------------------- *)

(* List the [*.opam] package names at the root of an unpacked tree
   ([atp.opam] → ["atp"]). Used to detect monorepos: when two trees
   declare overlapping opam packages, they're (almost certainly) two
   archive releases of the same source repo, and dune would barf if
   we vendored both. *)
let opam_names_at_root dir =
  match Sys.readdir dir with
  | exception Sys_error _ -> []
  | entries ->
      Array.to_list entries
      |> List.filter_map (fun name ->
          if Filename.check_suffix name ".opam" then
            Some (Filename.chop_suffix name ".opam")
          else None)
      |> List.sort_uniq String.compare

(* For each dune-buildable node in [plan], expose its source tree at
   either [project/<pkg>/] (if the node is a solve root — i.e. one of
   the targets the user asked for) or [vendor/<pkg>/] (otherwise).
   Symlinks point at [sources/<sha>/]; the Makefile (emitted from a
   sub-plan that drops the project trees) still builds the deps from
   there. The split matters because the root [dune] file marks
   [vendor/] as excluded from dune's walk — dune uses src/prefix/lib/
   for vendored deps via OCAMLPATH — while [project/] gets full strict
   dune treatment.

   Dedup by opam-name overlap: if a new tree's root contains any
   [*.opam] file whose name is already owned by another tree (in
   project/ or vendor/), the new tree is a monorepo sibling and we
   skip it — exposing both would have dune barf with [The package "X"
   is defined more than once] (since both archives ship the same
   dune-project listing X). Roots win: pass-1 places every dune-
   buildable root in [project/] and claims its opam names, so a later
   non-root pass-2 sharing the same monorepo tree is correctly elided
   from [vendor/]. *)
type vendor_layout = {
  project_pkgs : D10ir.Plan.package list;
  vendor_pkgs : D10ir.Plan.package list;
  project_owned_names : string list;
      (** opam package names ([atp], [xrpc-auth], …) that live inside a
          [project/<root>/] tree. The Makefile sub-plan drops every node whose
          package name is in this set: those packages are dune- built from
          [project/] by [./build.sh] and don't need a parallel opam-script build
          into [src/prefix/]. *)
}

let layout_project_and_vendor ~output ~(plan : D10ir.Plan.t) =
  let sources_dir = output / "sources" in
  let vendor_dir = output / "vendor" in
  let project_dir = output / "project" in
  let owned = Hashtbl.create 64 in
  let project_owned = Hashtbl.create 64 in
  let project_pkgs = ref [] in
  let vendor_pkgs = ref [] in
  let ext = Hashtbl.create 16 in
  List.iter
    (fun h -> Hashtbl.replace ext (D10ir.Layer_hash.to_string h) ())
    plan.external_layers;
  let roots = Hashtbl.create 16 in
  List.iter
    (fun h -> Hashtbl.replace roots (D10ir.Layer_hash.to_string h) ())
    plan.roots;
  let place ~dst_dir ~kind ~claim_project n =
    let h = D10ir.Layer_hash.to_string n.D10ir.Plan.layer_hash in
    if Hashtbl.mem ext h then `External
    else
      let sha = n.D10ir.Plan.archive.sha256 in
      let sha_dir = sources_dir / sha in
      if not (Sys.file_exists sha_dir && dir_is_dune_buildable sha_dir) then
        `Non_dune
      else
        let opams = opam_names_at_root sha_dir in
        match List.find_opt (fun name -> Hashtbl.mem owned name) opams with
        | Some existing_name ->
            let owning =
              try Hashtbl.find owned existing_name with Not_found -> "?"
            in
            Oi.Say.info "dedup: %s.%s shares source tree with %s (skip)"
              n.D10ir.Plan.package.name n.D10ir.Plan.package.version owning;
            `Dedup
        | None ->
            let pkg_dir = dst_dir / n.D10ir.Plan.package.name in
            mkdir_p dst_dir;
            if not (Sys.file_exists pkg_dir) then begin
              let target = ".." / "sources" / sha in
              try Unix.symlink target pkg_dir
              with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
            end;
            List.iter
              (fun name ->
                Hashtbl.replace owned name
                  (kind ^ "/" ^ n.D10ir.Plan.package.name);
                if claim_project then Hashtbl.replace project_owned name ())
              opams;
            `Placed
  in
  (* Pass 1: roots go to project/. Their opam names are also claimed
     for project_owned, so the makefile sub-plan can drop every node
     that contributes to a project tree (root or monorepo sibling). *)
  List.iter
    (fun (n : D10ir.Plan.node) ->
      let h = D10ir.Layer_hash.to_string n.layer_hash in
      if Hashtbl.mem roots h then
        match
          place ~dst_dir:project_dir ~kind:"project" ~claim_project:true n
        with
        | `Placed -> project_pkgs := n.package :: !project_pkgs
        | _ -> ())
    plan.nodes;
  (* Pass 2: remaining dune-buildable nodes go to vendor/. *)
  List.iter
    (fun (n : D10ir.Plan.node) ->
      let h = D10ir.Layer_hash.to_string n.layer_hash in
      if not (Hashtbl.mem roots h) then
        match
          place ~dst_dir:vendor_dir ~kind:"vendor" ~claim_project:false n
        with
        | `Placed -> vendor_pkgs := n.package :: !vendor_pkgs
        | _ -> ())
    plan.nodes;
  {
    project_pkgs = List.rev !project_pkgs;
    vendor_pkgs = List.rev !vendor_pkgs;
    project_owned_names =
      Hashtbl.fold (fun k () acc -> k :: acc) project_owned [];
  }

(* Build a sub-plan for the Makefile by dropping every node whose
   package name was claimed by a [project/] tree. The Makefile then
   builds compiler + dune + every dep (vendored or not) into
   [src/prefix/]; [./build.sh dune build] picks up from there to build
   the project trees themselves. Roots are reset to "every kept node"
   so [make all] (and the default goal [dest]) drives the full closure
   to completion — without this, the rendered Makefile would target
   the project roots and refuse to build their deps. *)
let makefile_plan ~(plan : D10ir.Plan.t) ~project_owned_names : D10ir.Plan.t =
  let project_names = Hashtbl.create 32 in
  List.iter (fun n -> Hashtbl.replace project_names n ()) project_owned_names;
  let is_project (n : D10ir.Plan.node) =
    Hashtbl.mem project_names n.D10ir.Plan.package.name
  in
  let nodes = List.filter (fun n -> not (is_project n)) plan.nodes in
  let roots = List.map (fun (n : D10ir.Plan.node) -> n.layer_hash) nodes in
  { plan with nodes; roots }

(* -- workspace files ---------------------------------------------------- *)

let dune_project_contents = "(lang dune 3.20)\n"

(* dune walks [project/] (the user's own packages, strict treatment)
   and ignores everything else: [vendor/] (the Makefile already
   installed those deps into src/prefix/lib/, so dune resolves them
   via OCAMLPATH instead of re-building from source); [sources/]
   (canonical unpacked trees — the Makefile reads these); [src/]
   (the Makefile's build scratch dir + layers + prefix). *)
let dune_root_contents = "(dirs (:standard \\ vendor sources src))\n"

(* The build.sh wrapper. Exports PATH / OCAMLPATH / OCAMLFIND_CONF to
   point at the Makefile-built prefix under [src/prefix/], then exec's
   either an explicit command (the args you pass to build.sh) or a
   default [dune build]. Hard-fail with a clear message if the prefix
   doesn't exist (i.e. the user hasn't run [make] yet). *)
let build_sh_contents =
  {sh|#!/bin/sh
# Generated by `oi dist duniverse`. Wraps a command (default: `dune build`)
# with PATH / OCAMLPATH / OCAMLFIND_CONF pointed at the Makefile-built
# toolchain + non-dune deps under src/prefix/. Run `make` first.
set -eu
ROOT="$(cd "$(dirname "$0")" && pwd)"
PREFIX="$ROOT/src/prefix"
if [ ! -d "$PREFIX/bin" ]; then
  echo "build.sh: $PREFIX/bin missing — run 'make prefix' first (or just 'make', which is 'make dest' which depends on prefix)" >&2
  exit 1
fi
export PATH="$PREFIX/bin:$PATH"
export OCAMLPATH="$PREFIX/lib:${OCAMLPATH:-}"
if [ -f "$PREFIX/lib/findlib.conf" ]; then
  export OCAMLFIND_CONF="$PREFIX/lib/findlib.conf"
fi
cd "$ROOT"
if [ "$#" -eq 0 ]; then
  exec dune build
else
  exec "$@"
fi
|sh}

let write_workspace_files ~output =
  let write ?(mode = 0o644) path s =
    Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc s);
    try Unix.chmod path mode with Unix.Unix_error _ -> ()
  in
  write (output / "dune-project") dune_project_contents;
  write (output / "dune") dune_root_contents;
  write ~mode:0o755 (output / "build.sh") build_sh_contents

(* Append the duniverse-specific targets to the Makefile that
   [Makefile_export.emit] just wrote with [~emit_dest_install:false]
   (which suppressed its own [dest:] / [install:] / default-goal so
   nothing here gets overridden — no "overriding recipe" warning from
   make). The extras:
   - [deps:] alias for [all] (just the layer-build closure).
   - [prefix:] assembles src/layers/* into src/prefix/ (build_node.sh
     wipes $PREFIX after each node, so we need an explicit join).
   - [build:] runs [./build.sh dune build @install] over project/.
   - [runtest:] runs [./build.sh dune runtest] over project/.
   - [dest:] (default goal) copies project binaries from
     [project/*/_build/install/default/{bin,sbin}] into dest/, using
     [cp -L] to dereference dune's promote symlinks so dest/bin/<x>
     is a real executable, not a link into a project's _build/.
   - [install:] same but to [$(DESTDIR)$(PREFIX)/{bin,sbin}/]. *)
let duniverse_makefile_suffix =
  {make|
# -- duniverse additions --------------------------------------------------
# Layered on top of the dist-makefile layer-build targets above to
# drive the dune-build phase of project/ from the same `make`
# invocation. The dist-makefile dest/install rules were suppressed
# (via Makefile_export.emit ~emit_dest_install:false) so the targets
# below define the canonical dest/install for the duniverse flow.

.PHONY: deps prefix build runtest dest install

# `make deps` — just the Makefile-built dep closure (every layer
# under src/layers/). Doesn't materialise a usable prefix.
deps: all

# `make prefix` — assemble every layer into a single usable prefix
# at src/prefix/. oi-build-node.sh wipes $PREFIX after each node, so
# this final cp pass is needed before any tool can use it.
prefix: all
	@rm -rf $(SRC)/prefix
	@mkdir -p $(SRC)/prefix
	@for sub in bin sbin lib/stublibs lib/toplevel share man etc doc; do \
	  mkdir -p "$(SRC)/prefix/$$sub"; \
	done
	@for h in $(ROOTS); do \
	  if [ -d "$(SRC)/layers/$$h" ]; then \
	    cp -a "$(SRC)/layers/$$h/." "$(SRC)/prefix/"; \
	  fi; \
	done
	@echo "  PREFIX $(SRC)/prefix"

# `make build` — dune build @install over project/ using src/prefix/'s
# compiler + dune + installed deps. Targets @install (rather than the
# default @@default) so install-stanza binaries land in
# _build/install/default/bin where `make dest` can find them.
build: prefix
	@./build.sh dune build @install

# `make runtest` — dune runtest over project/.
runtest: prefix
	@./build.sh dune runtest

# `make dest` (default) — project binaries only, in dest/{bin,sbin}/.
# Walks every sub-project's _build/install/default/{bin,sbin}/ as
# well as the workspace-root _build/ (in case dune treated the whole
# bundle as a single project). `cp -L` dereferences dune's promote
# symlinks so dest/bin/<binary> is a real executable, not a link into
# the project's _build/.
dest: build
	@rm -rf dest
	@mkdir -p dest
	@found=0; for pkg_build in project/*/_build/install/default _build/install/default; do \
	  for sub in bin sbin; do \
	    if [ -d "$$pkg_build/$$sub" ]; then \
	      mkdir -p "dest/$$sub"; \
	      for f in "$$pkg_build/$$sub"/*; do \
	        [ -e "$$f" ] || continue; \
	        cp -L "$$f" "dest/$$sub/"; \
	      done; \
	      found=1; \
	    fi; \
	  done; \
	done; \
	if [ "$$found" = 0 ]; then \
	  echo "make dest: no binaries found under project/*/_build/install/default/{bin,sbin}" >&2; \
	fi
	@echo "dest/ — project binaries (from dune build @install):"
	@{ ls dest/bin 2>/dev/null | sed 's|^|  dest/bin/|'; ls dest/sbin 2>/dev/null | sed 's|^|  dest/sbin/|'; } || true

# `make install PREFIX=/opt/foo` — same as `make dest` but copies into
# $(DESTDIR)$(PREFIX)/{bin,sbin}/ instead of dest/.
install: build
	@found=0; for pkg_build in project/*/_build/install/default _build/install/default; do \
	  for sub in bin sbin; do \
	    if [ -d "$$pkg_build/$$sub" ]; then \
	      mkdir -p "$(DESTDIR)$(PREFIX)/$$sub"; \
	      for f in "$$pkg_build/$$sub"/*; do \
	        [ -e "$$f" ] || continue; \
	        cp -L "$$f" "$(DESTDIR)$(PREFIX)/$$sub/"; \
	      done; \
	      found=1; \
	    fi; \
	  done; \
	done; \
	if [ "$$found" = 0 ]; then \
	  echo "make install: no binaries found under project/*/_build/install/default/{bin,sbin}" >&2; \
	fi
	@echo "installed project binaries to $(DESTDIR)$(PREFIX)"

.DEFAULT_GOAL := dest
|make}

let append_duniverse_targets ~output =
  let path = output / "Makefile" in
  let oc = open_out_gen [ Open_append; Open_text ] 0o644 path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc duniverse_makefile_suffix)

(* -- main flow ---------------------------------------------------------- *)

let resolve_initial_inputs ~fs ~cwd_s ~targets ~with_repos ~with_deps =
  let project = Oi.Project.load ~fs cwd_s in
  let targets = if targets = [] then project.deps else targets in
  if targets = [] then
    Oi.Error.fail_config_error
      "oi dist duniverse: at least one TARGET is required (or run from a \
       project dir with *.opam files)";
  let targets, with_repos, with_deps =
    split_handle_targets ~with_repos ~with_deps targets
  in
  (project, targets, with_repos, with_deps)

let run_bundle ~harness ~refresh ~registry ~use_registry ~with_repos ~with_deps
    ~toolchain_override ~targets ~output =
  let { Harness.fs; clock; _ } = harness in
  if output = "" then
    Oi.Error.fail_config_error "oi dist duniverse: -o DIR is required";
  let cwd_s, _ = Workspace.resolved_cwd fs in
  let _project, targets, with_repos, with_deps =
    resolve_initial_inputs ~fs ~cwd_s ~targets ~with_repos ~with_deps
  in
  let output, vendor_dir = prepare_output_dirs ~fs output in
  let grouped_targets = Dist_runner.coalesce_targets targets in
  let {
    Pipeline_setup.env = pipeline_env;
    request = req;
    layer_remote;
    source_remote;
    _;
  } =
    Pipeline_setup.prepare ~harness ~refresh ~locked:false ~skip_local:true
      ~registry ~use_registry ~with_repos ~with_deps ~toolchain_override
      ~targets:grouped_targets ()
  in
  (* Aux-install the relocatable toolchain and warm the d10 source-archive
     cache. Without this, the [force_source = true] solve below produces a
     plan whose [ocaml-compiler] / [ocaml-base-compiler] are empty stubs
     (no [relocatable-compiler] node), and the Makefile bootstrap fails
     with [Unbound module Stdlib] when [ocaml.gen_ocaml_config] runs. *)
  let _binaries =
    Dist_runner.ensure_toolchain_built ~harness ~pipeline_env ~req ~layer_remote
      ~source_remote ~targets ~clock
  in
  (* Force every package to materialise from source (no layer-cache
     short-circuit) and pull test deps so the bundled
     [./build.sh dune runtest] has what it needs. *)
  let req =
    { req with Oi.Build_pipeline.force_source = true; with_test = true }
  in
  let recipe_solved = Oi.Build_pipeline.solve pipeline_env req in
  match recipe_solved.Oi.Build_pipeline.merged with
  | None ->
      Oi.Error.fail_config_error
        "oi dist duniverse: every solve group failed; nothing to emit."
  | Some plan ->
      (* 1. Prefetch + statically unpack every archive into
         [<output>/sources/<sha>/]. *)
      Dist_runner.unpack_sources ~harness ~registry ~plan ~output;
      (* 2. [project/<pkg>/] symlinks for solve roots; [vendor/<pkg>/]
         for the rest of the dune-buildable closure; record opam-name
         ownership so the Makefile sub-plan can skip project trees. *)
      let { project_pkgs; vendor_pkgs; project_owned_names } =
        layout_project_and_vendor ~output ~plan
      in
      (* 3. Emit the Makefile against a sub-plan that BUILDS THE DEPS
         ONLY (everything that isn't part of a [project/] tree). The
         project trees are built by [./build.sh dune build] against
         the resulting [src/prefix/]. The dist-makefile dest/install
         is suppressed so the duniverse suffix can append its own
         without redefinition. *)
      let mk_plan = makefile_plan ~plan ~project_owned_names in
      Makefile_export.emit mk_plan ~output ~emit_dest_install:false ();
      append_duniverse_targets ~output;
      (* 4. Workspace files + build.sh dune wrapper. *)
      write_workspace_files ~output;
      Oi.Say.field "project" "%d root package(s) → %s/"
        (List.length project_pkgs) (output / "project");
      Oi.Say.field "vendor"
        "%d dune-buildable dep(s) → %s/ (browseable; Makefile installs them to \
         src/prefix/)"
        (List.length vendor_pkgs) vendor_dir;
      Oi.Say.field "makefile"
        "%d dep node(s) (of %d total; %d project trees skipped)"
        (List.length mk_plan.nodes)
        (List.length plan.nodes)
        (List.length plan.nodes - List.length mk_plan.nodes);
      Oi.Say.ok
        "wrote duniverse bundle to %s — run: cd %s && make   (== make dest: \
         deps + prefix + dune build + project binaries → dest/)"
        output output

let man_block =
  [
    `S Manpage.s_description;
    `P
      "Vendor $(b,TARGET)'s dep closure into a self-contained bundle that \
       needs no $(b,oi) or $(b,opam) at build time. Sources are fetched from \
       the d10ir archive store (local cache, then $(b,--registry)).";
    `S "OUTPUT LAYOUT";
    `I
      ( "$(b,DIR/project/<pkg>/)",
        "Symlink into $(b,sources/<sha>/) for each solve root (the packages \
         the user asked for). Treated as a normal workspace member — strict \
         warnings, tests run." );
    `I
      ( "$(b,DIR/vendor/<pkg>/)",
        "Symlink into $(b,sources/<sha>/) for every dune-buildable dependency. \
         The root $(b,dune) file excludes $(b,vendor/) from dune's walk; the \
         Makefile installs these into $(b,src/prefix/lib/) and dune resolves \
         them via $(b,OCAMLPATH)." );
    `I
      ( "$(b,DIR/sources/<sha>/)",
        "Canonical unpacked source tree for every package in the plan \
         (dune-buildable or not). The Makefile builds from here." );
    `I
      ( "$(b,DIR/Makefile)",
        "Builds the dep closure (every node minus project trees) into \
         $(b,src/prefix/), drives $(b,dune build @install) over $(b,project/) \
         via $(b,build.sh), and assembles project binaries into $(b,dest/)." );
    `I
      ( "$(b,DIR/build.sh)",
        "Exports $(b,PATH) / $(b,OCAMLPATH) / $(b,OCAMLFIND_CONF) so they \
         point at $(b,src/prefix/), then $(b,exec)'s its arguments (default: \
         $(b,dune build))." );
    `I
      ( "$(b,DIR/dune-project), $(b,DIR/dune)",
        "Workspace marker; the $(b,(dirs ...)) stanza excludes $(b,vendor/), \
         $(b,sources/), and $(b,src/) so dune only walks $(b,project/)." );
    `S "TYPICAL USE";
    `Pre
      "  oi dist duniverse @avsm/owntracks-cli -o ./bundle\n\
      \  cd ./bundle\n\
      \  make            # == make dest: deps + prefix + dune build + dest/\n\
      \  make runtest    # deps + prefix + `dune runtest`\n\
      \  make deps       # just the dep closure (no prefix, no dune)\n\
      \  make prefix     # deps + assemble src/prefix/\n\
      \  make install PREFIX=/opt/foo   # project binaries -> /opt/foo/bin\n\
      \  ./build.sh CMD ...   # CMD with src/prefix/ on PATH/OCAMLPATH";
    `S "SEE ALSO";
    `P "$(b,oi dist makefile)(1)";
  ]

let cmd =
  let run (c : Terms.common) refresh registry use_registry with_repos with_deps
      _jobs toolchain_override targets output =
    Harness.run @@ fun ~sw env ->
    let harness =
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    run_bundle ~harness ~refresh ~registry ~use_registry ~with_repos ~with_deps
      ~toolchain_override ~targets ~output
  in
  let output =
    Arg.(
      value & opt string ""
      & info ~docv:"DIR"
          ~doc:
            "Output directory (required). Created if missing; existing entries \
             are kept."
          [ "o"; "output" ])
  in
  let targets =
    Arg.(
      value & pos_all string []
      & info ~docv:"TARGET" ~doc:"Package name or $(b,@HANDLE/PKG). Repeatable."
          [])
  in
  let info =
    Cmd.info "duniverse"
      ~doc:
        "Vendor a target into a self-contained bundle (Makefile-built \
         toolchain + dune-vendored sources)"
      ~man:man_block
  in
  Cmd.v info
    Term.(
      const run $ Terms.common $ Terms.refresh $ Terms.registry
      $ Terms.use_registry $ Terms.with_repos $ Terms.with_deps $ Terms.jobs
      $ Terms.toolchain $ targets $ output)
