(* The [oi dist] command group. The makefile-emit engine lives in
   {!Dist_runner} so the bundle subcommand ({!Osdist_bundle}) can drive the
   same flow without inducing a module cycle through this file. *)

open Cmdliner

let makefile_run (c : Terms.common) refresh registry use_registry with_repos
    with_deps _jobs toolchain_override targets output =
  Harness.run @@ fun ~sw env ->
  let harness =
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  Dist_runner.run_makefile ~harness ~refresh ~registry ~use_registry
    ~with_repos ~with_deps ~toolchain_override ~targets ~output

let makefile_man =
  [
    `S Manpage.s_description;
    `P
      "Write a self-contained Makefile that builds $(i,TARGET) with no $(b,oi) \
       or $(b,opam) at build time — only $(b,make), $(b,curl), \
       $(b,tar)+$(b,zstd) and a host OCaml toolchain.";
    `P
      "With no $(i,TARGET), run from a project dir: the gitignore-clean \
       working tree is vendored as the source and built against its registry \
       deps — a self-contained release build.";
    `P
      "$(b,make) builds and assembles $(b,./dest); $(b,make V=1) is verbose; \
       $(b,make install) copies the binaries under $(b,DESTDIR)/$(b,PREFIX).";
    `P
      "A $(b,Dockerfile.<distro>) is also written for each target distro \
       (Alpine, Debian, Ubuntu 24.04/26.04, Fedora). Each is a two-stage \
       standalone build: a $(b,build) stage installs that distro's depexts and \
       runs $(b,make), and a clean runtime stage carries only the resulting \
       binaries/share. Build one with $(b,docker build -f Dockerfile.<distro> \
       .).";
    `S Manpage.s_examples;
    `Pre
      "  oi dist makefile utop -o ./utop-mk\n\
      \  oi dist makefile -o ./release      # current project\n\
      \  cd ./release && make && make install PREFIX=/opt/app\n\
      \  cd ./release && docker build -f Dockerfile.debian-stable -t app .";
  ]

let makefile_cmd =
  let output =
    Arg.(
      value & opt string ""
      & info ~docv:"DIR" ~doc:"Output directory (required)." [ "o"; "output" ])
  in
  let targets =
    Arg.(
      value & pos_all string []
      & info ~docv:"TARGET"
          ~doc:
            "Package name or $(b,@HANDLE/PKG). Omit to build the current \
             project from a gitignore-clean snapshot."
          [])
  in
  Cmd.v
    (Cmd.info "makefile"
       ~doc:"Emit a portable, oi-free Makefile build from registry archives"
       ~man:makefile_man)
    Term.(
      const makefile_run $ Terms.common $ Terms.refresh $ Terms.registry
      $ Terms.use_registry $ Terms.with_repos $ Terms.with_deps $ Terms.jobs
      $ Terms.toolchain $ targets $ output)

(* -- the group --------------------------------------------------------- *)

let cmd =
  let info =
    Cmd.info "dist"
      ~doc:
        "Emit portable build artefacts (Dockerfiles, obuilder specs, \
         Makefiles, .deb/.rpm/static-musl) for a target."
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Generate a self-contained build description for the current \
             project or a $(b,TARGET).";
          `S "SUBCOMMANDS";
          `I ("$(b,oi dist docker)",
              "Per-distro Dockerfiles (project / CI / registry).");
          `I ("$(b,oi dist obuilder)",
              "obuilder specs (s-expressions).");
          `I
            ( "$(b,oi dist makefile)",
              "Portable Makefile + per-distro Dockerfiles; no $(b,oi) needed \
               at build time." );
          `I
            ( "$(b,oi dist duniverse)",
              "Vendor a target's sources into a self-contained bundle." );
          `I
            ( "$(b,oi dist pkg)",
              "Emit a source bundle + per-distro packaging tree \
               (Dockerfiles, $(b,debian/), $(b,.spec)); with $(b,--build) \
               drives $(b,docker compose up --build) to produce .deb / .rpm \
               / static-musl artefacts in parallel." );
          `I
            ( "$(b,oi dist repo)",
              "Sign and assemble those artefacts into a published APT / DNF \
               / static-bin repo, idempotently. Auto-selects an Ed25519 \
               signing key from your keyring." );
        ]
  in
  Cmd.group info
    (Docker.subcommands
    @ [ makefile_cmd; Duniverse.cmd; Osdist_pkg.cmd; Osdist_repo.cmd ])
