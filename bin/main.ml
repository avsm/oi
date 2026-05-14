[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** [oi] entry point.

    Each top-level command lives in its own module under {!Oi_cmd}; the program
    info and shared man-page text are the only things this file still owns. *)

open Cmdliner

(* Populated by dune-build-info from the [oi] package's opam file (or
   [git describe] for a dev build). [n/a] when the binary was built
   outside a dune package context. *)
let version =
  match Build_info.V1.version () with
  | Some v -> Build_info.V1.Version.to_string v
  | None -> "n/a"

(* Stable exit code surface, mirroring opam where the meaning matches.
   Kept in sync with [Oi.Error.code]; the kind names come from
   [Oi.Error.kind_string] for the "kind" field of the JSON envelope. *)
let exits =
  let i ~doc code = Cmd.Exit.info ~doc code in
  [
    i ~doc:"on success." 0;
    i ~doc:"on a generic / unspecified failure." 1;
    i ~doc:"on a system error (out of disk, permission, ...)." 5;
    i ~doc:"on a configuration error (bad arguments, missing project, ...)." 10;
    i ~doc:"when the solver could not find a satisfying solution." 20;
    i ~doc:"on a fetch / sync failure (network, unavailable archive)." 30;
    i ~doc:"when a package's build commands failed." 31;
    i ~doc:"when one or more system depexts are not installed." 50;
    i ~doc:"when a package, binary, or layer was not found." 65;
    i ~doc:"on an internal error (bug); please report." 99;
    i ~doc:"on argument parsing errors." 124;
    i ~doc:"when interrupted (SIGINT/SIGTERM)." 130;
  ]

let man_description =
  [
    `S Manpage.s_description;
    `P
      "Solve $(b,*.opam) manifests against opam-repository and build, run, or \
       publish the result. Builds are content-addressed and cached.";
  ]

let man_quick_start =
  [
    `S "QUICK START";
    `P "$(b,1.) Run a tool from opam:";
    `Pre
      "  oi run utop\n\
      \  oi run ocamlformat -- --help\n\
      \  oi run --with=dune.3.20.0 -- dune --version";
    `P "$(b,2.) Run an $(b,.ml) script. Deps on the first line:";
    `Pre "  echo '[@@@opam fmt cmdliner]' > hello.ml\n  oi run hello.ml";
    `P "$(b,3.) Develop in a project. From the project root:";
    `Pre
      "  oi build               # sync deps + dev tools, run dune build\n\
      \  oi test                # dune runtest\n\
      \  oi build --deps-only   # sync only (after editing a *.opam)\n\
      \  oi exec -- dune utop   # any command, project env applied\n\
      \  oi add logs            # edit dune-project + re-solve";
    `P
      "$(b,oi build) writes $(b,.envrc) when $(b,direnv) is present. Otherwise:";
    `Pre "  eval \"\\$(oi env)\"";
    `P
      "$(b,4.) Pull from a curated overlay. $(b,oi repo) manages the reporepo \
       of known handles.";
    `Pre "  oi run @avsm/owntracks\n  oi run --with=@avsm/crockford roguedoi";
    `P "$(b,5.) Inspect before acting:";
    `Pre
      "  oi show utop                 # build plan + depexts\n\
      \  oi show --all                # every cached layer\n\
      \  oi search dune               # find a binary or package\n\
      \  oi run -n utop               # show the plan, run nothing";
    `P "$(b,6.) Publish a registry to a static HTTP server:";
    `Pre
      "  oi build --all --export ./registry\n\
      \  rsync -a ./registry/ user@server:/srv/oi-registry/\n\
      \  oi run --registry=https://server/oi-registry @avsm/owntracks";
    `P "$(b,7.) Generate Dockerfiles:";
    `Pre
      "  oi docker                                    # project build\n\
      \  oi docker --test --distro=alpine-3.23        # project tests\n\
      \  oi docker --all -o ./registry-build          # multi-distro";
  ]

let man_script_format =
  [
    `S "SCRIPT FORMAT";
    `P "First line of an $(b,.ml) script declares its dependencies:";
    `Pre "  [@@@opam fmt cmdliner>=1.2.0 lwt]";
    `P
      "Each token is an opam package, with optional version constraint \
       ($(b,>=), $(b,>), $(b,<=), $(b,<), $(b,=)) and optional findlib \
       sub-library ($(b,ppx_deriving.show)). $(b,ppx_*) packages are wired in \
       as preprocessors. $(b,oi run -vv SCRIPT.ml) prints the generated dune \
       project.";
  ]

let man_project_layout =
  [
    `S "PROJECT LAYOUT";
    `P
      "$(b,oi build) installs deps and dev tools into $(b,_oi/) under the \
       project root. Activate per shell:";
    `Pre
      "  eval \"\\$(oi env)\"          # one shell\n\
      \  oi exec -- CMD              # one command\n\
      \  direnv allow                # auto-activate (if direnv installed)";
    `P "$(b,_oi/) is rebuildable. Add to $(b,.gitignore).";
  ]

let man_automation =
  [
    `S "AUTOMATION";
    `P
      "Non-interactive, idempotent, non-zero exit on failure (see $(b,EXIT \
       STATUS)). $(b,oi self version --format=json) reports parseable schemas.";
    `Pre
      "  $(b,Discover)   oi CMD --help=plain, oi config, oi search PATTERN, oi \
       show TARGET\n\
      \  $(b,Plan)       -n on run/build; oi show --only-depexts TARGET\n\
      \  $(b,JSON)       --format=json on every command (stable schema_version \
       envelope, structured errors)\n\
      \  $(b,Quiet)      --color=never, -q, \
       --verbosity=quiet|error|warning|info|debug\n\
      \  $(b,Reproduce)  --toolchain, --with-repo, --registry, --use-registry; \
       oi source TARGET -o DIR\n\
      \  $(b,Sandbox)    --cache-dir, --data-dir, --use-registry=never, \
       --skip-local, --locked";
  ]

let man_environment =
  [
    `S Manpage.s_environment;
    `P
      "Long-lived state lives in the data directory (opam-repository clones, \
       toolchains). Rebuildable state lives in the cache directory (layers, \
       prefixes, source mirror).";
    `I
      ( "$(b,OI_DATA_DIR)",
        "Data directory. Falls back to $(b,XDG_DATA_HOME/oi), then \
         $(b,~/.local/share/oi)." );
    `I
      ( "$(b,OI_CACHE_DIR)",
        "Cache directory. Falls back to $(b,XDG_CACHE_HOME/oi), then \
         $(b,~/.cache/oi)." );
    `I
      ( "$(b,OI_REPOREPO)",
        "Reporepo clone path. Defaults to $(b,\\$OI_DATA_DIR/reporepo)." );
    `I
      ( "$(b,OI_REPOREPO_URL)",
        "Upstream URL for the initial reporepo clone. Ignored once the local \
         clone exists." );
    `S Manpage.s_see_also;
    `P "$(b,oix)(1) — one-shot runner for opam-packaged binaries.";
  ]

let man =
  List.concat
    [
      man_description;
      man_quick_start;
      man_script_format;
      man_project_layout;
      man_automation;
      man_environment;
    ]

let info =
  Cmd.info "oi" ~version ~exits ~doc:"Fast, stateless OCaml package manager"
    ~man

(* Cmdliner's [--help=auto] keys off [$TERM] alone — pager/groff (which
   emit ANSI escapes via [man]-style rendering) win whenever [TERM] is
   set, even when stdout is a pipe or a file. Rewrite a bare [--help]
   / [--help=auto] to [--help=plain] before [Cmd.eval] so [oi --help >
   out.txt] doesn't produce a file full of [\027[1m]/[\027[24m]. *)
let force_plain_help_when_redirected argv =
  let isatty = try Unix.isatty Unix.stdout with Unix.Unix_error _ -> false in
  if isatty then argv
  else
    Array.map
      (function "--help" | "--help=auto" -> "--help=plain" | a -> a)
      argv

let () =
  let group =
    Cmd.group info
      [
        Oi_cmd.Run.cmd;
        Oi_cmd.Build.cmd;
        Oi_cmd.Build.test_cmd;
        Oi_cmd.Install.cmd;
        Oi_cmd.Ir.cmd;
        Oi_cmd.Docker.cmd;
        Oi_cmd.Add.cmd;
        Oi_cmd.Exec.cmd;
        Oi_cmd.Search.cmd;
        Oi_cmd.Show.cmd;
        Oi_cmd.Env.cmd;
        Oi_cmd.Config.cmd;
        Oi_cmd.Repo.cmd;
        Oi_cmd.Clean.cmd;
        Oi_cmd.Cache.cmd;
        Oi_cmd.Self.cmd;
        Oi_cmd.Source.cmd;
      ]
  in
  let argv = force_plain_help_when_redirected Sys.argv in
  exit (Cmd.eval ~argv group)
