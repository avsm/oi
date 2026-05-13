open Cmdliner

let ( / ) = Filename.concat

(* Default install prefix: [$HOME/.local]. Matches the XDG-ish convention
   that [pip install --user] / [cargo install] / [pipx] all already use. *)
let default_prefix () =
  match Sys.getenv_opt "HOME" with
  | Some h -> h / ".local"
  | None -> "/usr/local"

(* Expand a leading [~] / [~/] to [$HOME]. Cmdliner doesn't run the
   value through the shell so [--prefix=~/.local] would otherwise create
   a literal [~] directory at cwd. *)
let expand_tilde p =
  if p = "" then p
  else if p = "~" then default_prefix () |> Filename.dirname
  else if String.length p >= 2 && p.[0] = '~' && p.[1] = '/' then
    match Sys.getenv_opt "HOME" with
    | Some h -> h ^ String.sub p 1 (String.length p - 1)
    | None -> p
  else p

(* Normalise for $PATH comparison: drop a trailing [/] so [~/.local/bin]
   and [~/.local/bin/] compare equal. *)
let strip_trailing_slash p =
  let n = String.length p in
  if n > 1 && p.[n - 1] = '/' then String.sub p 0 (n - 1) else p

let path_contains dir =
  let dir = strip_trailing_slash dir in
  let p = Stdlib.Option.value (Sys.getenv_opt "PATH") ~default:"" in
  String.split_on_char ':' p
  |> List.exists (fun e -> strip_trailing_slash e = dir)

(* Detect the user's shell to suggest the right rc file. Returns the
   shell name (e.g. ["bash"]) and a [(rc-path, append-syntax)] pair. *)
let detect_shell_rc () =
  let shell = Stdlib.Option.value (Sys.getenv_opt "SHELL") ~default:"" in
  match Filename.basename shell with
  | "zsh" -> Some ("zsh", "~/.zshrc")
  | "bash" -> Some ("bash", "~/.bashrc")
  | "fish" -> Some ("fish", "~/.config/fish/config.fish")
  | _ -> None

(* PATH hint when [bin_dir] isn't on $PATH. Phrased so the user can
   copy-paste either form (current shell vs persisted). The persisted
   line is shell-specific (fish uses [fish_add_path] rather than
   [export]) so we tailor the snippet when we can detect the shell. *)
let print_path_hint ~bin_dir =
  Oi.Say.newline ();
  Oi.Say.warn "%s is not on your PATH." bin_dir;
  Fmt.pr "Add it to the current shell:@.";
  Fmt.pr "  export PATH=\"%s:$PATH\"@." bin_dir;
  match detect_shell_rc () with
  | None -> ()
  | Some ("fish", rc) ->
      Fmt.pr "Persist (fish):@.";
      Fmt.pr "  fish_add_path %s@." bin_dir;
      Fmt.kstr (Fmt.pr "  %a@." Oi.Style.pp_dim_string) "(writes to %s)" rc
  | Some (_, rc) ->
      Fmt.pr "Persist (add to %s):@." rc;
      Fmt.pr "  echo 'export PATH=\"%s:$PATH\"' >> %s@." bin_dir rc

(* Map a CLI target token to a [Build_pipeline.target]. [@h/pkg] →
   Overlay_pkg, [@h] → Overlay_all, anything else → Plain. *)
let parse_target s = Oi.Build_pipeline.parse s

(* Enumerate what [collect_install] would write for [root_layer_fs] into
   [prefix], i.e. every [bin/<n>], [sbin/<n>] file in the layer. Used
   for the pre-flight overwrite check; [share/] is deliberately
   skipped because data files commonly land at the same path across
   versions and [--force] is too coarse a hammer for them. *)
let preview_targets ~root_layer_fs ~prefix =
  let scan sub =
    let src = root_layer_fs / sub in
    if not (Sys.file_exists src) then []
    else
      try
        Sys.readdir src |> Array.to_list |> List.sort String.compare
        |> List.filter_map (fun name ->
            let s = src / name in
            match (Unix.stat s).st_kind with
            | Unix.S_REG -> Some (sub, name, prefix / sub / name)
            | _ -> None
            | exception Unix.Unix_error _ -> None)
      with Sys_error _ -> []
  in
  scan "bin" @ scan "sbin"

(* Promote every [root] tree (a directory shaped like [{bin,sbin,share}/])
   into [prefix]. Common backend for the target-mode and project-mode
   install paths: target-mode passes one [<cache>/layers/<oskey>/<h>/fs]
   per root layer; project-mode passes [<cwd>/_build/install/default].

   Walks [roots] once for conflicts, errors with the full list when any
   exist and [force = false], then runs {!Dist.collect_install} per
   root. Prints the install summary and a PATH hint when [bin_dir] isn't
   on $PATH. *)
let promote_to_prefix ~prefix ~bin_dir ~force ~(roots : string list) =
  let conflicts =
    List.concat_map
      (fun root ->
        if not (Sys.file_exists root) then []
        else
          preview_targets ~root_layer_fs:root ~prefix
          |> List.filter (fun (_, _, dst) -> Sys.file_exists dst))
      roots
  in
  (if conflicts <> [] && not force then
     let lines =
       List.map
         (fun (sub, name, dst) ->
           Fmt.str "  %s/%s %a %s" sub name Oi.Style.pp_dim_string "→" dst)
         conflicts
       |> String.concat "\n"
     in
     Oi.Error.fail_config_error
       "oi install: %d file(s) already exist under %s:@\n\
        %s@\n\
        Re-run with --force to overwrite."
       (List.length conflicts) prefix lines);
  let installed =
    List.concat_map
      (fun root ->
        if Sys.file_exists root then Dist.collect_install ~root ~dst:prefix
        else [])
      roots
  in
  let count sub =
    List.length (List.filter (fun (s, _, _) -> s = sub) installed)
  in
  let n_bin = count "bin" in
  let n_sbin = count "sbin" in
  let n_share = count "share" in
  Oi.Say.newline ();
  Oi.Say.step "Installed %d binary(ies) to %s" (n_bin + n_sbin) bin_dir;
  List.iter
    (fun (sub, name, dst) ->
      if sub = "bin" || sub = "sbin" then
        Fmt.pr "  %s %a %s@." name Oi.Style.pp_dim_string "→" dst)
    installed;
  if n_share > 0 then
    Oi.Say.info "+ %d data file(s) under %s/share/" n_share prefix;
  if (n_bin > 0 || n_sbin > 0) && not (path_contains bin_dir) then
    print_path_hint ~bin_dir

(* Detect "project mode": no positional targets, [--skip-local] not
   set, and the cwd has at least one non-empty [*.opam] file. Mirrors
   the same check [oi build] does at [build.ml:716-724]. *)
let cwd_has_opam_files cwd =
  try
    Sys.readdir cwd
    |> Array.exists (fun f ->
        Filename.check_suffix f ".opam" && Filename.chop_suffix f ".opam" <> "")
  with Sys_error _ -> false

(* -- Main command body --------------------------------------------------- *)

let cmd =
  let run (c : Terms.common) refresh locked skip_local registry use_registry
      with_repos with_deps jobs toolchain_override prefix force targets =
    Harness.run @@ fun ~sw env ->
    let harness =
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    let {
      Harness.fs;
      proc_mgr;
      clock;
      sys;
      platform;
      os_key;
      cache;
      data_dir;
      http_session;
      _;
    } =
      harness
    in
    let prefix = expand_tilde prefix in
    let bin_dir = prefix / "bin" in
    (* Pre-flight: make sure the prefix is writable (or creatable). The
       Dist copy code mkdirs on demand but a clear up-front error beats
       a half-finished tree if e.g. the user typoed a path under a dir
       they don't own. *)
    (try Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / prefix)
     with exn ->
       Oi.Error.fail_config_error
         "oi install: cannot create %s: %s. Pass --prefix=DIR to install \
          elsewhere."
         prefix (Printexc.to_string exn));
    let cwd_s, _ = Workspace.resolved_cwd fs in
    let project_mode =
      targets = [] && (not skip_local) && cwd_has_opam_files cwd_s
    in
    if targets = [] && not project_mode then
      Oi.Error.fail_config_error
        "oi install: pass one or more PKG / @HANDLE/PKG targets, or run from a \
         directory containing *.opam files.";
    if project_mode then begin
      (* Project mode: dune-build the cwd via [Project_build] (same path
         [oi build] uses with no positional), then promote the resulting
         [_build/install/default/] tree into [--prefix]. The Project_build
         step is the heavy lift — sync deps into [_oi/], run dune; if it
         exits non-zero we propagate that immediately. *)
      let ec =
        Project_build.run ~action:`Build ~fs ~proc_mgr ~clock ~sys ~platform
          ~os_key ~cache ~data_dir ~registry ~use_registry ~session:http_session
          ~refresh ~with_repos ~with_deps ?jobs ?toolchain:toolchain_override
          ~cwd:cwd_s ()
      in
      if ec <> 0 then exit ec;
      let install_root = cwd_s / "_build" / "install" / "default" in
      if not (Sys.file_exists install_root) then
        Oi.Error.fail_config_error
          "oi install: project built but %s is missing. Does the project's \
           dune-project declare anything installable?"
          install_root;
      promote_to_prefix ~prefix ~bin_dir ~force ~roots:[ install_root ]
    end
    else begin
      (* Explicit targets bypass cwd project metadata: [oi install] should
         match what [oi build] would do for the same targets so that
         [install = build + copy] holds. Build's non-project flow doesn't
         call [Project.load cwd], so neither should install here. Otherwise
         a project's [x-repos:] / [pin-depends:] silently expands the solve
         scope when the user gave an explicit [@h/pkg], producing different
         layer hashes than [oi build @h/pkg] in the same cwd. [--skip-local]
         (the CLI flag) still controls project_mode above; here we force
         project skipping unconditionally because we already decided we're
         not in project mode. *)
      let {
        Pipeline_setup.env = pipeline_env;
        request = req;
        layer_remote;
        source_remote;
        _;
      } =
        Pipeline_setup.prepare ~harness ~refresh ~locked ~skip_local:true
          ~registry ~use_registry ~with_repos ~with_deps ~toolchain_override
          ~targets:(List.map parse_target targets)
          ()
      in
      let target_label = String.concat ", " targets in
      let solved, build_result =
        Progress_ui.with_ui ~target:target_label
          ~clock:(clock :> _ Eio.Resource.t)
          ~enabled:(Tty.is_tty ())
        @@ fun reporter ->
        let solved = Oi.Build_pipeline.solve pipeline_env ~reporter req in
        let result =
          Oi.Build_pipeline.build pipeline_env ~reporter
            {
              solved;
              layer_remote;
              source_remote;
              jobs;
              upload_archive_url = None;
            }
        in
        (solved, result)
      in
      let cache_root = Oi.Cache.root_s cache in
      if
        List.for_all
          (fun (gr : Oi.Build_pipeline.group_result) ->
            Result.is_error gr.error)
          solved.groups
      then
        Oi.Error.fail_config_error
          "oi install: every solve group failed; nothing to install.";
      (* Build outcome triage. Mirrors what [oi run] does, minus the
         empty-d10ir-plan diagnostic (the install path can't usefully
         reason about cache freshness — if [Build_pipeline.build]
         returned [Some r] with zero work, every root layer should
         still be present and we just copy from them). *)
      (match build_result with
      | None -> Oi.Error.fail_config_error "oi install: build pipeline failed."
      | Some r when r.failed = 0 && r.skipped = 0 -> ()
      | Some r ->
          let pp_fail (f : D10ir.Direct.failure) =
            Fmt.str "%s.%s @ %s: %s — see %s" f.package.name f.package.version
              (D10ir.Direct.string_of_phase f.phase)
              f.error f.log_path
          in
          if r.failures <> [] then begin
            let summary = List.map pp_fail r.failures |> String.concat "\n  " in
            Oi.Error.fail_config_error
              "oi install: build failed (%d node(s), %d skipped).@\n  %s"
              r.failed r.skipped summary
          end
          else
            Oi.Error.fail_config_error
              "oi install: build failed (%d skipped). Re-run with \
               --verbosity=debug for the per-node trace."
              r.skipped);
      let root_hashes = Oi.Build_pipeline.root_layer_hashes solved in
      if root_hashes = [] then
        Oi.Error.fail_config_error
          "oi install: solve succeeded but no root packages matched the \
           requested targets. Re-run with --refresh.";
      let roots =
        List.map
          (fun h -> cache_root / "layers" / os_key / h / "fs")
          root_hashes
      in
      promote_to_prefix ~prefix ~bin_dir ~force ~roots
    end
  in
  let prefix =
    Arg.(
      value
      & opt string (default_prefix ())
      & info ~docv:"DIR"
          ~doc:
            "Install prefix; files land in $(i,DIR)/bin, /sbin, /share. \
             Leading $(b,~) is expanded."
          [ "prefix" ])
  in
  let force =
    Arg.(
      value & flag
      & info ~doc:"Overwrite existing files under $(b,--prefix)."
          [ "force"; "f" ])
  in
  let targets =
    Arg.(
      value & pos_all string []
      & info ~docv:"TARGET"
          ~doc:
            "Package to install: $(b,PKG), $(b,@HANDLE/PKG), or $(b,@HANDLE). \
             Repeatable. With no $(i,TARGET), builds the cwd project."
          [])
  in
  let info =
    Cmd.info "install"
      ~doc:"Build a target and copy its binaries into a user prefix"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Solve and build $(i,TARGET), then copy each root package's \
             $(b,bin/), $(b,sbin/), and $(b,share/) into $(b,--prefix) \
             (default $(b,\\$HOME/.local)).";
          `P
            "With no $(i,TARGET) and a cwd containing $(b,*.opam) files, \
             builds the cwd project via $(b,dune build) and promotes \
             $(b,_build/install/default/) instead. $(b,--skip-local) forces \
             target mode in a project directory.";
          `P
            "Refuses to clobber existing files; $(b,--force) overrides. Prints \
             a $(b,\\$PATH) hint when $(b,--prefix/bin) is missing from it.";
          `S Manpage.s_examples;
          `Pre
            "  oi install                  # cwd project\n\
            \  oi install dune\n\
            \  oi install @avsm/owntracks\n\
            \  oi install --prefix=/opt/oi --force utop merlin";
        ]
  in
  Cmd.v info
    Term.(
      const run $ Terms.common $ Terms.refresh $ Terms.locked $ Terms.skip_local
      $ Terms.registry $ Terms.use_registry $ Terms.with_repos $ Terms.with_deps
      $ Terms.jobs $ Terms.toolchain $ prefix $ force $ targets)
