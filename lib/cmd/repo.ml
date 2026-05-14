open Cmdliner

let ( / ) = Filename.concat

let reporepo_term =
  Arg.(
    value
    & opt string (Terms.reporepo_path ())
    & info ~docv:"DIR"
        ~doc:
          "Local reporepo clone. Falls back to $(b,\\$OI_REPOREPO), then \
           $(b,\\$OI_DATA_DIR/reporepo)."
        [ "reporepo" ])

let reporepo_url_term =
  Arg.(
    value
    & opt string (Terms.reporepo_url ())
    & info ~docv:"URL"
        ~doc:
          "Git URL used for the initial clone when no local reporepo exists. \
           Falls back to $(b,\\$OI_REPOREPO_URL), then the built-in upstream. \
           Ignored once the clone exists."
        [ "reporepo-url" ])

let depend_term =
  Arg.(
    value & opt_all string []
    & info ~docv:"HANDLE[=VERSION]"
        ~doc:
          "Declare a dependency on another overlay. $(b,HANDLE=VERSION) pins a \
           recorded version; bare $(b,HANDLE) accepts any. Repeatable. \
           Defaults to the current $(b,default) and $(b,relocatable)."
        [ "depend"; "d" ])

let parse_depend_spec s =
  match String.index_opt s '=' with
  | None -> (s, None)
  | Some i ->
      let h = String.sub s 0 i in
      let v = String.sub s (i + 1) (String.length s - i - 1) in
      (h, Some v)

let auto_commit ~sys ~reporepo ~op =
  match
    Oi.Source.Reporepo.commit_dirty ~sys ~path:reporepo
      ~msg:(Fmt.str "oi repo %s" op) ()
  with
  | [] -> ()
  | files ->
      Fmt.pr "  committed %d file(s) to reporepo HEAD@." (List.length files)

let parse_handle_version s =
  match String.index_opt s '=' with
  | None -> (s, None)
  | Some i ->
      (String.sub s 0 i, Some (String.sub s (i + 1) (String.length s - i - 1)))

(* Visible-column width of the toolchain target column. Counts the
   em-dash (one display column despite 3-byte UTF-8) as 1 so column
   alignment doesn't drift on entries without a toolchain. *)
let handle_span (e : Oi.Source.Reporepo.entry) =
  let style =
    if e.toolchain_name <> None then Oi.Style.accent else Oi.Style.header
  in
  Tty.Span.styled style e.handle

let toolchain_target_span (e : Oi.Source.Reporepo.entry) =
  match e.toolchain with
  | Some t -> Tty.Span.styled Oi.Style.info t
  | None -> Tty.Span.styled Oi.Style.dim "—"

let commit_span commit =
  let short =
    if commit = "" then ""
    else String.sub commit 0 (min 7 (String.length commit))
  in
  Tty.Span.styled Oi.Style.dim short

(* Upstream tip status for a reporepo entry, computed by re-running
   [git ls-remote] against its URL + ref. *)
type upstream_status =
  | Fresh  (** Pinned commit matches the upstream tip. *)
  | Stale of string  (** Upstream tip differs; carries its 40-char sha. *)
  | Unknown  (** [git ls-remote] failed (offline, auth, moved URL…). *)
  | Definition_only
      (** Entry has no [url:] (toolchain definition / metadata-only): nothing to
          check upstream. *)

let short_sha s = String.sub s 0 (min 7 (String.length s))

let check_upstream ~sys (e : Oi.Source.Reporepo.entry) =
  if e.url = "" then Definition_only
  else
    match Oi.Source.Reporepo.ls_remote_sha ~sys ?ref_:e.ref_ e.url with
    | tip when tip = e.commit -> Fresh
    | tip -> Stale tip
    | exception _ -> Unknown

let status_span = function
  | Fresh -> Tty.Span.styled Oi.Style.ok "up-to-date"
  | Unknown -> Tty.Span.styled Oi.Style.warn "unreachable"
  | Definition_only -> Tty.Span.styled Oi.Style.info "toolchain"
  | Stale tip ->
      Tty.Span.(
        styled Oi.Style.error "stale"
        ++ space
        ++ Fmt.kstr (styled Oi.Style.dim) "(%s)" (short_sha tip))

(* Toolchain section: definitions ([toolchain_name] set) printed under
   their CLI-facing name (the [x-oi-toolchain-name] field) rather than
   the [toolchain-*] reporepo handle. *)
let toolchain_cli_name (e : Oi.Source.Reporepo.entry) =
  match e.toolchain_name with Some n -> n | None -> e.handle

let mode_of (e : Oi.Source.Reporepo.entry) =
  match e.relocatable with
  | Some true -> ("relocatable", Oi.Style.ok)
  | Some false -> ("fixed-prefix", Oi.Style.warn)
  | None -> ("?", Oi.Style.dim)

let overlay_row ?status (e : Oi.Source.Reporepo.entry) =
  let base =
    [
      handle_span e;
      Tty.Span.text e.version;
      commit_span e.commit;
      toolchain_target_span e;
    ]
  in
  let status_cells =
    match status with None -> [] | Some s -> [ status_span s ]
  in
  base @ status_cells @ [ Tty.Span.styled Oi.Style.dim e.url ]

let overlay_columns ~with_status =
  let base =
    [
      Tty.Table.column "HANDLE";
      Tty.Table.column "VERSION";
      Tty.Table.column "COMMIT";
      Tty.Table.column "TOOLCHAIN";
    ]
  in
  let status_col = if with_status then [ Tty.Table.column "STATUS" ] else [] in
  base @ status_col @ [ Tty.Table.column "URL" ]

let render_overlay_table ~with_status rows =
  let table =
    Tty.Table.of_rows ~header_style:Oi.Style.header
      (overlay_columns ~with_status)
      rows
  in
  Oi.Style.pp_table Fmt.stdout table

let toolchain_row (e : Oi.Source.Reporepo.entry) =
  let name = toolchain_cli_name e in
  let mode, mode_style = mode_of e in
  let default_span =
    if e.default_toolchain then Tty.Span.styled Oi.Style.accent "yes"
    else Tty.Span.text ""
  in
  [
    Tty.Span.styled Oi.Style.header name;
    Tty.Span.text e.version;
    Tty.Span.styled mode_style mode;
    default_span;
    Tty.Span.text (Stdlib.Option.value ~default:"" e.toolchain_compiler);
  ]

let render_toolchain_table rows =
  let table =
    Tty.Table.of_rows ~header_style:Oi.Style.header
      [
        Tty.Table.column "NAME";
        Tty.Table.column "VERSION";
        Tty.Table.column "MODE";
        Tty.Table.column "DEFAULT";
        Tty.Table.column "COMPILER";
      ]
      rows
  in
  Oi.Style.pp_table Fmt.stdout table

let ref_term =
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"REF"
        ~doc:
          "Track $(i,REF) (branch or tag) instead of the upstream default \
           branch. Persisted; future $(b,oi repo bump) keeps following it."
        [ "ref"; "r" ])

let toolchain_repo_term =
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"NAME"
        ~doc:
          "Tag this overlay with a builtin toolchain (e.g. $(b,oxcaml), \
           $(b,ocaml-5.4)). Recorded as $(b,x-oi-toolchain); replaces the \
           default base-depends with the toolchain's own."
        [ "toolchain" ])

(* Look up a builtin toolchain's [depends] for use as [~base_handles]
   into [Reporepo.add]/[bump]. Errors loudly when the user passes a
   handle that isn't a known builtin so they don't silently get the
   default base set. *)
let base_handles_of_toolchain = function
  | None -> None
  | Some t -> (
      match Oi.Toolchain.depends_of ~handle:t with
      | Some d -> Some d
      | None ->
          Oi.Error.fail_config_error
            "unknown toolchain %S — known builtins listed by 'oi config'" t)

module Ls = struct
  (* Split toolchain definitions out of the overlay table — they don't have
     a URL of their own and their reporepo handle ([toolchain-*]) is an
     implementation detail; users address them by their CLI name (e.g.
     [ocaml-5.4]). Within overlays, base entries (no [x-oi-toolchain] tag,
     e.g. [default], [relocatable]) sort first since they're not user-
     supplied. *)
  let split_overlays_toolchains entries =
    let toolchains, overlays =
      List.partition
        (fun (e : Oi.Source.Reporepo.entry) -> e.toolchain_name <> None)
        entries
    in
    let untagged, tagged =
      List.partition
        (fun (e : Oi.Source.Reporepo.entry) -> e.toolchain = None)
        overlays
    in
    (untagged @ tagged, toolchains)

  let pp_subtitle ppf s = Oi.Style.pp_dim_string ppf s

  (* Per-entry [git ls-remote] runs in parallel up to four at a time. Four
     keeps the pipe/fd footprint small without making a 30-entry reporepo
     serial. Failures downgrade to [Unknown] — a flaky network must not
     make [oi repo list] unusable. *)
  let render_overlays_with_status ~sys overlays =
    let indexed = List.mapi (fun i e -> (i, e)) overlays in
    let statuses = Array.make (List.length indexed) Unknown in
    Eio.Fiber.List.iter ~max_fibers:4
      (fun (i, e) -> statuses.(i) <- check_upstream ~sys e)
      indexed;
    render_overlay_table ~with_status:true
      (List.mapi (fun i e -> overlay_row ~status:statuses.(i) e) overlays)

  let render_overlays ~sys ~no_check overlays =
    if overlays = [] then ()
    else begin
      Fmt.pr "%a@." Oi.Style.pp_header_string "OVERLAYS";
      Fmt.pr "%a@.@." pp_subtitle
        "Curated opam packages pinned to git commits. Use as \
         --with-repo=@HANDLE, --with=@HANDLE/PKG, or 'x-repos:' in *.opam.";
      if no_check then
        render_overlay_table ~with_status:false (List.map overlay_row overlays)
      else render_overlays_with_status ~sys overlays
    end

  let render_toolchains ~had_overlays toolchains =
    if toolchains = [] then ()
    else begin
      if had_overlays then Fmt.pr "@.";
      Fmt.pr "%a@." Oi.Style.pp_header_string "TOOLCHAINS";
      Fmt.pr "%a@.@." pp_subtitle
        "Compiler bundles. Select with --toolchain=NAME; the DEFAULT entry is \
         used otherwise.";
      render_toolchain_table (List.map toolchain_row toolchains)
    end

  let render_entries ~sys ~reporepo ~no_check entries =
    Fmt.pr "Reporepo: %s@.@." reporepo;
    let latest_entries =
      entries
      |> List.map (fun (e : Oi.Source.Reporepo.entry) -> e.handle)
      |> List.sort_uniq String.compare
      |> List.filter_map (fun handle ->
          Oi.Source.Reporepo.latest entries ~handle)
    in
    let overlays, toolchains = split_overlays_toolchains latest_entries in
    render_overlays ~sys ~no_check overlays;
    render_toolchains ~had_overlays:(overlays <> []) toolchains

  let cmd =
    let run () reporepo reporepo_url no_check =
      Harness.run @@ fun ~sw:_ env ->
      let proc_mgr = Eio.Stdenv.process_mgr env in
      let fs = Eio.Stdenv.fs env in
      let sys =
        D10.Sysops.v ~stdout:(Eio.Stdenv.stdout env)
          ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs
          ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ()
      in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
        ~url:reporepo_url ();
      match Oi.Source.Reporepo.load ~path:reporepo with
      | [] -> Fmt.pr "Reporepo %s is empty.@." reporepo
      | entries -> render_entries ~sys ~reporepo ~no_check entries
    in
    let no_check =
      Arg.(
        value & flag
        & info ~doc:"Skip the $(b,git ls-remote) upstream check (offline)."
            [ "no-check" ])
    in
    let info =
      Cmd.info "list" ~doc:"List overlays and toolchains in the reporepo"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Print two tables: $(b,OVERLAYS) (with upstream status) and \
               $(b,TOOLCHAINS).";
            `S "OVERLAYS";
            `P
              "Curated opam packages pinned to git commits. Reference with \
               $(b,--with-repo=@HANDLE), $(b,--with=@HANDLE/PKG), or \
               $(b,x-repos: [\"@HANDLE\"]) in $(b,*.opam).";
            `P
              "Base repos ($(b,default), $(b,relocatable)) sort first; user \
               overlays follow. Columns: $(b,HANDLE), $(b,VERSION), \
               $(b,COMMIT), $(b,TOOLCHAIN), $(b,STATUS), $(b,URL).";
            `P "$(b,STATUS) (from parallel $(b,git ls-remote)):";
            `I ("$(b,up-to-date)", "Pin matches upstream.");
            `I ("$(b,stale)", "Upstream has moved past the pin.");
            `I ("$(b,unreachable)", "Remote could not be contacted.");
            `S "TOOLCHAINS";
            `P
              "Compiler bundles. Select with $(b,--toolchain=NAME); the \
               $(b,DEFAULT) entry is used otherwise.";
            `P
              "Columns: $(b,NAME), $(b,VERSION), $(b,MODE) ($(b,relocatable) \
               or $(b,fixed-prefix)), $(b,DEFAULT), $(b,COMPILER).";
          ]
    in
    Cmd.v info
      Term.(
        const run $ Terms.log $ reporepo_term $ reporepo_url_term $ no_check)
end

module Show = struct
  let print_depend (h, v) =
    match v with
    | None -> Fmt.pr "    %s@." h
    | Some ver -> Fmt.pr "    %s = %s@." h ver

  let print_root_group = function
    | [] -> ()
    | [ p ] -> Fmt.pr "    %s@." p
    | multi -> Fmt.pr "    [%s]@." (String.concat " " multi)

  let print_entry (e : Oi.Source.Reporepo.entry) =
    Fmt.pr "%s.%s@." e.handle e.version;
    Fmt.pr "  url:    %s@." e.url;
    Fmt.pr "  commit: %s@." e.commit;
    (match e.ref_ with Some r -> Fmt.pr "  ref:    %s@." r | None -> ());
    (match e.depends with
    | [] -> ()
    | ds ->
        Fmt.pr "  depends:@.";
        List.iter print_depend ds);
    (match e.root_packages with
    | [] -> ()
    | groups ->
        Fmt.pr "  root-packages:@.";
        List.iter print_root_group groups);
    Fmt.pr "@."

  let cmd =
    let run () reporepo reporepo_url handle =
      Harness.run @@ fun ~sw:_ env ->
      let proc_mgr = Eio.Stdenv.process_mgr env in
      let fs = Eio.Stdenv.fs env in
      let sys =
        D10.Sysops.v ~stdout:(Eio.Stdenv.stdout env)
          ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs
          ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ()
      in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
        ~url:reporepo_url ();
      let entries = Oi.Source.Reporepo.load ~path:reporepo in
      let matches =
        List.filter
          (fun (e : Oi.Source.Reporepo.entry) -> e.handle = handle)
          entries
        |> List.sort
             (fun
               (a : Oi.Source.Reporepo.entry) (b : Oi.Source.Reporepo.entry) ->
               OpamPackage.Version.compare
                 (OpamPackage.Version.of_string b.version)
                 (OpamPackage.Version.of_string a.version))
      in
      if matches = [] then
        Oi.Error.fail_not_found handle "no overlay %s in reporepo %s" handle
          reporepo;
      List.iter print_entry matches
    in
    let handle =
      Arg.(
        required
        & pos 0 (some string) None
        & info ~docv:"HANDLE" ~doc:"Overlay handle to inspect." [])
    in
    let info =
      Cmd.info "show"
        ~doc:"Show every version of one overlay, with commits and dependencies"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Print the recorded history of $(b,HANDLE). For each version: \
               git URL, pinned commit, tracked $(b,--ref), and depended-on \
               overlays.";
          ]
    in
    Cmd.v info
      Term.(const run $ Terms.log $ reporepo_term $ reporepo_url_term $ handle)
end

module Add = struct
  let print_dep (h, v) =
    match v with
    | Some ver -> Fmt.pr "  %s = %s@." h ver
    | None -> Fmt.pr "  %s@." h

  let print_materialise_summary ~fs ~sys ~path (e : Oi.Source.Reporepo.entry) =
    Fmt.pr "Materialising v2/%s/ from %s @ %s@." e.handle e.url
      (String.sub e.commit 0 (min 7 (String.length e.commit)));
    let summary =
      Oi.Source.Reporepo.materialise_handle ~fs ~sys ~path ~handle:e.handle
        ~url:e.url ~commit:e.commit
    in
    Fmt.pr
      "  %d packages: %d already pinned, %d rewritten, %d marked unavailable@."
      summary.total summary.kept summary.rewrote
      (List.length summary.unavailable)

  let cmd =
    let run () reporepo reporepo_url handle url ref_ toolchain depend_specs
        force =
      Harness.run @@ fun ~sw:_ env ->
      let proc_mgr = Eio.Stdenv.process_mgr env in
      let fs = Eio.Stdenv.fs env in
      let sys =
        D10.Sysops.v ~stdout:(Eio.Stdenv.stdout env)
          ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs
          ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ()
      in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
        ~url:reporepo_url ();
      let depends =
        match depend_specs with
        | [] -> None
        | _ -> Some (List.map parse_depend_spec depend_specs)
      in
      let base_handles = base_handles_of_toolchain toolchain in
      let e =
        Oi.Source.Reporepo.add ~fs ~sys ~path:reporepo ~handle ~url ?ref_
          ?toolchain ?base_handles ?depends ~force ()
      in
      Fmt.pr "Added %s.%s@ url=%s@ commit=%s@ at %s@." e.handle e.version e.url
        e.commit e.opam_path;
      if e.depends <> [] then begin
        Fmt.pr "Depends:@.";
        List.iter print_dep e.depends
      end;
      if e.url <> "" then print_materialise_summary ~fs ~sys ~path:reporepo e;
      auto_commit ~sys ~reporepo ~op:(Fmt.str "add %s" handle)
    in
    let handle =
      Arg.(
        required
        & pos 0 (some string) None
        & info ~docv:"HANDLE"
            ~doc:"Short opam-valid overlay name (e.g. $(b,avsm))." [])
    in
    let url =
      Arg.(
        required
        & pos 1 (some string) None
        & info ~docv:"URL" ~doc:"Git URL of the upstream opam-repository." [])
    in
    let force =
      Arg.(
        value & flag
        & info
            ~doc:
              "Add a new $(b,YYYYMMDD.N) entry even if $(i,HANDLE) exists. \
               Older entries are kept."
            [ "force"; "f" ])
    in
    let info =
      Cmd.info "add" ~doc:"Register a new overlay in the reporepo"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Register $(b,HANDLE), pinned to the current commit on \
               $(b,URL)'s default branch (or $(b,--ref BRANCH)).";
            `P
              "Non-base overlays auto-depend on the current $(b,default) and \
               $(b,relocatable). $(b,--toolchain=NAME) substitutes the \
               toolchain's own base set.";
            `S Manpage.s_examples;
            `Pre
              "  oi repo add default \
               https://github.com/ocaml/opam-repository.git\n\
              \  oi repo add relocatable \
               https://github.com/dra27/opam-repository.git --ref relocatable\n\
              \  oi repo add avsm \
               https://tangled.org/anil.recoil.org/aoah-opam-repo.git";
          ]
    in
    Cmd.v info
      Term.(
        const run $ Terms.log $ reporepo_term $ reporepo_url_term $ handle $ url
        $ ref_term $ toolchain_repo_term $ depend_term $ force)
end

module Bump = struct
  (* -- Source identity for incremental bake -------------------------------

     Each opam file in v2/<handle>/ has a "source identity": the
     [(url, sorted checksums)] pair plus a sorted list of
     [(extra_name, url, sorted checksums)]. Two opam files with the
     same source identity will fetch the same source bytes, so they
     can share an [x-d10-archive] sha — the bake step short-circuits
     when the post-materialise opam matches a snapshot from the
     pre-materialise tree.

     [materialise_handle] always rewrites v2/<handle>/ from scratch
     (so opam files lose [x-d10-archive] in the rewrite). The bump
     flow takes a snapshot before materialise, restores shas after,
     and only fetches+tars for packages whose source identity is
     genuinely new or changed. *)

  (* Source identity: optional [(url, sorted checksums)] +
     a sorted list of [(extra_name, url, sorted checksums)]. Pure
     comparison: two identical tuples mean two opam files agree on
     what bytes they fetch. *)
  let read_source_identity_and_sha opam_path =
    try
      let opam =
        OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw opam_path))
      in
      let url =
        OpamFile.OPAM.url opam
        |> Stdlib.Option.map (fun urlf ->
            let u = OpamUrl.to_string (OpamFile.URL.url urlf) in
            let cks =
              OpamFile.URL.checksum urlf
              |> List.map OpamHash.to_string
              |> List.sort String.compare
            in
            (u, cks))
      in
      let extras =
        OpamFile.OPAM.extra_sources opam
        |> List.map (fun (b, urlf) ->
            let n = OpamFilename.Base.to_string b in
            let u = OpamUrl.to_string (OpamFile.URL.url urlf) in
            let cks =
              OpamFile.URL.checksum urlf
              |> List.map OpamHash.to_string
              |> List.sort String.compare
            in
            (n, u, cks))
        |> List.sort compare
      in
      Some ((url, extras), Oi.Keys.read_string_ext Oi.Keys.d10_archive opam)
    with Failure _ | Sys_error _ -> None

  (* Apply [f] to one [<pkg>.<ver_dir>/opam] file if it parses and the dir
     name starts with [pkg.]. *)
  let iter_one_opam ~pkg ~pkg_dir ~ver_dir f =
    let opam_path = pkg_dir / ver_dir / "opam" in
    if not (Sys.file_exists opam_path) then ()
    else
      let prefix = pkg ^ "." in
      if not (String.starts_with ~prefix ver_dir) then ()
      else
        let version =
          String.sub ver_dir (String.length prefix)
            (String.length ver_dir - String.length prefix)
        in
        f ~pkg ~version ~pkg_dir:(pkg_dir / ver_dir) ~opam_path

  (* Apply [f] to every [packages/<pkg>/<pkg>.VER/opam] under [pkg_dir]. *)
  let iter_pkg_versions ~pkg ~pkg_dir f =
    if not (Sys.is_directory pkg_dir) then ()
    else
      Sys.readdir pkg_dir
      |> Array.iter (fun ver_dir -> iter_one_opam ~pkg ~pkg_dir ~ver_dir f)

  let iter_handle_opams ~reporepo ~handle f =
    let pkgs_dir = reporepo / "v2" / handle / "packages" in
    if not (Sys.file_exists pkgs_dir) then ()
    else
      Sys.readdir pkgs_dir
      |> Array.iter (fun pkg ->
          let pkg_dir = pkgs_dir / pkg in
          iter_pkg_versions ~pkg ~pkg_dir f)

  (* Capture (pkg, ver_dir) -> (source_identity, baked sha) for every
     opam file in v2/<handle>/ that already has an x-d10-archive set.
     Called BEFORE materialise wipes the tree. *)
  let snapshot_handle ~reporepo ~handle =
    let snap = Hashtbl.create 256 in
    iter_handle_opams ~reporepo ~handle
      (fun ~pkg ~version ~pkg_dir:_ ~opam_path ->
        match read_source_identity_and_sha opam_path with
        | Some (id, Some sha) -> Hashtbl.replace snap (pkg, version) (id, sha)
        | _ -> ());
    snap

  (* Walk the post-materialise v2/<handle>/ tree. For every opam file
     whose new source identity matches a snapshot entry, write the
     snapshot's sha back as x-d10-archive. Returns the count of
     restored entries. *)
  let restore_unchanged_archives ~reporepo ~handle ~snap =
    let restored = ref 0 in
    iter_handle_opams ~reporepo ~handle
      (fun ~pkg ~version ~pkg_dir:_ ~opam_path ->
        match
          ( read_source_identity_and_sha opam_path,
            Hashtbl.find_opt snap (pkg, version) )
        with
        | Some (new_id, _), Some (prior_id, prior_sha) when new_id = prior_id
          -> (
            match Ir.opam_set_x_d10_archive ~path:opam_path ~sha:prior_sha with
            | `Added -> incr restored
            | `Already -> ())
        | _ -> ());
    !restored

  (* Walk v2/<handle>/ and fetch+tar every package whose opam still
     lacks x-d10-archive (i.e. wasn't restored from the snapshot).
     Returns (baked, failed) counts. [?on_baked] fires for each
     successful bake with the resulting sha + on-disk path, so callers
     can mirror the archive (e.g. [s3cmd put]) without re-walking. *)
  (* A bake is "still current" only when both the [.tar.zst] AND its
     [.json] sidecar exist locally for the recorded x-d10-archive sha.
     If the sidecar is missing (e.g. archive pre-dates the
     [Source_manifest] scheme), force a re-bake so the bake path
     emits the missing JSON. The archive content is deterministic in
     the inputs, so the re-bake produces the same sha. *)
  let sidecar_present ~(d10 : D10.Config.t) ~sha =
    let archives_dir =
      Filename.concat
        (Eio.Path.native_exn d10.root)
        (Filename.concat "d10ir" "archives")
    in
    Sys.file_exists (Filename.concat archives_dir (sha ^ ".json"))

  let bake_changed_archives ?on_baked ~proc_mgr ~fs ~d10 ~cache_root ~platform
      ~reporepo ~handle () =
    let baked = ref 0 in
    let failed = ref 0 in
    iter_handle_opams ~reporepo ~handle
      (fun ~pkg ~version ~pkg_dir ~opam_path ->
        match read_source_identity_and_sha opam_path with
        | Some (_, Some sha) when sidecar_present ~d10 ~sha ->
            () (* already has x-d10-archive AND a sidecar — nothing to do *)
        | _ -> (
            try
              let built =
                Oi.Archive_builder.build_no_solve ~proc_mgr ~fs ~d10 ~cache_root
                  ~platform ~name:pkg ~version ~pkg_dir ~opam_path ()
              in
              (match
                 Ir.opam_set_x_d10_archive ~path:opam_path ~sha:built.sha256
               with
              | `Added | `Already -> ());
              incr baked;
              Fmt.pr "  %a %s.%s@." Oi.Style.pp_ok_string "baked" pkg version;
              match on_baked with
              | None -> ()
              | Some f ->
                  f ~name:pkg ~version ~sha:built.sha256 ~path:built.path
            with exn ->
              incr failed;
              Fmt.pr "  %a %s.%s: %s@." Oi.Style.pp_warn_string "skip" pkg
                version (Printexc.to_string exn)));
    (!baked, !failed)

  let count_packages_missing_archive ~reporepo ~handle =
    let n = ref 0 in
    iter_handle_opams ~reporepo ~handle
      (fun ~pkg:_ ~version:_ ~pkg_dir:_ ~opam_path ->
        match read_source_identity_and_sha opam_path with
        | Some (_, Some _) -> ()
        | _ -> incr n);
    !n

  (* Bake strip is disabled: [oi]'s toolchain-bootstrap flow
     ([Toolchain.install_via_opam] et al.) feeds the unbaked opam file
     to opam directly, which fetches the pristine [url{}] tarball and
     applies [patches:] / [extra-files:] before running [build:]. There
     is no clean signal at bake time for "this package will only ever be
     consumed via its d10-archive", so we can't safely delete patch
     metadata. Stripping it broke [oxcaml-compiler.5.2.0minus31]'s
     [ignore-opam.patch] (which adds [init_deps] to oxcaml's
     [(data_only_dirs ...)] list), and the same risk applies to any
     bootstrap package whose [build:] depends on a [patches:] edit.
     Disk-space recovery can come back when we have an explicit
     "archive-only" marker per package. *)
  let strip_resolved_artifacts ~fs:_ ~reporepo:_ ~handle:_ = 0

  let bump_one ~fs ~sys ~reporepo ~handle ~url ~ref_ ~toolchain ~depends
      ~default =
    let effective_toolchain =
      match toolchain with
      | Some _ -> toolchain
      | None ->
          let entries = Oi.Source.Reporepo.load ~path:reporepo in
          Stdlib.Option.bind (Oi.Source.Reporepo.latest entries ~handle)
            (fun (e : Oi.Source.Reporepo.entry) -> e.toolchain)
    in
    let base_handles = base_handles_of_toolchain effective_toolchain in
    (* Setting --default on toolchain X requires clearing the flag on
       whichever toolchain currently holds it, otherwise the reporepo
       has two defaults and Source.Reporepo.load refuses to parse. *)
    if default = Some true then begin
      let entries = Oi.Source.Reporepo.load ~path:reporepo in
      match Oi.Source.Reporepo.default_toolchain entries with
      | Some prev when prev.handle <> handle -> begin
          Fmt.pr "Clearing default flag on %s (replaced by %s)@." prev.handle
            handle;
          match
            Oi.Source.Reporepo.bump ~fs ~sys ~path:reporepo ~handle:prev.handle
              ~default:false ()
          with
          | `Bumped b -> Fmt.pr "  -> %s.%s@." b.handle b.version
          | `Unchanged _ -> ()
        end
      | _ -> ()
    end;
    let bumped =
      Oi.Source.Reporepo.bump ~fs ~sys ~path:reporepo ~handle ?url ?ref_
        ?toolchain ?base_handles ?depends ?default ()
    in
    let entry = match bumped with `Bumped e -> e | `Unchanged e -> e in
    (match bumped with
    | `Bumped e ->
        Fmt.pr "Bumped %s to %s@ commit=%s@ at %s@." e.handle e.version e.commit
          e.opam_path;
        if e.default_toolchain then Fmt.pr "  marked as default toolchain@."
    | `Unchanged e ->
        Fmt.pr
          "No change: %s.%s already pins the current upstream commit (%s).@."
          e.handle e.version e.commit);
    if entry.url = "" then ()
    else begin
      Fmt.pr
        "Materialising v2/%s/ from %s @ %s — this resolves every package's url \
         and may take a while.@."
        entry.handle entry.url
        (String.sub entry.commit 0 (min 7 (String.length entry.commit)));
      let summary =
        Oi.Source.Reporepo.materialise_handle ~fs ~sys ~path:reporepo
          ~handle:entry.handle ~url:entry.url ~commit:entry.commit
      in
      Fmt.pr
        "  %d packages: %d already pinned, %d rewritten, %d marked \
         unavailable@."
        summary.total summary.kept summary.rewrote
        (List.length summary.unavailable);
      List.iter
        (fun (pkg, reason) ->
          Fmt.pr "    %a %s: %s@." Oi.Style.pp_warn_string "unavailable" pkg
            reason)
        summary.unavailable
    end;
    (* Whether we materialised or not, return the entry so the
       caller can decide what to commit with. *)
    entry

  (* Normalise trailing slash off the archives-URL so [<base>/<sha>] never
     grows a double slash. *)
  let strip_trailing_slash u =
    if String.length u > 0 && u.[String.length u - 1] = '/' then
      String.sub u 0 (String.length u - 1)
    else u

  (* [--upload-archives]: for each fresh bake, [s3cmd put] both the
     [.tar.zst] archive and its sibling [.json] source-manifest sidecar
     so the bucket stays self-describing. Skipping the sidecar (as we
     used to) left fresh local caches downstream missing the
     [.json] — [D10ir.Registry.do_fetch_sidecar] is a best-effort
     companion to the [.tar.zst] pull and silently no-ops when the
     bucket lacks the JSON. Failures print a warning but don't abort.

     Honours [OI_S3CFG] so the [s3cmd] invocation reads the same config
     [Oi.Build_pipeline.s3_put_quiet] does; matters inside the [oi
     docker] containers where the config lives under [/tmp]. *)
  let s3cmd_argv args =
    match Sys.getenv_opt "OI_S3CFG" with
    | Some path when path <> "" -> "s3cmd" :: "-c" :: path :: args
    | _ -> "s3cmd" :: args

  let put_one ~sys ~src ~url =
    D10.Sysops.Cmd.run sys (s3cmd_argv [ "put"; "--quiet"; src; url ])

  let upload_tar ~sys ~pkg ~src ~url =
    try
      put_one ~sys ~src ~url;
      Fmt.pr "    %a %s -> %s@." Oi.Style.pp_ok_string "uploaded" pkg url
    with exn ->
      Fmt.pr "    %a upload %s: %s@." Oi.Style.pp_warn_string "skip" pkg
        (Printexc.to_string exn)

  let upload_sidecar ~sys ~pkg ~json_path ~json_url =
    if not (Sys.file_exists json_path) then
      Fmt.pr "    %a sidecar %s: %s missing@." Oi.Style.pp_warn_string "skip"
        pkg json_path
    else
      try
        put_one ~sys ~src:json_path ~url:json_url;
        Fmt.pr "    %a %s -> %s@." Oi.Style.pp_ok_string "uploaded"
          (pkg ^ " sidecar") json_url
      with exn ->
        Fmt.pr "    %a sidecar %s: %s@." Oi.Style.pp_warn_string "skip" pkg
          (Printexc.to_string exn)

  let make_upload_callback ~sys ~upload_archives ~archives_url =
    if not upload_archives then None
    else
      let base = strip_trailing_slash archives_url in
      Some
        (fun ~name ~version ~sha ~path ->
          let pkg = Fmt.str "%s.%s" name version in
          let tar_url = Fmt.str "%s/%s.tar.zst" base sha in
          let json_path = Filename.dirname path / Fmt.str "%s.json" sha in
          let json_url = Fmt.str "%s/%s.json" base sha in
          upload_tar ~sys ~pkg ~src:path ~url:tar_url;
          upload_sidecar ~sys ~pkg ~json_path ~json_url)

  type bump_ctx = {
    proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
    fs : Eio.Fs.dir_ty Eio.Path.t;
    sys : D10.Sysops.t;
    d10 : D10.Config.t;
    cache_root : string;
    platform : Osrel.t;
    reporepo : string;
    no_bake : bool;
    rebake : bool;
    upload_on_baked :
      (name:string -> version:string -> sha:string -> path:string -> unit)
      option;
  }

  let restore_or_discard_shas ctx ~handle ~snap =
    if ctx.rebake then
      Fmt.pr
        "@.--rebake: discarding previous x-d10-archive shas, will re-bake \
         every package@."
    else begin
      Fmt.pr "@.Restoring x-d10-archive for unchanged sources ...@.";
      let restored =
        restore_unchanged_archives ~reporepo:ctx.reporepo ~handle ~snap
      in
      Fmt.pr "  %d restored from previous bump@." restored
    end

  let run_bake_phase ctx ~handle =
    if not ctx.no_bake then begin
      Fmt.pr "Baking x-d10-archive for new or changed sources ...@.";
      let baked, failed =
        bake_changed_archives ?on_baked:ctx.upload_on_baked
          ~proc_mgr:ctx.proc_mgr ~fs:ctx.fs ~d10:ctx.d10
          ~cache_root:ctx.cache_root ~platform:ctx.platform
          ~reporepo:ctx.reporepo ~handle ()
      in
      Fmt.pr "  %d baked, %d failed@." baked failed
    end
    else
      Fmt.pr
        "  --no-bake: skipping fresh bakes; %d package(s) without \
         x-d10-archive will be left unbaked@."
        (count_packages_missing_archive ~reporepo:ctx.reporepo ~handle)

  (* Snapshot pre-materialise source identity + baked sha for every package
     so the post-materialise pass can preserve [x-d10-archive] for any
     package whose source URL + checksums are unchanged — even with
     [--no-bake]. [--rebake] feeds an empty snapshot, so restore is a no-op
     and bake sees every opam. *)
  let bump_and_bake ~ctx ~url ~ref_ ~toolchain ~depends ~default handle =
    let snap =
      if ctx.rebake then Hashtbl.create 0
      else snapshot_handle ~reporepo:ctx.reporepo ~handle
    in
    let entry =
      bump_one ~fs:ctx.fs ~sys:ctx.sys ~reporepo:ctx.reporepo ~handle ~url ~ref_
        ~toolchain ~depends ~default
    in
    if entry.url <> "" then begin
      restore_or_discard_shas ctx ~handle ~snap;
      run_bake_phase ctx ~handle;
      let stripped =
        strip_resolved_artifacts ~fs:ctx.fs ~reporepo:ctx.reporepo ~handle
      in
      if stripped > 0 then
        Fmt.pr "Stripped patches+extra-files from %d baked package(s)@."
          stripped
    end;
    auto_commit ~sys:ctx.sys ~reporepo:ctx.reporepo
      ~op:(Fmt.str "bump %s" handle)

  let check_all_overrides ~url ~ref_ ~toolchain ~depend_specs ~default =
    if
      url <> None || ref_ <> None || toolchain <> None || depend_specs <> []
      || default <> None
    then begin
      Fmt.epr
        "oi repo bump --all: per-entry overrides (--url, --ref, --toolchain, \
         --depend, --default) are not supported with --all.@.";
      exit 1
    end

  let bump_all ~reporepo ~bump_one_handle =
    let entries = Oi.Source.Reporepo.load ~path:reporepo in
    let handles =
      entries
      |> List.map (fun (e : Oi.Source.Reporepo.entry) -> e.handle)
      |> List.sort_uniq String.compare
    in
    List.iter
      (fun h ->
        Fmt.pr "@.%a@." Oi.Style.pp_header_string ("== " ^ h ^ " ==");
        try bump_one_handle h
        with exn ->
          Fmt.pr "  %a %s: %s@." Oi.Style.pp_error_string "error" h
            (Printexc.to_string exn))
      handles

  let cmd =
    let run (c : Terms.common) reporepo reporepo_url handle_opt all url ref_
        toolchain depend_specs default no_bake rebake upload_archives
        archives_url =
      Harness.run @@ fun ~sw env ->
      let { Harness.proc_mgr; fs; clock; sys; platform; os_key; cache; _ } =
        Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
          c.cache_dir
      in
      if rebake && no_bake then begin
        Fmt.epr "oi repo bump: --rebake and --no-bake are mutually exclusive.@.";
        exit 1
      end;
      if upload_archives && no_bake then begin
        Fmt.epr
          "oi repo bump: --upload-archives requires baking; drop --no-bake.@.";
        exit 1
      end;
      let upload_on_baked =
        make_upload_callback ~sys ~upload_archives ~archives_url
      in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
        ~url:reporepo_url ();
      let depends =
        match depend_specs with
        | [] -> None
        | _ -> Some (List.map parse_depend_spec depend_specs)
      in
      let d10 =
        Oi.Pipeline.d10 ~sys ~fs ~clock:(clock :> D10.Config.clk) ~cache ~os_key
      in
      let cache_root = Oi.Cache.root_s cache in
      let ctx =
        {
          proc_mgr;
          fs;
          sys;
          d10;
          cache_root;
          platform;
          reporepo;
          no_bake;
          rebake;
          upload_on_baked;
        }
      in
      let bump_one_handle h =
        bump_and_bake ~ctx ~url ~ref_ ~toolchain ~depends ~default h
      in
      match (all, handle_opt) with
      | false, None ->
          Fmt.epr "oi repo bump: pass HANDLE or --all.@.";
          exit 1
      | true, Some _ ->
          Fmt.epr "oi repo bump: HANDLE and --all are mutually exclusive.@.";
          exit 1
      | true, None ->
          check_all_overrides ~url ~ref_ ~toolchain ~depend_specs ~default;
          bump_all ~reporepo ~bump_one_handle
      | false, Some handle -> bump_one_handle handle
    in
    let handle =
      Arg.(
        value
        & pos 0 (some string) None
        & info ~docv:"HANDLE" ~doc:"Overlay handle to bump." [])
    in
    let all =
      Arg.(
        value & flag
        & info
            ~doc:
              "Bump every overlay. Per-entry overrides ($(b,--url), \
               $(b,--ref), $(b,--toolchain), $(b,--depend), $(b,--default)) \
               are forbidden."
            [ "all" ])
    in
    let url =
      Arg.(
        value
        & opt (some string) None
        & info ~docv:"URL"
            ~doc:"Override the upstream URL recorded by the overlay." [ "url" ])
    in
    let default =
      let set =
        Arg.(
          value & flag
          & info
              ~doc:
                "Mark this toolchain as the default. Auto-clears any other \
                 default. Requires $(b,x-oi-toolchain-name)."
              [ "default" ])
      in
      let unset =
        Arg.(
          value & flag
          & info
              ~doc:
                "Clear the default-toolchain flag. Solving commands without \
                 $(b,--toolchain) then hard-error."
              [ "no-default" ])
      in
      let combine s u =
        match (s, u) with
        | true, true ->
            `Error (false, "--default and --no-default are mutually exclusive")
        | true, false -> `Ok (Some true)
        | false, true -> `Ok (Some false)
        | false, false -> `Ok None
      in
      Term.(ret (const combine $ set $ unset))
    in
    let info =
      Cmd.info "bump" ~doc:"Re-pin an overlay to its current upstream commit"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Re-fetch the upstream tip of $(b,HANDLE)'s tracked branch, \
               record a new $(b,YYYYMMDD.N) entry, sha-pin every package URL, \
               and rewrite $(b,<reporepo>/v2/<HANDLE>/). Pass $(b,--all) for \
               every overlay.";
            `P
              "Bakes consolidated source archives for new or changed packages, \
               then strips $(b,patches:) and $(b,extra-files:) from baked opam \
               files (the archive subsumes them).";
            `P
              "Idempotent: prints $(b,No change) when commit, URL, ref, \
               depends, and flags are unchanged. The $(b,v2/) tree is rebuilt \
               either way.";
            `P
              "Non-base overlays auto-relock against the current \
               $(b,default)/$(b,relocatable), or the toolchain's own base set \
               when $(b,x-oi-toolchain) is set. $(b,--depend) overrides.";
          ]
    in
    let no_bake =
      Arg.(
        value & flag
        & info
            ~doc:
              "Skip the bake step. Opam files reference upstream $(b,url{src; \
               checksum}) only."
            [ "no-bake" ])
    in
    let rebake =
      Arg.(
        value & flag
        & info
            ~doc:
              "Force a full re-bake of every package's $(b,x-d10-archive), \
               even unchanged ones. Use after a bake-time transform changes. \
               Mutually exclusive with $(b,--no-bake)."
            [ "rebake" ])
    in
    let upload_archives =
      Arg.(
        value & flag
        & info
            ~doc:
              "Upload each freshly baked $(b,x-d10-archive) tarball $(i,and \
               its $(b,.json) source-manifest sidecar) to $(b,--archives-url) \
               via $(b,s3cmd put). Requires a working $(b,~/.s3cfg) (or \
               $(b,OI_S3CFG) pointing at a config). Already-present archives \
               are not re-uploaded (we only upload what was freshly baked this \
               run)."
            [ "upload-archives" ])
    in
    let archives_url =
      Arg.(
        value
        & opt string "s3://oiu/d10ir-archives/"
        & info ~docv:"URL"
            ~doc:
              "Destination URL prefix for $(b,--upload-archives). The full \
               object keys are $(b,<URL>/<sha>.tar.zst) and \
               $(b,<URL>/<sha>.json) (the source-manifest sidecar)."
            [ "archives-url" ])
    in
    Cmd.v info
      Term.(
        const run $ Terms.common $ reporepo_term $ reporepo_url_term $ handle
        $ all $ url $ ref_term $ toolchain_repo_term $ depend_term $ default
        $ no_bake $ rebake $ upload_archives $ archives_url)
end

module Bake = struct
  (* Read [x-d10-archive] off a baked opam file. Used to collect the
     set of shas to publish for a given handle. *)
  let read_archive_sha opam_path =
    try
      OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw opam_path))
      |> Oi.Keys.read_string_ext Oi.Keys.d10_archive
    with Failure _ | Sys_error _ -> None

  let collect_handle_shas ~reporepo ~handle =
    let acc = ref [] in
    Bump.iter_handle_opams ~reporepo ~handle
      (fun ~pkg:_ ~version:_ ~pkg_dir:_ ~opam_path ->
        match read_archive_sha opam_path with
        | Some sha -> acc := sha :: !acc
        | None -> ());
    List.sort_uniq String.compare !acc

  (* Bake every missing archive for a handle, strip files dirs, then
     publish the resulting shas as hardlinks into [to_dir]. *)
  let bake_one ~proc_mgr ~fs ~d10 ~cache_root ~platform ~cache ~reporepo ~to_dir
      handle =
    let missing = Bump.count_packages_missing_archive ~reporepo ~handle in
    if missing = 0 then
      Fmt.pr "@.%a %s: every package already baked@." Oi.Style.pp_ok_string "✓"
        handle
    else begin
      Fmt.pr "@.%a %s: baking %d missing archive(s)...@."
        Oi.Style.pp_info_string "▸" handle missing;
      let baked, failed =
        Bump.bake_changed_archives ~proc_mgr ~fs ~d10 ~cache_root ~platform
          ~reporepo ~handle ()
      in
      Fmt.pr "  %d baked, %d failed@." baked failed
    end;
    (* Same strip step as [oi repo bump]: the freshly-baked archives
       subsume each package's [files/] dir. *)
    let stripped = Bump.strip_resolved_artifacts ~fs ~reporepo ~handle in
    if stripped > 0 then
      Fmt.pr "  Stripped patches+extra-files from %d baked package(s)@."
        stripped;
    let shas = collect_handle_shas ~reporepo ~handle in
    let { Oi.D10ir_archives.linked; present; missing } =
      Oi.D10ir_archives.publish_shas ~cache ~output:to_dir shas
    in
    let total = linked + present in
    Fmt.pr
      "  %a %d archive(s) at %s/d10ir-archives/ (%d new, %d already present)@."
      Oi.Style.pp_ok_string "✓" total to_dir linked present;
    if missing > 0 then
      Fmt.pr
        "  %a %d archive(s) referenced by %s opams but not in local cache; run \
         [oi repo bump %s] to bake them@."
        Oi.Style.pp_warn_string "!" missing handle handle

  let bake_all_handles ~reporepo ~bake_handle =
    let entries = Oi.Source.Reporepo.load ~path:reporepo in
    let handles =
      List.map (fun (e : Oi.Source.Reporepo.entry) -> e.handle) entries
      |> List.sort_uniq String.compare
    in
    if handles = [] then
      Fmt.epr "oi repo bake: no overlays found in %s@." reporepo
    else List.iter bake_handle handles

  let cmd =
    let run (c : Terms.common) reporepo reporepo_url handle_opt to_dir =
      Harness.run @@ fun ~sw env ->
      let { Harness.proc_mgr; fs; clock; sys; platform; os_key; cache; _ } =
        Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
          c.cache_dir
      in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
        ~url:reporepo_url ();
      let d10 =
        Oi.Pipeline.d10 ~sys ~fs ~clock:(clock :> D10.Config.clk) ~cache ~os_key
      in
      let cache_root = Oi.Cache.root_s cache in
      let bake_handle h =
        bake_one ~proc_mgr ~fs ~d10 ~cache_root ~platform ~cache ~reporepo
          ~to_dir h
      in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / to_dir);
      match handle_opt with
      | Some handle -> bake_handle handle
      | None -> bake_all_handles ~reporepo ~bake_handle
    in
    let handle =
      Arg.(
        value
        & pos 0 (some string) None
        & info ~docv:"HANDLE" ~doc:"Overlay to bake. Omit for every overlay." [])
    in
    let to_dir =
      Arg.(
        required
        & opt (some string) None
        & info ~docv:"DIR"
            ~doc:
              "Output directory. Archives are published as \
               $(b,DIR/d10ir-archives/<sha>.tar.zst)."
            [ "to" ])
    in
    let info =
      Cmd.info "bake"
        ~doc:
          "Bake an overlay's consolidated source archives and publish them to \
           a directory"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Bake every package in $(b,<reporepo>/v2/HANDLE/) (or every \
               overlay) that lacks $(b,x-d10-archive): fetch sources, apply \
               patches, write a consolidated tarball, and record its sha. Then \
               strip $(b,patches:) and $(b,extra-files:) from baked opam \
               files.";
            `P
              "Hardlink every $(b,x-d10-archive) into $(b,DIR/d10ir-archives/) \
               — the layout $(b,oi build) expects from a remote registry.";
            `P
              "No-op on packages already carrying $(b,x-d10-archive). $(b,oi \
               repo bump) runs the same bake as a side effect.";
            `S Manpage.s_examples;
            `Pre
              "  oi repo bake @avsm --to=./registry\n\
              \  oi repo bake --to=./registry         # every overlay\n\
              \  oi build --all --export=./registry   # full registry, \
               including layers";
          ]
    in
    Cmd.v info
      Term.(
        const run $ Terms.common $ reporepo_term $ reporepo_url_term $ handle
        $ to_dir)
end

module Index = struct
  let cmd =
    let run (c : Terms.common) registry to_dir =
      Harness.run @@ fun ~sw env ->
      let { Harness.fs; clock; sys; os_key; cache; _ } =
        Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
          c.cache_dir
      in
      Registry_export.run ~fs
        ~clock:(clock :> D10.Config.clk)
        ~sys ~os_key ~cache ~registry ~output:to_dir
    in
    let to_dir =
      Arg.(
        required
        & opt (some string) None
        & info ~docv:"DIR"
            ~doc:
              "Output directory. Layer tarballs are written as \
               $(b,DIR/<os_key>/<hash>.tar.zst); the rebuilt sqlite index as \
               $(b,DIR/<os_key>/index.db)."
            [ "to" ])
    in
    let info =
      Cmd.info "index"
        ~doc:"(Re)build a registry's sqlite index from the local layer cache"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Re-publishes the local cache as a registry tree at $(b,DIR): \
               exports every succeeded layer as $(b,<os_key>/<hash>.tar.zst), \
               rebuilds $(b,<os_key>/index.db) from the layer dirs, and \
               records each tarball's sha256+size. Idempotent — re-running \
               only adds newly-built layers and re-encodes the index.";
            `P
              "Reads overlay attribution from each layer's \
               $(b,provenance.json) sidecar (written by $(b,oi build) at \
               commit time). Layers whose sidecar is missing land in the index \
               with $(b,overlay_handle = NULL).";
            `P
              "Useful after fixing a stale or corrupt index without re-running \
               the build phase. $(b,oi build --export=DIR) rolls this together \
               with the build itself.";
            `S Manpage.s_examples;
            `Pre
              "  oi repo index --to=./registry\n\
              \  oi build --export=./registry   # build + bake + index";
          ]
    in
    Cmd.v info Term.(const run $ Terms.common $ Terms.registry $ to_dir)
end

module Set_roots = struct
  let cmd =
    (* Parse a PKG token: a comma-separated list becomes a multi-package
     solve group; a bare name becomes a singleton group. Empty tokens
     between commas are dropped. *)
    let parse_group token =
      String.split_on_char ',' token
      |> List.map String.trim
      |> List.filter (fun s -> s <> "")
    in
    let run () reporepo reporepo_url handle pkgs =
      Harness.run @@ fun ~sw:_ env ->
      let proc_mgr = Eio.Stdenv.process_mgr env in
      let fs = Eio.Stdenv.fs env in
      let sys =
        D10.Sysops.v ~stdout:(Eio.Stdenv.stdout env)
          ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs
          ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ()
      in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
        ~url:reporepo_url ();
      let groups =
        List.filter_map
          (fun t -> match parse_group t with [] -> None | g -> Some g)
          pkgs
      in
      (match
         Oi.Source.Reporepo.bump ~fs ~sys ~path:reporepo ~handle
           ~root_packages:groups ()
       with
      | `Bumped e ->
          Fmt.pr "Bumped %s to %s (root-packages: %d entr%s)@." e.handle
            e.version
            (List.length e.root_packages)
            (if List.length e.root_packages = 1 then "y" else "ies")
      | `Unchanged e ->
          Fmt.pr "No change: %s.%s already has that root-packages list.@."
            e.handle e.version);
      auto_commit ~sys ~reporepo ~op:(Fmt.str "set-roots %s" handle)
    in
    let handle =
      Arg.(
        required
        & pos 0 (some string) None
        & info ~docv:"HANDLE" ~doc:"Overlay to update." [])
    in
    let pkgs =
      Arg.(
        value & pos_right 0 string []
        & info ~docv:"PKG"
            ~doc:
              "Root package group. Each argument is one solve group; a \
               comma-separated list solves together (compiler variants). No \
               $(b,PKG) clears the list."
            [])
    in
    let info =
      Cmd.info "set-roots"
        ~doc:"Record which packages should be pre-built for an overlay"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Write $(b,x-root-packages: [...]) on a new bumped version of \
               $(b,HANDLE). $(b,oi build --all) iterates each group: bare \
               names build as $(b,@HANDLE/PKG); comma-separated lists solve \
               together. Stamps $(b,YYYYMMDD.N); previous entry is kept.";
            `S Manpage.s_examples;
            `P "Three independent root packages:";
            `Pre "  oi repo set-roots relocatable dune utop merlin";
            `P "A compiler variant alongside plain packages:";
            `Pre
              "  oi repo set-roots relocatable \
               ocaml-option-flambda,ocaml-option-static,ocaml dune utop";
          ]
    in
    Cmd.v info
      Term.(
        const run $ Terms.log $ reporepo_term $ reporepo_url_term $ handle
        $ pkgs)
end

module Remove = struct
  let cmd =
    let run () reporepo reporepo_url handle_spec =
      Harness.run @@ fun ~sw:_ env ->
      let proc_mgr = Eio.Stdenv.process_mgr env in
      let fs = Eio.Stdenv.fs env in
      let sys =
        D10.Sysops.v ~stdout:(Eio.Stdenv.stdout env)
          ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs
          ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ()
      in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
        ~url:reporepo_url ();
      let handle, version = parse_handle_version handle_spec in
      Oi.Source.Reporepo.remove ~fs ~path:reporepo ~handle ?version ();
      Fmt.pr "Removed %s%s from %s@." handle
        (match version with None -> " (all versions)" | Some v -> "." ^ v)
        reporepo;
      auto_commit ~sys ~reporepo
        ~op:
          (Fmt.str "remove %s%s" handle
             (match version with None -> "" | Some v -> "." ^ v))
    in
    let handle_spec =
      Arg.(
        required
        & pos 0 (some string) None
        & info ~docv:"HANDLE[=VERSION]"
            ~doc:"Overlay to remove. Bare $(b,HANDLE) removes every version." [])
    in
    let info =
      Cmd.info "remove" ~doc:"Delete an overlay from the reporepo"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Delete an overlay entry. $(b,HANDLE=VERSION) removes one \
               version; bare $(b,HANDLE) removes all.";
            `P
              "Only the reporepo is edited. Cloned overlay bundles under the \
               data directory are kept. Use $(b,oi clean --repos) to remove \
               them.";
          ]
    in
    Cmd.v info
      Term.(
        const run $ Terms.log $ reporepo_term $ reporepo_url_term $ handle_spec)
end

module Push = struct
  let print_step_outcome = function
    | Oi.Source.Reporepo.Step_commit { files = [] } ->
        Fmt.pr "  commit: %a (working tree clean)@." Oi.Style.pp_dim_string
          "skipped"
    | Oi.Source.Reporepo.Step_commit { files } ->
        Fmt.pr "  commit: %a (%d file(s))@." Oi.Style.pp_ok_string "ok"
          (List.length files);
        List.iter (fun f -> Fmt.pr "    %s@." f) files
    | Oi.Source.Reporepo.Step_pull { commits = 0 } ->
        Fmt.pr "  pull:   %a (already up to date)@." Oi.Style.pp_dim_string
          "skipped"
    | Oi.Source.Reporepo.Step_pull { commits } ->
        Fmt.pr "  pull:   %a (%d new upstream commit(s))@."
          Oi.Style.pp_ok_string "ok" commits
    | Oi.Source.Reporepo.Step_push { commits = 0 } ->
        Fmt.pr "  push:   %a (nothing to push)@." Oi.Style.pp_dim_string
          "skipped"
    | Oi.Source.Reporepo.Step_push { commits } ->
        Fmt.pr "  push:   %a (%d local commit(s) sent)@." Oi.Style.pp_ok_string
          "ok" commits

  let cmd =
    let run () reporepo reporepo_url push_url =
      Harness.run @@ fun ~sw:_ env ->
      let proc_mgr = Eio.Stdenv.process_mgr env in
      let fs = Eio.Stdenv.fs env in
      let sys =
        D10.Sysops.v ~stdout:(Eio.Stdenv.stdout env)
          ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs
          ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ()
      in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
        ~url:reporepo_url ();
      Fmt.pr "%a %s@." Oi.Style.pp_header_string "reporepo:" reporepo;
      (match push_url with
      | None -> ()
      | Some u ->
          Oi.Source.Reporepo.set_push_url ~sys ~path:reporepo u;
          Fmt.pr "%a push URL of origin set to %s@." Oi.Style.pp_ok_string "ok"
            u);
      let on_step_start n title =
        Fmt.pr "@.%a %s@." Oi.Style.pp_header_string (Fmt.str "[%d/3]" n) title
      in
      let outcome =
        Oi.Source.Reporepo.push ~on_step_start ~sys ~path:reporepo ()
      in
      Fmt.pr "@.%a@." Oi.Style.pp_header_string "summary:";
      List.iter print_step_outcome outcome
    in
    let push_url =
      Arg.(
        value
        & opt (some string) None
        & info [ "push-url" ] ~docv:"URL"
            ~doc:
              "Set $(b,origin)'s push URL via $(b,git remote set-url --push). \
               Persistent; fetch URL is left alone.")
    in
    let info =
      Cmd.info "push"
        ~doc:"Pull, commit local edits, and push the reporepo to its remote"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Three steps: auto-commit any uncommitted changes, $(b,git pull \
               --rebase), then $(b,git push) if ahead. Idempotent.";
            `P
              "Authentication uses the system $(b,git) configuration. $(b,oi) \
               shells out and never handles credentials.";
            `P
              "$(b,--push-url URL) sets the push remote on the local checkout. \
               Useful when the clone URL is read-only HTTPS but push goes over \
               SSH.";
            `S Manpage.s_examples;
            `P "Bump and publish:";
            `Pre "  oi repo bump avsm && oi repo push";
            `P "Switch the push URL to SSH:";
            `Pre
              "  oi repo push --push-url \
               git@tangled.org:anil.recoil.org/reporepo.git";
          ]
    in
    Cmd.v info
      Term.(
        const run $ Terms.log $ reporepo_term $ reporepo_url_term $ push_url)
end

(* -- repo lint ----------------------------------------------------------- *)

(* Shape and integrity checks for a reporepo on disk. The contract every
   command's [Pipeline.pick_toolchain] depends on: exactly one
   default toolchain, every [x-oi-toolchain] reference resolves, every
   toolchain definition carries the fields the resolver and Toolchain
   pipeline read. Loading a reporepo through {!Reporepo.load} already
   enforces some invariants ("at most one default", well-formed opam
   files); [oi repo lint] surfaces the rest as a precommit-grade check. *)
module Lint = struct
  (* Each problem carries a stable label ([where]) plus the absolute
     opam-file path(s) to jump to. Reporepo-wide problems (multiple
     defaults, missing default) attach every offending entry's
     [opam_path] so the user can fix them all in one pass. *)
  type problem = { where : string; paths : string list; msg : string }

  let version_compare a b =
    OpamPackage.Version.compare
      (OpamPackage.Version.of_string a)
      (OpamPackage.Version.of_string b)

  let latest_per_handle (entries : Oi.Source.Reporepo.entry list) =
    let tbl = Hashtbl.create 32 in
    List.iter
      (fun (e : Oi.Source.Reporepo.entry) ->
        match Hashtbl.find_opt tbl e.handle with
        | None -> Hashtbl.replace tbl e.handle e
        | Some (prev : Oi.Source.Reporepo.entry)
          when version_compare e.version prev.version > 0 ->
            Hashtbl.replace tbl e.handle e
        | Some _ -> ())
      entries;
    tbl

  (* Mutable accumulator for problems found while scanning. [add_problem]
     appends in reverse order; the public collector flips at the end. *)
  type acc = { mutable items : problem list }

  let add_problem acc ~where ~paths fmt =
    Fmt.kstr (fun msg -> acc.items <- { where; paths; msg } :: acc.items) fmt

  (* Basic shape checks: empty handle/version, url/commit consistency. *)
  let check_entry_shape ~acc ~where (e : Oi.Source.Reporepo.entry) =
    let here fmt = add_problem acc ~where ~paths:[ e.opam_path ] fmt in
    if e.handle = "" then here "empty handle";
    if e.version = "" then here "empty version";
    if e.url = "" && e.commit <> "" then
      here "[commit] is set (%s) but [url] is empty" e.commit;
    if e.url <> "" && e.commit = "" then
      here "[url: %s] is set but no commit is pinned" e.url;
    if e.commit <> "" && not (Oi.Source.Reporepo.is_sha_string e.commit) then
      here "pinned commit %S is not a 40-char hex sha" e.commit

  (* Toolchain-definition fields must appear together. *)
  let check_toolchain_fields ~acc ~where (e : Oi.Source.Reporepo.entry) =
    let here fmt = add_problem acc ~where ~paths:[ e.opam_path ] fmt in
    match e.toolchain_name with
    | None ->
        if e.relocatable <> None then
          here
            "[%s] is set but [%s] is not — only toolchain definitions take \
             this field"
            Oi.Keys.relocatable Oi.Keys.toolchain_name;
        if e.toolchain_roots <> [] then
          here
            "[%s] is set but [%s] is not — only toolchain definitions take \
             this field"
            Oi.Keys.toolchain_roots Oi.Keys.toolchain_name
    | Some name ->
        if name = "" then here "[%s] is empty" Oi.Keys.toolchain_name;
        if e.relocatable = None then
          here "toolchain %S missing [%s] (must be true or false)" name
            Oi.Keys.relocatable;
        if e.toolchain_roots = [] then
          here "toolchain %S has empty [%s]" name Oi.Keys.toolchain_roots;
        if e.toolchain_compiler = None then
          here "toolchain %S missing [%s] (e.g. \"ocaml-base-compiler.5.4.1\")"
            name Oi.Keys.toolchain_compiler

  let check_toolchain_ref ~acc ~where ~toolchain_names ~known_toolchain
      (e : Oi.Source.Reporepo.entry) =
    match e.toolchain with
    | None -> ()
    | Some t when known_toolchain t -> ()
    | Some t ->
        add_problem acc ~where ~paths:[ e.opam_path ]
          "[%s: %s] does not match any [%s] in this reporepo (known: %s)"
          Oi.Keys.toolchain t Oi.Keys.toolchain_name
          (if toolchain_names = [] then "none"
           else String.concat ", " toolchain_names)

  (* For url-bearing entries, the materialised [v2/<handle>/] tree must exist
     on disk. A missing tree means "ran [oi repo add] / received a checkout
     that hasn't been bumped"; downstream commands fail confusingly without
     this check. *)
  let check_materialised_tree ~reporepo ~acc ~where
      (e : Oi.Source.Reporepo.entry) =
    if e.url <> "" then
      let v2 = reporepo / "v2" / e.handle / "packages" in
      if not (Sys.file_exists v2) then
        add_problem acc ~where ~paths:[ e.opam_path ]
          "missing materialised tree at v2/%s/packages/ — run 'oi repo bump \
           %s' to populate"
          e.handle e.handle

  (* Append the appropriate out-of-date / unknown-handle problem for one
     [(handle, pinned)] depend on a latest-version entry. *)
  let check_one_depend ~acc ~where ~latest (e : Oi.Source.Reporepo.entry)
      (h, pinned) =
    let here fmt = add_problem acc ~where ~paths:[ e.opam_path ] fmt in
    match Hashtbl.find_opt latest h with
    | None ->
        here "depends on unknown handle %S (no entry for it in this reporepo)" h
    | Some (latest_dep : Oi.Source.Reporepo.entry)
      when version_compare pinned latest_dep.version < 0 -> (
        match latest_dep.toolchain_name with
        | Some tname ->
            here
              "uses out-of-date toolchain %S: pinned [%s = %s] but latest is \
               %s. Run 'oi repo bump %s' to update."
              tname h pinned latest_dep.version e.handle
        | None ->
            here
              "out-of-date depend [%s = %s]: latest is %s. Run 'oi repo bump \
               %s' to update."
              h pinned latest_dep.version e.handle)
    | Some _ -> ()

  (* Out-of-date dependencies on the latest version of each handle. Older
     entries are kept around for history with old depends — skip them. *)
  let check_dependencies ~acc ~where ~latest (e : Oi.Source.Reporepo.entry) =
    match Hashtbl.find_opt latest e.handle with
    | Some (l : Oi.Source.Reporepo.entry) when l.opam_path = e.opam_path ->
        List.iter
          (fun (h, vopt) ->
            match vopt with
            | None -> ()
            | Some pinned -> check_one_depend ~acc ~where ~latest e (h, pinned))
          e.depends
    | _ -> ()

  let check_entry ~reporepo ~toolchain_names ~known_toolchain ~latest ~acc
      (e : Oi.Source.Reporepo.entry) =
    let where = Fmt.str "%s.%s" e.handle e.version in
    check_entry_shape ~acc ~where e;
    check_toolchain_fields ~acc ~where e;
    check_toolchain_ref ~acc ~where ~toolchain_names ~known_toolchain e;
    check_materialised_tree ~reporepo ~acc ~where e;
    check_dependencies ~acc ~where ~latest e

  let report_no_default ~acc ~toolchain_names =
    let hint =
      if toolchain_names = [] then "no toolchain definitions found"
      else
        Fmt.str "mark one with: oi repo bump <handle> --default. Known: %s"
          (String.concat ", " toolchain_names)
    in
    add_problem acc ~where:"(reporepo)" ~paths:[]
      "no entry has [%s: true] — %s. Without a default, [oi run] / [oi sync] / \
       etc. hard-error when the user omits [--toolchain]."
      Oi.Keys.default_toolchain hint

  let report_many_defaults ~acc many =
    let labels =
      List.map
        (fun (e : Oi.Source.Reporepo.entry) ->
          Fmt.str "%s.%s" e.handle e.version)
        many
    in
    let paths =
      List.map (fun (e : Oi.Source.Reporepo.entry) -> e.opam_path) many
    in
    add_problem acc ~where:"(reporepo)" ~paths
      "multiple handles flagged [%s: true]: %s — only one toolchain handle may \
       be the default. Clear the flag on the losing handle with 'oi repo bump \
       <handle> --no-default'."
      Oi.Keys.default_toolchain
      (String.concat ", " labels)

  let report_stale_default ~acc ~latest (e : Oi.Source.Reporepo.entry) =
    let where = Fmt.str "%s.%s" e.handle e.version in
    add_problem acc ~where ~paths:[ e.opam_path ]
      "stale [%s: true] on a non-latest version of %s (latest is %s). Clear \
       with 'oi repo bump %s --no-default' on this version, or just remove \
       this opam file if it's no longer needed."
      Oi.Keys.default_toolchain e.handle
      (match Hashtbl.find_opt latest e.handle with
      | Some (l : Oi.Source.Reporepo.entry) -> l.version
      | None -> "?")
      e.handle

  (* Default-toolchain validation. Match {!Reporepo.load}'s semantics: count
     one default per HANDLE using its latest version. Older versions of the
     same handle still carrying the flag are stale (an older bump didn't get
     cleared by a later --default rotation); they don't break [load] but the
     user should clear them to avoid confusion. *)
  let check_defaults ~acc ~toolchain_names ~latest entries =
    let latest_defaults =
      Hashtbl.fold
        (fun _ (e : Oi.Source.Reporepo.entry) acc ->
          if e.default_toolchain then e :: acc else acc)
        latest []
    in
    let stale_defaults =
      List.filter
        (fun (e : Oi.Source.Reporepo.entry) ->
          e.default_toolchain
          &&
          match Hashtbl.find_opt latest e.handle with
          | Some l -> l.opam_path <> e.opam_path
          | None -> false)
        entries
    in
    (match latest_defaults with
    | [ _ ] -> () (* exactly one handle is default — fine *)
    | [] -> report_no_default ~acc ~toolchain_names
    | many -> report_many_defaults ~acc many);
    List.iter (report_stale_default ~acc ~latest) stale_defaults

  let collect_problems ~reporepo (entries : Oi.Source.Reporepo.entry list) :
      problem list =
    let acc = { items = [] } in
    let toolchain_names =
      List.filter_map
        (fun (e : Oi.Source.Reporepo.entry) -> e.toolchain_name)
        entries
      |> List.sort_uniq String.compare
    in
    let known_toolchain n = List.mem n toolchain_names in
    let latest = latest_per_handle entries in
    List.iter
      (check_entry ~reporepo ~toolchain_names ~known_toolchain ~latest ~acc)
      entries;
    check_defaults ~acc ~toolchain_names ~latest entries;
    List.rev acc.items

  let print_problem { where; paths; msg } =
    Fmt.pr "%a %s: %s@." Oi.Style.pp_error_string "error:" where msg;
    List.iter (fun p -> Fmt.pr "    %a@." Oi.Style.pp_dim_string p) paths

  let cmd =
    let run () reporepo reporepo_url =
      Harness.run @@ fun ~sw:_ env ->
      let proc_mgr = Eio.Stdenv.process_mgr env in
      let fs = Eio.Stdenv.fs env in
      let sys =
        D10.Sysops.v ~stdout:(Eio.Stdenv.stdout env)
          ~stderr:(Eio.Stdenv.stderr env) ~proc_mgr ~fs
          ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ()
      in
      Oi.Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path:reporepo
        ~url:reporepo_url ();
      let entries = Oi.Source.Reporepo.load ~path:reporepo in
      let problems = collect_problems ~reporepo entries in
      if problems = [] then begin
        Fmt.pr "%a %d entries, no problems found.@." Oi.Style.pp_ok_string "OK:"
          (List.length entries);
        exit 0
      end
      else begin
        List.iter print_problem problems;
        Fmt.pr "@.%d problem(s) in %s@." (List.length problems) reporepo;
        exit 1
      end
    in
    let info =
      Cmd.info "lint" ~doc:"Validate a reporepo's well-formedness"
        ~man:
          [
            `S Manpage.s_description;
            `P
              "Check every entry for the invariants $(b,oi) relies on. Each \
               problem prints the offending opam file path so it can be opened \
               and fixed directly.";
            `I
              ( "Default toolchain.",
                "Exactly one toolchain handle's latest version carries \
                 $(b,x-oi-default-toolchain: true). Older versions of that \
                 handle still flagged are reported as stale." );
            `I
              ( "Toolchain references.",
                "Every $(b,x-oi-toolchain: NAME) resolves to some entry's \
                 $(b,x-oi-toolchain-name)." );
            `I
              ( "Toolchain definitions.",
                "Entries with $(b,x-oi-toolchain-name) also declare \
                 $(b,x-oi-relocatable), $(b,x-oi-toolchain-compiler), and a \
                 non-empty $(b,x-oi-toolchain-roots)." );
            `I
              ( "Field discipline.",
                "Toolchain-only fields are not set on non-toolchain entries." );
            `I
              ( "Source identity.",
                "$(b,url:) and $(b,commit:) are either both set (with a \
                 40-char sha) or both empty." );
            `I
              ( "Materialised tree.",
                "Url-bearing handles have a $(b,v2/HANDLE/packages/) directory \
                 — runs of $(b,oi repo bump) keep this populated." );
            `I
              ( "Out-of-date dependencies.",
                "Each handle's latest version's $(b,depends:) pins are \
                 compared to the latest of each depended handle in the \
                 reporepo. Out-of-date toolchain pins are called out \
                 specially." );
            `P "Exits non-zero on failure.";
          ]
    in
    Cmd.v info Term.(const run $ Terms.log $ reporepo_term $ reporepo_url_term)
end

let cmd =
  let info =
    Cmd.info "repo"
      ~doc:"Manage the directory of package-source bundles you pull from"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "A $(i,reporepo) is a directory of pinned opam-repository commits. \
             Each $(i,handle) names a package set; entries with \
             $(b,x-oi-toolchain-name) define toolchains.";
          `P
            "Reference handles as $(b,@HANDLE/PKG), $(b,--with-repo=@HANDLE), \
             or $(b,x-repos: [\"@HANDLE\"]) in $(b,*.opam).";
          `P
            "First $(b,oi repo) command auto-clones the upstream reporepo. The \
             working copy is yours to edit, commit, and push.";
          `S "FILES";
          `I
            ( "$(b,\\$OI_REPOREPO) (default: $(b,\\$OI_DATA_DIR/reporepo))",
              "Local git working copy." );
          `S "EXAMPLE WORKFLOW";
          `Pre
            "  oi repo list                 # auto-clones on first use\n\
            \  oi repo add h URL            # pin somebody's overlay\n\
            \  oi run @h/some-tool          # use it\n\
            \  oi repo bump h               # pick up upstream commits\n\
            \  oi repo push                 # commit + push the bumps";
        ]
  in
  Cmd.group info
    [
      Ls.cmd;
      Show.cmd;
      Add.cmd;
      Bump.cmd;
      Bake.cmd;
      Index.cmd;
      Set_roots.cmd;
      Remove.cmd;
      Push.cmd;
      Lint.cmd;
    ]
