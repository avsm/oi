[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

let ( / ) = Filename.concat
let log_src = Logs.Src.create "oi.toolchain"

module Log = (val Logs.src_log log_src : Logs.LOG)

type info = {
  handle : string;
  ocaml_version : string;
  install_prefix : string;
  hash : string;
  relocatable : bool;
      (** [true] when the toolchain's compiler can be installed into a per-solve
          consumer prefix. Skips the fixed-prefix install and the PATH/OCAMLPATH
          layering, so the binary cache pipeline works end-to-end the same way
          it does in the no-toolchain flow. The version pin in
          {!opam_ctx_of_info} still drives compiler selection. [false] (e.g.
          oxcaml) means the compiler is provided by a preinstalled system
          package at {!external_prefix}, layered via PATH like a relocatable
          one but never built. *)
  external_prefix : string option;
      (** [Some p] for a non-relocatable toolchain whose compiler is
          preinstalled at [p] (a system package: [/opt/oxcaml/<ver>] on Linux,
          [brew --prefix <cask>] on macOS). Resolved per-OS at {!resolve} time.
          [None] for relocatable toolchains. When set, [install_prefix = p] and
          oi never builds the compiler — it probes for [p] and PATH-layers it.
      *)
  external_brew : string option;
      (** [x-oi-external-brew] verbatim — the Homebrew cask/formula ref, kept
          so {!ensure_installed}'s "not found" hint can print the exact
          [brew install <ref>] line. [None] for relocatable toolchains. *)
  packages : OpamPackage.Set.t;
  compiler_name : OpamPackage.Name.t;
  root_names : OpamPackage.Name.Set.t;
  packages_dirs : string list;
  tools : string list;
  dep_handles : string list;
}

(* Probe for the compiler driver under a preinstalled external prefix. The
   oxcaml system package ships [bin/ocamlc] (a symlink to [ocamlc.opt]); its
   presence is our "the package is installed" signal. We deliberately don't
   own a [.oi-toolchain-ready] marker here — the prefix is system-managed and
   typically read-only. *)
let external_ready p = Sys.file_exists (p / "bin" / "ocamlc")

let is_ready info =
  info.relocatable
  ||
  match info.external_prefix with
  | Some p -> external_ready p
  | None -> false

let opam_ctx_of_info (info : info) : Solver.Ctx.toolchain =
  {
    install_prefix = info.install_prefix;
    hash = info.hash;
    relocatable = info.relocatable;
    packages = info.packages;
    root_names = info.root_names;
  }

let apply_conf info (conf : Solver.Ctx.conf) =
  match info with
  | None -> conf
  | Some (i : info) -> { conf with ocaml_version = i.ocaml_version }

(* -- Install root derivation -------------------------------------------- *)

(* Toolchains live under $XDG_CACHE_HOME so they sit alongside the rest
   of oi's cache. User said explicitly: use XDG_CACHE_HOME, not
   /opt-style system paths. *)
let default_root = Cache.toolchains_root

(* -- Reporepo-backed toolchain discovery ------------------------------ *)

(* All toolchains live in the reporepo as definition-only entries
   (url-less, depends-only) carrying [x-oi-toolchain-name],
   [x-oi-toolchain-compiler], [x-oi-relocatable], and
   [x-oi-toolchain-roots]. The reporepo handle (e.g.
   [toolchain-oxcaml]) and the CLI toolchain name (e.g. [oxcaml])
   live in separate namespaces; the latter is the [x-oi-toolchain-name]
   field. *)

(* Read the reporepo's entries, swallowing parse failures. The empty
   list is a real possibility on a fresh machine where the reporepo
   hasn't been cloned yet — the caller decides whether that's fatal. *)
let load_entries () =
  let path = Source.Reporepo.env_path () in
  if not (Sys.file_exists path) then []
  else try Source.Reporepo.load ~path with Error.E _ -> []

(* Find the latest reporepo entry that defines a toolchain with CLI
   name [name]. Multiple reporepo handles defining the same CLI name
   is an error (ambiguous lookup). *)
let find_entry_by_toolchain_name ~name =
  let entries = load_entries () in
  let handles =
    entries
    |> List.filter_map (fun (e : Source.Reporepo.entry) ->
        match e.toolchain_name with
        | Some n when n = name -> Some e.handle
        | _ -> None)
    |> List.sort_uniq String.compare
  in
  match handles with
  | [] -> None
  | [ h ] -> Source.Reporepo.latest entries ~handle:h
  | _ ->
      Error.fail_config_error
        "multiple reporepo handles define toolchain %S: %s — fix by removing \
         the duplicate definitions"
        name
        (String.concat ", " handles)

(* For [oi show] / man-page rendering: surface the URL of the FIRST
   depends overlay as the toolchain's "primary source". Toolchain
   entries are url-less, but their first depends overlay is the
   compiler-bearing one (e.g. [oxcaml] for the oxcaml toolchain), so
   that's the URL to show. *)
let url_of ~handle =
  match find_entry_by_toolchain_name ~name:handle with
  | None -> None
  | Some e -> (
      match e.depends with
      | [] -> None
      | (h, _) :: _ -> (
          let entries = load_entries () in
          match Source.Reporepo.latest entries ~handle:h with
          | Some dep when dep.url <> "" -> Some dep.url
          | _ -> None))

let depends_of ~handle =
  match find_entry_by_toolchain_name ~name:handle with
  | None -> None
  | Some e -> Some (List.map fst e.depends)

(* -- Listing for [oi config] ------------------------------------------- *)

type summary = {
  handle : string;
  url : string;
  ref_ : string option;
  relocatable : bool;
  is_default : bool;
  depends : string list;
  roots : string list;
  tools : string list;
}

(* For displaying the toolchain's "primary source" URL + ref in [oi
   config]. Same lookup as [url_of] but returns the ref too. *)
let primary_source ~entries (e : Source.Reporepo.entry) =
  match e.depends with
  | [] -> ("", None)
  | (h, _) :: _ -> (
      match Source.Reporepo.latest entries ~handle:h with
      | Some dep -> (dep.url, dep.ref_)
      | None -> ("", None))

let available () =
  let entries = load_entries () in
  let handles =
    entries
    |> List.filter_map (fun (e : Source.Reporepo.entry) ->
        match e.toolchain_name with Some _ -> Some e.handle | None -> None)
    |> List.sort_uniq String.compare
  in
  List.filter_map
    (fun h ->
      match Source.Reporepo.latest entries ~handle:h with
      | None -> None
      | Some e ->
          let cli_name =
            Stdlib.Option.value e.toolchain_name ~default:e.handle
          in
          let url, ref_ = primary_source ~entries e in
          let relocatable = Stdlib.Option.value e.relocatable ~default:true in
          let roots = List.flatten e.toolchain_roots in
          Some
            {
              handle = cli_name;
              url;
              ref_;
              relocatable;
              is_default = e.default_toolchain;
              depends = List.map fst e.depends;
              roots;
              tools = e.toolchain_tools;
            })
    handles

(* -- Resolve ------------------------------------------------------------ *)

(* Parse a [name] / [name.version] / [name=version] spec into a
   [(name, version opt)] pair. *)
let parse_spec s =
  match String.index_opt s '=' with
  | Some i ->
      let n = String.sub s 0 i in
      let v = String.sub s (i + 1) (String.length s - i - 1) in
      (n, Some v)
  | None -> (
      match String.index_opt s '.' with
      | Some i ->
          let n = String.sub s 0 i in
          let v = String.sub s (i + 1) (String.length s - i - 1) in
          (n, Some v)
      | None -> (s, None))

(* Derive the install-prefix hash. Combines [D10.Layer.hash] (which
   hashes the solved packages' opam [effective_part]) with the conf's
   platform fields. Same set of inputs that drive opam-0install's
   solve, so any change → fresh install_prefix → old prefix is left
   alone. [external_prefix] is folded in so that bumping the system
   package (e.g. /opt/oxcaml/5.2.0minus31 → minus32) invalidates every
   consumer layer even if the solved opam set were somehow unchanged. *)
let compute_hash ?external_prefix ~packages_dirs ~(conf : Solver.Ctx.conf) pkgs =
  let layer_hash = D10.Layer.hash ~packages_dirs pkgs in
  let material =
    String.concat "\n"
      ([
         conf.arch;
         conf.os;
         conf.os_distribution;
         conf.os_version;
         conf.os_family;
         layer_hash;
       ]
      @ match external_prefix with Some p -> [ p ] | None -> [])
  in
  Digest.string material |> Digest.to_hex

(* Pick the OCaml-version-string out of the solved package set,
   using the toolchain entry's [x-oi-toolchain-compiler] spec to
   identify which package the version should come from. The spec is
   required on every toolchain definition (validated at parse time
   in [Source.Reporepo.parse_entry_file]) so there's no fallback
   guess list anymore. *)
let pick_ocaml_version ~explicit_compiler pkgs =
  let name, _ = parse_spec explicit_compiler in
  List.find_opt
    (fun p -> OpamPackage.Name.to_string (OpamPackage.name p) = name)
    pkgs
  |> Stdlib.Option.map (fun p ->
      OpamPackage.Version.to_string (OpamPackage.version p))

(* Look up the toolchain entry by name and fail with a friendly error listing
   the known handles when there's no match. *)
let entry_or_fail ~handle ~path =
  match find_entry_by_toolchain_name ~name:handle with
  | Some e -> e
  | None ->
      let known =
        load_entries ()
        |> List.filter_map (fun (e : Source.Reporepo.entry) -> e.toolchain_name)
        |> List.sort_uniq String.compare
      in
      Error.fail_config_error
        "toolchain %S not registered in reporepo at %s. Known: %s" handle path
        (if known = [] then "(none — add toolchain definition entries)"
         else String.concat ", " known)

(* Resolve the toolchain's transitive dep handles → list of URL-bearing
   overlay packages_dirs. The toolchain entry itself is url-less. *)
let dep_packages_dirs ~entries ~path (entry : Source.Reporepo.entry) =
  let dep_roots =
    List.map
      (fun (h, v) : Source.Reporepo.root -> { handle = h; version = v })
      entry.depends
  in
  let resolved =
    if dep_roots = [] then []
    else Source.Reporepo.resolve entries ~roots:dep_roots |> List.rev
  in
  List.filter_map
    (fun (e : Source.Reporepo.entry) ->
      if e.url = "" then None
      else Some (Source.Reporepo.assert_overlay_dir ~path ~handle:e.handle))
    resolved

(* Build the constraint map + name list from the toolchain's
   [x-oi-toolchain-roots] specs. *)
let constraints_of_root_specs root_specs =
  let constraints =
    List.fold_left
      (fun m spec ->
        match parse_spec spec with
        | _, None -> m
        | n, Some v ->
            OpamPackage.Name.Map.add
              (OpamPackage.Name.of_string n)
              (`Eq, OpamPackage.Version.of_string v)
              m)
      OpamPackage.Name.Map.empty root_specs
  in
  let names =
    List.map
      (fun spec -> OpamPackage.Name.of_string (fst (parse_spec spec)))
      root_specs
  in
  (constraints, names)

(* Run [Solver.raw_solve] with the toolchain's constraints — no auto-pinning
   of OCaml-family packages, so the overlay's [+ox] versions aren't fought
   against [conf.ocaml_version]. *)
let solve_toolchain_packages ~handle ~conf ~packages_dirs ~constraints names =
  let env v = Solver.filter_env conf (OpamVariable.Full.of_string v) in
  let pkgs =
    match Solver.raw_solve ~env ~packages_dirs ~constraints names with
    | Ok pkgs -> pkgs
    | Error msg ->
        Error.fail_config_error "toolchain %s: solve failed: %s" handle msg
  in
  Solver.topo_sort ~packages_dirs ~conf pkgs

let pick_explicit_compiler ~handle (entry : Source.Reporepo.entry) =
  match entry.toolchain_compiler with
  | Some s -> s
  | None ->
      (* [Source.Reporepo.parse_entry_file] guarantees a toolchain entry has
         [x-oi-toolchain-compiler] set; this branch only triggers if the
         reporepo bypassed validation. *)
      Error.fail_config_error "toolchain %s: %s.%s has no %s" handle
        entry.handle entry.version Keys.toolchain_compiler

(* Resolve the OS-appropriate external compiler prefix for a non-relocatable
   toolchain, in precedence order:
   1. [OI_<HANDLE>_PREFIX] env override (CI / testing escape hatch);
   2. macOS with [x-oi-external-brew] set: [brew --prefix <ref>];
   3. [x-oi-external-prefix] verbatim (the Linux pin).
   [Source.Reporepo] validation guarantees a non-relocatable toolchain
   carries [x-oi-external-prefix], so case 3 always yields [Some _] there. *)
let env_var_of_handle handle =
  "OI_"
  ^ String.uppercase_ascii
      (String.map
         (fun c ->
           if
             (c >= 'a' && c <= 'z')
             || (c >= 'A' && c <= 'Z')
             || (c >= '0' && c <= '9')
           then c
           else '_')
         handle)
  ^ "_PREFIX"

let resolve_external_prefix ~sys ~(conf : Solver.Ctx.conf) ~handle
    (entry : Source.Reporepo.entry) =
  let env_var = env_var_of_handle handle in
  match Sys.getenv_opt env_var with
  | Some p when p <> "" ->
      Log.info (fun m ->
          m "Toolchain %s: external prefix from %s = %s" handle env_var p);
      Some p
  | _ -> (
      match (conf.os, entry.external_brew) with
      | "macos", Some brew_ref ->
          let p =
            try
              D10.Sysops.Cmd.run_out_quiet sys
                [ "brew"; "--prefix"; brew_ref ]
            with Eio.Io _ as e ->
              Error.fail_config_error
                "toolchain %s: 'brew --prefix %s' failed (%s) — install it \
                 with: brew install %s"
                handle brew_ref (Printexc.to_string e) brew_ref
          in
          Log.info (fun m ->
              m "Toolchain %s: external prefix via 'brew --prefix %s' = %s"
                handle brew_ref p);
          Some p
      | _ -> entry.external_prefix)

let resolve ~fs ~sys ~data_dir:_ ~(conf : Solver.Ctx.conf) ~handle =
  (* Auto-clone the reporepo if missing so [--toolchain=HANDLE] still works on
     a fresh machine — the toolchain definitions live there now. *)
  let path = Source.Reporepo.env_path () in
  Source.Reporepo.ensure_clone ~fs ~sys ~refresh:false ~path
    ~url:(Source.Reporepo.env_url ())
    ();
  let entry = entry_or_fail ~handle ~path in
  let entries = load_entries () in
  Log.info (fun m ->
      m "Resolving toolchain %s (reporepo handle %s.%s, depends: %s)" handle
        entry.handle entry.version
        (String.concat ", " (List.map fst entry.depends)));
  let packages_dirs = dep_packages_dirs ~entries ~path entry in
  let root_specs = List.flatten entry.toolchain_roots in
  if root_specs = [] then
    Error.fail_config_error "toolchain %s: %s.%s declares no %s" handle
      entry.handle entry.version Keys.toolchain_roots;
  let constraints, names = constraints_of_root_specs root_specs in
  let pkgs =
    solve_toolchain_packages ~handle ~conf ~packages_dirs ~constraints names
  in
  Log.info (fun m ->
      m "Toolchain %s solved packages: %s" handle
        (String.concat ", " (List.map OpamPackage.to_string pkgs)));
  let explicit_compiler = pick_explicit_compiler ~handle entry in
  let ocaml_version =
    match pick_ocaml_version ~explicit_compiler pkgs with
    | Some v -> v
    | None ->
        Error.fail_config_error
          "toolchain %s: solved set contains no %s package" handle
          (fst (parse_spec explicit_compiler))
  in
  let relocatable = Stdlib.Option.value entry.relocatable ~default:true in
  (* Non-relocatable toolchains are provided by a preinstalled system
     package — resolve its OS-appropriate prefix now. Relocatable ones
     never use it. *)
  let external_prefix =
    if relocatable then None
    else resolve_external_prefix ~sys ~conf ~handle entry
  in
  let hash = compute_hash ?external_prefix ~packages_dirs ~conf pkgs in
  let short = if String.length hash >= 8 then String.sub hash 0 8 else hash in
  let dir_name = Fmt.str "%s-%s-%s" handle ocaml_version short in
  let install_prefix =
    match external_prefix with
    | Some p -> p
    | None ->
        (* Relocatable toolchains never materialise this directory; it is
           folded into the cache key / identity only. *)
        default_root () / dir_name / "_opam"
  in
  let compiler_name =
    OpamPackage.Name.of_string (fst (parse_spec explicit_compiler))
  in
  {
    handle;
    ocaml_version;
    install_prefix;
    hash;
    relocatable;
    external_prefix;
    external_brew = (if relocatable then None else entry.external_brew);
    packages = OpamPackage.Set.of_list pkgs;
    compiler_name;
    root_names = OpamPackage.Name.Set.of_list names;
    packages_dirs;
    tools = entry.toolchain_tools;
    dep_handles = List.map fst entry.depends;
  }

(* -- Probe (external toolchains are never built by oi) ------------------- *)

let not_found_hint (info : info) prefix =
  let buf = Buffer.create 256 in
  let add fmt = Fmt.kstr (Buffer.add_string buf) fmt in
  add "toolchain %s: preinstalled compiler not found at %s@.@." info.handle
    prefix;
  add
    "  oi no longer builds this toolchain — install it as a system package:@.";
  add
    "    Linux : curl -fsSL https://oi.thicket.dev/install.sh | sh@.\
    \             (or your distro's 'oxcaml-compiler' package)@.";
  (match info.external_brew with
  | Some ref_ -> add "    macOS : brew install %s@." ref_
  | None -> ());
  add "@.  Already installed elsewhere? Point oi at it: %s=<prefix>"
    (env_var_of_handle info.handle);
  Buffer.contents buf

let ensure_installed ?(reporter = Build_progress.null) ~fs:_ (info : info) =
  if info.relocatable then
    Log.debug (fun m ->
        m
          "toolchain %s is relocatable — skipping fixed-prefix probe (the \
           consumer solve will build the compiler into its own prefix)"
          info.handle)
  else
    match info.external_prefix with
    | Some p when external_ready p ->
        Fmt.kstr
          (fun s -> reporter.Build_progress.event (Status s))
          "Using preinstalled toolchain %s" info.handle;
        Log.info (fun m ->
            m "toolchain %s: using preinstalled compiler at %s" info.handle p)
    | Some p -> Error.fail_config_error "%s" (not_found_hint info p)
    | None ->
        (* Unreachable in practice: [Source.Reporepo] validation rejects a
           non-relocatable toolchain definition without [x-oi-external-prefix].
           Kept total + defensive in case a reporepo bypassed validation. *)
        Error.fail_config_error
          "toolchain %s: non-relocatable but no %s set — oi no longer builds \
           toolchains; the reporepo entry must declare a preinstalled prefix"
          info.handle Keys.external_prefix
