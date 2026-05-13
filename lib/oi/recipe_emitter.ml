let log_src = Logs.Src.create "oi.recipe_emitter"

module Log = (val Logs.src_log log_src : Logs.LOG)

(* POSIX shell-escape: wrap in single quotes, escape any embedded
   single quote as '\''. Result is always safe for /bin/sh. *)
let sh_escape s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '\'';
  String.iter
    (fun c ->
      if c = '\'' then Buffer.add_string buf "'\\''" else Buffer.add_char buf c)
    s;
  Buffer.add_char buf '\'';
  Buffer.contents buf

let script_of_commands ~build_cmds ~install_cmds =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "set -e\n";
  let emit_cmd cmd =
    Buffer.add_string buf (String.concat " " (List.map sh_escape cmd));
    Buffer.add_char buf '\n'
  in
  List.iter emit_cmd build_cmds;
  List.iter emit_cmd install_cmds;
  Buffer.contents buf

(* The env array we receive from [Plan.elaborate] / [Solver.compilation_env]
   contains everything opam computed for the build, including a passthrough
   of the host's [Unix.environment ()]. For a portable, hermetic recipe we
   want only:
     - vars oi/opam set explicitly (PATH, OPAM_*, OCAMLPATH, CAML_*, …);
     - a small set of host-derived knobs every well-behaved unix build
       expects to find (HOME, USER, locale, terminal, tmpdir).
   Everything else (CLAUDE_*, AI_AGENT, DBUS_*, SSH_*, DISPLAY, shell
   bookkeeping, …) is stripped so the recipe emitted on one machine has
   no host-specific tail. *)

let blessed_host_keys =
  [
    "HOME";
    "USER";
    "LOGNAME";
    "TMPDIR";
    "TZ";
    "LANG";
    "LC_ALL";
    "LC_CTYPE";
    "LC_COLLATE";
    "LC_MESSAGES";
    "LC_TIME";
    "LC_NUMERIC";
    "LC_MONETARY";
    "TERM";
    "SHELL";
    "SOURCE_DATE_EPOCH";
  ]

let blessed_prefixes =
  [
    "PATH=";
    "MANPATH=";
    "OPAM_";
    "OPAMROOT=";
    "OPAMCLI=";
    "OPAMSWITCH=";
    "OCAML";
    "CAML_";
    "DUNE_";
    "OI_";
    "OINDEX_";
    "MAKEFLAGS=";
    "MAKELEVEL=";
    "CDPATH=";
  ]

let key_of entry =
  match String.index_opt entry '=' with
  | None -> entry
  | Some i -> String.sub entry 0 i

let env_entry_allowed entry =
  let starts_with prefix =
    let l = String.length prefix in
    String.length entry >= l && String.sub entry 0 l = prefix
  in
  List.exists starts_with blessed_prefixes
  || List.mem (key_of entry) blessed_host_keys

(* Opam-build hardening that's not in [compilation_env]: stop ocamlfind
   from appending to a non-writeable [<toolchain>/lib/ocaml/ld.conf]
   under non-relocatable toolchains (the oxcaml zarith case). The d10ir
   library doesn't know about ocamlfind; we bake this into the recipe
   so the consumer doesn't need to. *)
let opam_build_env_extras = [ "OCAMLFIND_LDCONF=ignore" ]

(* Standard FHS system paths every linux build expects. We replace the
   host's [Sys.getenv "PATH"] tail with this so user-specific dirs
   (e.g. [~/.local/bin], [~/.opam/<switch>/bin], [/snap/bin]) don't
   leak into the recipe. *)
let standard_system_path =
  "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

(* PATH gets special treatment: keep oi-controlled entries (anything under
   [cache_root] or [toolchains_root] — the latter handles non-relocatable
   toolchains whose [_opam/bin/] lives outside the per-cache tree), replace
   the rest with [standard_system_path]. Without [toolchains_root] in the
   allow-list a non-relocatable toolchain's [bin/] gets stripped from the
   recipe's PATH and every build script that invokes [ocaml] / [dune] fails
   with exit 127 inside the docker container, where [OI_CACHE_DIR=/cache]
   but the toolchain lives under [~/.cache/oi/toolchains/]. *)
let path_entry_oi_owned ~owned_prefixes p =
  List.exists
    (fun prefix ->
      prefix <> ""
      && String.length p >= String.length prefix
      && String.sub p 0 (String.length prefix) = prefix)
    owned_prefixes

let scrub_path ~owned_prefixes entry =
  let prefix = "PATH=" in
  if not (String.length entry >= 5 && String.sub entry 0 5 = prefix) then entry
  else
    let value = String.sub entry 5 (String.length entry - 5) in
    let parts = String.split_on_char ':' value in
    let oi_owned = List.filter (path_entry_oi_owned ~owned_prefixes) parts in
    "PATH=" ^ String.concat ":" (oi_owned @ [ standard_system_path ])

let env_of_array ~cache_root env_array =
  let owned_prefixes = [ cache_root; Cache.toolchains_root () ] in
  let kept =
    Array.to_list env_array
    |> List.filter (fun s -> String.contains s '=')
    |> List.filter env_entry_allowed
    |> List.map (scrub_path ~owned_prefixes)
  in
  let already key =
    let k = key ^ "=" in
    List.exists
      (fun e ->
        String.length e >= String.length k
        && String.sub e 0 (String.length k) = k)
      kept
  in
  let extras =
    List.filter (fun e -> not (already (key_of e))) opam_build_env_extras
  in
  kept @ extras

let overlay_of_d10 (o : D10.Overlay.t) : D10ir.Plan.overlay =
  { handle = o.handle; version = o.version }

let opam_file_sha256_of (p : Plan.package_plan) =
  match p.opam_path with
  | Some path -> Provenance.hash_opam_file ~path
  | None -> ""

(* Strict consumer of x-d10-archive: errors if the package's opam
   file doesn't carry the field, since the only path that should
   populate it ([oi repo bake] / [oi repo bump]) is solver-free
   and runs as part of the reporepo workflow.

   Archive *presence* on disk is NOT verified here — the pipeline's
   pre-fetch step pulls missing archives from the configured registry
   before [D10ir.Direct.run] takes over. The Direct executor itself
   does no network I/O; if an archive is still missing post-prefetch,
   it errors at unpack time. *)
let node_of_package_plan ~cache_root (p : Plan.package_plan) : D10ir.Plan.node =
  let archive : D10ir.Archive.t =
    let handle_str =
      match p.overlay with Some o -> o.handle | None -> "<handle>"
    in
    match p.d10_archive with
    | None ->
        let opam_loc =
          match p.opam_path with
          | Some path -> Fmt.str "@\n  opam file: %s" path
          | None -> ""
        in
        Error.fail_config_error
          "%s: opam file has no [x-d10-archive] — run [oi repo bump %s] to \
           populate the consolidated source archive sha.%s@\n\
           Bake is solver-free and writes shas into the v2 reporepo as part of \
           the bump."
          p.pkg handle_str opam_loc
    | Some sha ->
        { path = Fmt.str "%s.tar.zst" sha; sha256 = sha; strip_components = 0 }
  in
  let env = env_of_array ~cache_root p.env in
  let pkg_name, pkg_version =
    match String.index_opt p.pkg '.' with
    | None -> (p.pkg, "")
    | Some i ->
        ( String.sub p.pkg 0 i,
          String.sub p.pkg (i + 1) (String.length p.pkg - i - 1) )
  in
  let script =
    script_of_commands ~build_cmds:p.build_commands
      ~install_cmds:p.install_commands
  in
  let dep_layer_hashes =
    List.map
      (fun (d : Identity.dep) -> D10ir.Layer_hash.of_string d.hash)
      p.dep_layers
  in
  {
    package = { name = pkg_name; version = pkg_version };
    layer_hash = D10ir.Layer_hash.of_string p.layer_hash;
    dep_layer_hashes;
    archive;
    script;
    env;
    depexts = p.depexts;
    prefix = p.prefix;
    substs = p.substs;
    subst_vars = List.map (fun (k, v) -> Fmt.str "%s=%s" k v) p.subst_vars;
    overlay = Option.map overlay_of_d10 p.overlay;
    opam_file_sha256 = opam_file_sha256_of p;
  }

(* Where the user's local dune cache lives. Mirrors dune's own lookup
   so [oi build] shares an artifact pool with regular [dune build]
   invocations outside oi — incremental rebuilds across both tools
   are then a single hardlink lookup. Falls back gracefully if neither
   env var is set. *)
let dune_cache_root () =
  match Sys.getenv_opt "DUNE_CACHE_ROOT" with
  | Some p -> p
  | None -> (
      match Sys.getenv_opt "XDG_CACHE_HOME" with
      | Some d -> Filename.concat d "dune"
      | None ->
          let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
          Filename.concat home ".cache/dune")

(* Default mount set shared across every recipe.

   Right now this is just the dune build cache. Each entry advertises
   a host path (created at execution time) and the env vars the build
   process expects to find pointing into it. On the native executor
   [source = target]; for a future sandboxed backend [target] would
   be the in-sandbox path.

   Why this lives in the recipe rather than the executor: the recipe
   is the contract between [oi build] / [oi ir emit] and any executor
   that runs it (Direct now, obuilder later, remote-runner one day).
   Encoding the cache layout here means a recipe baked on machine A
   carries enough info for machine B's sandbox to wire up identically,
   without machine B's executor needing to hard-code the same set of
   well-known caches. *)
let default_mounts () : D10ir.Plan.mount list =
  let dune_cache = dune_cache_root () in
  [
    {
      name = "dune-cache";
      source = dune_cache;
      target = dune_cache;
      mode = `Rw;
      env =
        [
          "DUNE_CACHE=enabled";
          Fmt.str "DUNE_CACHE_ROOT=%s" dune_cache;
          (* hardlink: same FS as the staging dir, near-zero copy. *)
          "DUNE_CACHE_STORAGE_MODE=hardlink";
        ];
    };
  ]

let emit ~d10 ?(cli_invocation = []) ~toolchain_name ~toolchain_layer
    (plan : Plan.t) : D10ir.Plan.t =
  let cache_root = plan.cache_root in
  let nodes =
    List.filter_map
      (fun (p : Plan.package_plan) ->
        match p.method_ with
        | Identity.Binary ->
            Log.debug (fun m -> m "skipping binary package %s" p.pkg);
            None
        | Source -> Some (node_of_package_plan ~cache_root p))
      plan.packages
  in
  let roots =
    (* Roots: nodes that no other node depends on. Compute over the
       layer-hash graph. *)
    let dep_set = Hashtbl.create 64 in
    List.iter
      (fun (n : D10ir.Plan.node) ->
        List.iter
          (fun h -> Hashtbl.replace dep_set (D10ir.Layer_hash.to_string h) ())
          n.dep_layer_hashes)
      nodes;
    List.filter_map
      (fun (n : D10ir.Plan.node) ->
        if Hashtbl.mem dep_set (D10ir.Layer_hash.to_string n.layer_hash) then
          None
        else Some n.layer_hash)
      nodes
  in
  let toolchain : D10ir.Plan.toolchain =
    {
      name = toolchain_name;
      base_layer = D10ir.Layer_hash.of_string toolchain_layer;
    }
  in
  let metadata : D10ir.Plan.metadata =
    {
      oi_version = "0.9.9";
      generated_at = Unix.gettimeofday ();
      cli_invocation;
    }
  in
  {
    schema_version = D10ir.Plan.current_schema_version;
    os_key = plan.os_key;
    toolchain;
    (* Absolute path to the content-addressed archive store. Combined
       with each node's [archive.path] basename, the executor resolves
       <archive_root>/<basename>.tar. Plans are not yet portable across
       machines (the archives live under the local d10 cache); making
       them so means turning archive_root into a path relative to the
       recipe.json's location and bundling the archives, which is a
       v2 concern. *)
    archive_root =
      Filename.concat
        (Eio.Path.native_exn d10.D10.Config.root)
        (Filename.concat "d10ir" "archives");
    nodes;
    roots;
    mounts = default_mounts ();
    external_layers =
      List.map D10ir.Layer_hash.of_string plan.external_layer_hashes;
    metadata;
  }
