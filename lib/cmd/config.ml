open Cmdliner

let human_bytes b =
  if Int64.compare b 1_000_000_000L > 0 then
    Fmt.str "%.1fGB" (Int64.to_float b /. 1e9)
  else if Int64.compare b 1_000_000L > 0 then
    Fmt.str "%.1fMB" (Int64.to_float b /. 1e6)
  else if Int64.compare b 1_000L > 0 then
    Fmt.str "%.1fKB" (Int64.to_float b /. 1e3)
  else Fmt.str "%LdB" b

(* -- Data model --------------------------------------------------------- *)

type platform = { os_key : string; ocaml_version : string }
type directories = { data : string; cache : string }
type registry = { url : string; index_ttl_s : float }
type source_mirror = { dir : string; blobs : int; total_bytes : int64 }

type toolchain_entry = {
  handle : string;
  url : string;
  ref_ : string option;
  relocatable : bool;
  is_default : bool;
  depends : string list;
  roots : string list;
  tools : string list;
}

type toolchains = { install_root : string; entries : toolchain_entry list }

type base_overlay = {
  handle : string;
  version : string;
  url : string;
  materialised : bool;
}

type pin_summary = { name : string; version : string; url : string }
type extra_repo_summary = { name : string; url : string }

type dev_tool = {
  name : string;
  hit : bool;
  version : string option;
  detail : string;
}

type project_summary = {
  extra_repos : extra_repo_summary list;
  pins : pin_summary list;
  overlays : string list;
  packages_dir : string option;
  dev_tools : dev_tool list;
}

type t = {
  platform : platform;
  directories : directories;
  registry : registry;
  source_mirror : source_mirror;
  toolchains : toolchains;
  base_overlays : base_overlay list;
  project : project_summary option;
}

(* -- Codec -------------------------------------------------------------- *)

let platform_codec =
  let open Jsont in
  Object.map ~kind:"platform" (fun os_key ocaml_version ->
      { os_key; ocaml_version })
  |> Object.mem "os_key" string ~enc:(fun p -> p.os_key)
  |> Object.mem "ocaml_version" string ~enc:(fun p -> p.ocaml_version)
  |> Object.finish

let directories_codec =
  let open Jsont in
  Object.map ~kind:"directories" (fun data cache -> { data; cache })
  |> Object.mem "data" string ~enc:(fun d -> d.data)
  |> Object.mem "cache" string ~enc:(fun d -> d.cache)
  |> Object.finish

let registry_codec =
  let open Jsont in
  Object.map ~kind:"registry" (fun url index_ttl_s ->
      ({ url; index_ttl_s } : registry))
  |> Object.mem "url" string ~enc:(fun (r : registry) -> r.url)
  |> Object.mem "index_ttl_s" number ~enc:(fun (r : registry) -> r.index_ttl_s)
  |> Object.finish

let source_mirror_codec =
  let open Jsont in
  Object.map ~kind:"source_mirror" (fun dir blobs total_bytes ->
      { dir; blobs; total_bytes })
  |> Object.mem "dir" string ~enc:(fun m -> m.dir)
  |> Object.mem "blobs" int ~enc:(fun m -> m.blobs)
  |> Object.mem "total_bytes" (map ~dec:Int64.of_int ~enc:Int64.to_int int)
       ~enc:(fun m -> m.total_bytes)
  |> Object.finish

let toolchain_entry_codec =
  let open Jsont in
  let enc f (t : toolchain_entry) = f t in
  Object.map ~kind:"toolchain"
    (fun
      handle
      url
      ref_
      relocatable
      is_default
      depends
      roots
      tools
      :
      toolchain_entry
    -> { handle; url; ref_; relocatable; is_default; depends; roots; tools })
  |> Object.mem "handle" string ~enc:(enc (fun t -> t.handle))
  |> Object.mem "url" string ~enc:(enc (fun t -> t.url))
  |> Object.opt_mem "ref" string ~enc:(enc (fun t -> t.ref_))
  |> Object.mem "relocatable" bool ~enc:(enc (fun t -> t.relocatable))
  |> Object.mem "is_default" bool ~enc:(enc (fun t -> t.is_default))
  |> Object.mem "depends" (list string) ~dec_absent:[]
       ~enc:(enc (fun t -> t.depends))
       ~enc_omit:(( = ) [])
  |> Object.mem "roots" (list string) ~dec_absent:[]
       ~enc:(enc (fun t -> t.roots))
       ~enc_omit:(( = ) [])
  |> Object.mem "tools" (list string) ~dec_absent:[]
       ~enc:(enc (fun t -> t.tools))
       ~enc_omit:(( = ) [])
  |> Object.finish

let toolchains_codec =
  let open Jsont in
  Object.map ~kind:"toolchains" (fun install_root entries ->
      { install_root; entries })
  |> Object.mem "install_root" string ~enc:(fun t -> t.install_root)
  |> Object.mem "entries" (list toolchain_entry_codec) ~enc:(fun t -> t.entries)
  |> Object.finish

let base_overlay_codec =
  let open Jsont in
  let enc f (o : base_overlay) = f o in
  Object.map ~kind:"base_overlay"
    (fun handle version url materialised : base_overlay ->
      { handle; version; url; materialised })
  |> Object.mem "handle" string ~enc:(enc (fun o -> o.handle))
  |> Object.mem "version" string ~enc:(enc (fun o -> o.version))
  |> Object.mem "url" string ~enc:(enc (fun o -> o.url))
  |> Object.mem "materialised" bool ~enc:(enc (fun o -> o.materialised))
  |> Object.finish

let pin_summary_codec =
  let open Jsont in
  let enc f (p : pin_summary) = f p in
  Object.map ~kind:"pin" (fun name version url : pin_summary ->
      { name; version; url })
  |> Object.mem "name" string ~enc:(enc (fun p -> p.name))
  |> Object.mem "version" string ~enc:(enc (fun p -> p.version))
  |> Object.mem "url" string ~enc:(enc (fun p -> p.url))
  |> Object.finish

let extra_repo_summary_codec =
  let open Jsont in
  let enc f (r : extra_repo_summary) = f r in
  Object.map ~kind:"extra_repo" (fun name url : extra_repo_summary ->
      { name; url })
  |> Object.mem "name" string ~enc:(enc (fun r -> r.name))
  |> Object.mem "url" string ~enc:(enc (fun r -> r.url))
  |> Object.finish

let dev_tool_codec =
  let open Jsont in
  Object.map ~kind:"dev_tool" (fun name hit version detail ->
      { name; hit; version; detail })
  |> Object.mem "name" string ~enc:(fun t -> t.name)
  |> Object.mem "hit" bool ~enc:(fun t -> t.hit)
  |> Object.opt_mem "version" string ~enc:(fun t -> t.version)
  |> Object.mem "detail" string ~enc:(fun t -> t.detail)
  |> Object.finish

let project_summary_codec =
  let open Jsont in
  Object.map ~kind:"project"
    (fun extra_repos pins overlays packages_dir dev_tools ->
      { extra_repos; pins; overlays; packages_dir; dev_tools })
  |> Object.mem "extra_repos"
       (list extra_repo_summary_codec)
       ~dec_absent:[]
       ~enc:(fun p -> p.extra_repos)
       ~enc_omit:(( = ) [])
  |> Object.mem "pins" (list pin_summary_codec) ~dec_absent:[]
       ~enc:(fun p -> p.pins)
       ~enc_omit:(( = ) [])
  |> Object.mem "overlays" (list string) ~dec_absent:[]
       ~enc:(fun p -> p.overlays)
       ~enc_omit:(( = ) [])
  |> Object.opt_mem "packages_dir" string ~enc:(fun p -> p.packages_dir)
  |> Object.mem "dev_tools" (list dev_tool_codec) ~dec_absent:[]
       ~enc:(fun p -> p.dev_tools)
       ~enc_omit:(( = ) [])
  |> Object.finish

let codec =
  let open Jsont in
  Object.map ~kind:"oi_config"
    (fun
      _schema_version
      platform
      directories
      registry
      source_mirror
      toolchains
      base_overlays
      project
    ->
      {
        platform;
        directories;
        registry;
        source_mirror;
        toolchains;
        base_overlays;
        project;
      })
  |> Object.mem "schema_version" string ~enc:(fun _ ->
      Oi.Stamp.json_schema_version)
  |> Object.mem "platform" platform_codec ~enc:(fun c -> c.platform)
  |> Object.mem "directories" directories_codec ~enc:(fun c -> c.directories)
  |> Object.mem "registry" registry_codec ~enc:(fun c -> c.registry)
  |> Object.mem "source_mirror" source_mirror_codec ~enc:(fun c ->
      c.source_mirror)
  |> Object.mem "toolchains" toolchains_codec ~enc:(fun c -> c.toolchains)
  |> Object.mem "base_overlays" (list base_overlay_codec) ~enc:(fun c ->
      c.base_overlays)
  |> Object.opt_mem "project" project_summary_codec ~enc:(fun c -> c.project)
  |> Object.finish

(* -- Data gathering ------------------------------------------------------ *)

let gather_toolchain_entries () =
  List.map
    (fun (s : Oi.Toolchain.summary) ->
      {
        handle = s.handle;
        url = s.url;
        ref_ = s.ref_;
        relocatable = s.relocatable;
        is_default = s.is_default;
        depends = s.depends;
        roots = s.roots;
        tools = s.tools;
      })
    (Oi.Toolchain.available ())

let gather_base_overlays () =
  let reporepo_path = Terms.reporepo_path () in
  List.map
    (fun (e : Oi.Source.Reporepo.entry) ->
      let dir =
        Oi.Source.Reporepo.overlay_packages_dir ~path:reporepo_path
          ~handle:e.handle
      in
      {
        handle = e.handle;
        version = e.version;
        url = e.url;
        materialised = e.url <> "" && Sys.file_exists dir;
      })
    (Oi.Source.Reporepo.base_entries ())

(* Build the [project_summary] view of the loaded [Oi.Project.t]. Pure
   field-by-field projection. *)
let project_summary_of ~fs cwd_s (p : Oi.Project.t) =
  let extra_repos =
    List.map
      (fun (r : Oi.Project.extra_repo) -> { name = r.name; url = r.url })
      p.extra_repos
  in
  let pins =
    List.map
      (fun (pin : Oi.Project.pin) ->
        {
          name = OpamPackage.Name.to_string (OpamPackage.name pin.pkg);
          version = OpamPackage.Version.to_string (OpamPackage.version pin.pkg);
          url = OpamUrl.to_string pin.url;
        })
      p.pins
  in
  let dev_tools =
    List.map
      (fun (r : Oi.Project.Tool.result) ->
        {
          name = r.spec.name;
          hit = r.hit;
          version = r.version;
          detail = r.detail;
        })
      (Oi.Project.Tool.probe ~fs cwd_s)
  in
  {
    extra_repos;
    pins;
    overlays = p.overlays;
    packages_dir = p.packages_dir;
    dev_tools;
  }

let gather_project ~fs ~skip_local =
  if skip_local then None
  else
    let cwd_s, _ = Workspace.resolved_cwd fs in
    match Oi.Project.load ~fs cwd_s with
    | exception Sys_error _ -> None
    | exception Eio.Exn.Io _ -> None
    | p -> Some (project_summary_of ~fs cwd_s p)

let gather ~fs ~cache ~os_key ~data_dir ~cache_dir ~skip_local =
  let mirror_stats = Oi.Source.Mirror.stats ~cache in
  {
    platform = { os_key; ocaml_version = Workspace.ocaml_version };
    directories = { data = data_dir; cache = cache_dir };
    registry =
      {
        url = Terms.default_registry;
        index_ttl_s = Layer_index.remote_index_max_age;
      };
    source_mirror =
      {
        dir = Oi.Source.Mirror.dir ~cache;
        blobs = mirror_stats.count;
        total_bytes = mirror_stats.total_size;
      };
    toolchains =
      {
        install_root = Oi.Toolchain.default_root ();
        entries = gather_toolchain_entries ();
      };
    base_overlays = gather_base_overlays ();
    project = gather_project ~fs ~skip_local;
  }

(* -- Renderers ----------------------------------------------------------- *)

let render_platform (p : platform) =
  Fmt.pr "@[<v>%a@," Oi.Style.pp_header_string "Platform";
  Fmt.pr "  os-key:     %s@," p.os_key;
  Fmt.pr "  ocaml:      %s (relocatable)@," p.ocaml_version

let render_directories (d : directories) =
  Fmt.pr "@,%a@," Oi.Style.pp_header_string "Directories";
  Fmt.pr "  data:       %s@," d.data;
  Fmt.pr "  cache:      %s@," d.cache

let render_registry (r : registry) =
  Fmt.pr "@,%a@," Oi.Style.pp_header_string "Registry";
  Fmt.pr "  url:        %s@," r.url;
  Fmt.pr "  index TTL:  %gs@," r.index_ttl_s

let render_source_mirror (m : source_mirror) =
  Fmt.pr "@,%a@," Oi.Style.pp_header_string "Source mirror";
  Fmt.pr "  dir:        %s@," m.dir;
  Fmt.pr "  blobs:      %d@," m.blobs;
  Fmt.pr "  total size: %s@," (human_bytes m.total_bytes)

let render_toolchain_entry (t : toolchain_entry) =
  let url_with_ref =
    match t.ref_ with Some r -> Fmt.str "%s#%s" t.url r | None -> t.url
  in
  let mode_tag =
    if t.relocatable then Fmt.str "[%a]" Oi.Style.pp_ok_string "relocatable"
    else Fmt.str "[%a]" Oi.Style.pp_warn_string "external"
  in
  let default_tag =
    if t.is_default then Fmt.str "  [%a]" Oi.Style.pp_accent_string "default"
    else ""
  in
  Fmt.pr "  %a  %s%s  %s@," Oi.Style.pp_header_string t.handle mode_tag
    default_tag url_with_ref;
  if t.depends <> [] then
    Fmt.pr "    depends:    %s@," (String.concat ", " t.depends);
  Fmt.pr "    roots:      %s@," (String.concat ", " t.roots);
  if t.tools <> [] then
    Fmt.pr "    tools:      %s@," (String.concat ", " t.tools);
  if not t.relocatable then
    Fmt.pr "    source:     %a@," Oi.Style.pp_dim_string
      "external compiler (auto-installed on first build)"

let render_toolchains (t : toolchains) =
  Fmt.pr "@,%a@," Oi.Style.pp_header_string "Toolchains";
  Fmt.pr "  install root:  %s@," t.install_root;
  List.iter render_toolchain_entry t.entries

let render_base_overlay (o : base_overlay) =
  let status =
    if o.url = "" then Fmt.str "%a" Oi.Style.pp_dim_string "definition only"
    else if o.materialised then
      Fmt.str "%a" Oi.Style.pp_ok_string "materialised"
    else Fmt.str "%a" Oi.Style.pp_warn_string "not materialised"
  in
  Fmt.pr "  %a.%s  %s  %s@," Oi.Style.pp_header_string o.handle o.version status
    o.url

let render_base_overlays overlays =
  Fmt.pr "@,%a@," Oi.Style.pp_header_string "Base overlays (from reporepo)";
  if overlays = [] then
    Fmt.pr
      "  %a no 'relocatable' overlay in reporepo %s. Run 'oi repo add' to \
       bootstrap.@,"
      Oi.Style.pp_warn_string "(none)" (Terms.reporepo_path ())
  else List.iter render_base_overlay overlays

let render_dev_tool (t : dev_tool) =
  let mark =
    if t.hit then Fmt.str "%a" Oi.Style.pp_ok_string "hit"
    else Fmt.str "%a" Oi.Style.pp_dim_string "miss"
  in
  Fmt.pr "  %-18s %-4s %s@." t.name mark t.detail

let render_project (p : project_summary) =
  if p.extra_repos <> [] then begin
    Fmt.pr "@.Project extra repositories:@.";
    List.iter
      (fun (r : extra_repo_summary) -> Fmt.pr "  %-20s %s@." r.name r.url)
      p.extra_repos
  end;
  if p.pins <> [] then begin
    Fmt.pr "@.Project pin-depends:@.";
    List.iter
      (fun (pin : pin_summary) ->
        Fmt.pr "  %-20s %s@." (pin.name ^ "." ^ pin.version) pin.url)
      p.pins
  end;
  if p.overlays <> [] then begin
    Fmt.pr "@.Project overlays (x-repos @-handles):@.";
    List.iter (fun h -> Fmt.pr "  %s@." h) p.overlays
  end;
  (match p.packages_dir with
  | None -> ()
  | Some dir ->
      Fmt.pr "@.Project local opam-repository:@.";
      Fmt.pr "  %s@." dir);
  Fmt.pr "@.Dev tools:@.";
  List.iter render_dev_tool p.dev_tools

let render_text c =
  render_platform c.platform;
  render_directories c.directories;
  render_registry c.registry;
  render_source_mirror c.source_mirror;
  render_toolchains c.toolchains;
  render_base_overlays c.base_overlays;
  Fmt.pr "@]@.";
  match c.project with None -> () | Some p -> render_project p

let render_json c =
  match Jsont_bytesrw.encode_string ~format:Jsont.Indent codec c with
  | Ok s ->
      print_string s;
      print_newline ()
  | Error e -> Oi.Error.fail_config_error "json encode failed: %s" e

let cmd =
  let run (c : Terms.common) skip_local =
    Harness.run @@ fun ~sw env ->
    let { Harness.fs; os_key; cache; _ } =
      Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env
        c.cache_dir
    in
    let view =
      gather ~fs ~cache ~os_key ~data_dir:c.data_dir ~cache_dir:c.cache_dir
        ~skip_local
    in
    match c.format with Text -> render_text view | Json -> render_json view
  in
  let info =
    Cmd.info "config" ~doc:"Show oi's view of this machine and project."
      ~man:
        [
          `S Manpage.s_description;
          `P "Print machine and project state as $(b,oi) sees it.";
          `P "$(b,--format=json) emits a stable JSON document.";
          `I ("$(b,Platform)", "$(b,os-key) and OCaml version.");
          `I
            ( "$(b,Directories)",
              "Data and cache dirs. Override with $(b,OI_DATA_DIR), \
               $(b,OI_CACHE_DIR), or XDG defaults." );
          `I ("$(b,Registry)", "Layer registry URL and index TTL.");
          `I
            ( "$(b,Source mirror)",
              "Local opam source mirror: path, blob count, total size." );
          `I
            ( "$(b,Toolchains)",
              "Handles accepted by $(b,--toolchain=NAME), tagged \
               $(b,[relocatable]) or $(b,[fixed-prefix]), with source URL and \
               install status." );
          `I
            ( "$(b,Base overlays)",
              "Overlays declared by the reporepo, each marked \
               $(b,materialised), $(b,not materialised), or $(b,definition \
               only)." );
          `I
            ( "$(b,Project)",
              "Shown when run in a project tree (suppress with \
               $(b,--skip-local)): $(b,x-repos:), $(b,pin-depends:), overlay \
               handles, project-local $(b,packages/) opam-repository." );
          `I
            ( "$(b,Dev tools)",
              "Tools the next $(b,oi build) would install. $(b,hit) = trigger \
               fired; $(b,miss) = skipped." );
        ]
  in
  Cmd.v info Term.(const run $ Terms.common $ Terms.skip_local)
