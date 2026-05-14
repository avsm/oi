open Cmdliner

let log_src = Logs.Src.create "oi.cmd.show" ~doc:"oi show command"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

(* -- plan ---------------------------------------------------------------- *)

(* Rendering helpers for [oi show]'s default succinct page. *)

(* Format the top-block "Target:" line. For a CLI-supplied target we
   print it verbatim (e.g. "utop", "@avsm/tangled"); for the
   local-project case we show the package name (or count when there
   is more than one *.opam file). *)
let target_label_of ~targets ~local_packages =
  match targets with
  | [] -> (
      match local_packages with
      | [] -> "local project"
      | [ p ] -> Fmt.str "local project (%s)" p
      | many -> Fmt.str "local project (%d packages)" (List.length many))
  | _ -> String.concat " " targets

(* The overlay line is only printed when the solve actually pulled from
   an overlay. CLI-supplied [@handle/pkg] shortcuts and project
   [x-repos:] both feed into [with_repos], so we take the first handle
   we see. *)
let overlay_label_of ~with_repos =
  match with_repos with
  | [] -> None
  | h :: _ when not (Target.is_url_like h) -> Some ("@" ^ h)
  | _ -> None

(* Split the elaborated plan's packages into (cached, source) counts. *)
let counts_of (plan : Oi.Plan.t) =
  List.fold_left
    (fun (c, s) (p : Oi.Plan.package_plan) ->
      match p.method_ with
      | Oi.Identity.Binary -> (c + 1, s)
      | Source -> (c, s + 1))
    (0, 0) plan.packages

let opam_pkg_of_package_plan (p : Oi.Plan.package_plan) =
  match OpamPackage.of_string_opt p.pkg with
  | Some op -> op
  | None ->
      OpamPackage.create
        (OpamPackage.Name.of_string p.pkg)
        (OpamPackage.Version.of_string "0")

(* Compute the depexts declared by every package in the plan (both
   cached and source), along with the host installation status. The
   full closure is what the old [oi depexts] reported and is the right
   answer for scripting use ("what would this need from apt if I were
   building from scratch?"). When [--os] is set the host check isn't
   meaningful and we return [None] for the status. *)
let pp_depexts ~conf ~packages_dirs ~(plan : Oi.Plan.t) ~os_override =
  let all_pkgs = List.map opam_pkg_of_package_plan plan.packages in
  let entries = Oi.Depexts.compute_for_conf ~conf ~packages_dirs all_pkgs in
  let all =
    List.fold_left
      (fun acc e -> OpamSysPkg.Set.union acc e.Oi.Depexts.sys_pkgs)
      OpamSysPkg.Set.empty entries
  in
  let status =
    if os_override <> None then None else Some (Oi.Depexts.status all)
  in
  (all, status)

(* Read every *.opam file in [cwd] directly, for the no-target case
   where we want to surface the project's own metadata. Returns one
   [(pkg, opam)] per file (sorted alphabetically by package name) with
   a placeholder "dev" version, since a project's own opam files are
   typically versionless. Multi-package projects (foo.opam, bar.opam)
   contribute one entry each so the info page shows every package's
   synopsis/license/etc., not just the first file's. *)
let read_local_opams ~cwd =
  let entries = try Sys.readdir cwd |> Array.to_list with Sys_error _ -> [] in
  entries
  |> List.filter (fun n ->
      Filename.check_suffix n ".opam" && Filename.chop_suffix n ".opam" <> "")
  |> List.sort String.compare
  |> List.filter_map (fun filename ->
      let path = Filename.concat cwd filename in
      let name = Filename.chop_suffix filename ".opam" in
      try
        let opam = OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path)) in
        let pkg =
          OpamPackage.create
            (OpamPackage.Name.of_string name)
            (OpamPackage.Version.of_string "dev")
        in
        Some (pkg, opam)
      with Failure _ | Sys_error _ -> None)

(* Pick the package whose metadata we'll surface on the default info
   page. A CLI target resolves to its action-plan node. For the
   local-project case we read the project's own first *.opam file
   directly (otherwise we'd show metadata for the first
   dependency, which is misleading). Anything else falls through to
   the first plan node as a last-ditch option. *)
type meta_source =
  | From_pkg of Oi.Plan.package_plan
  | From_project_opams of (OpamPackage.t * OpamFile.OPAM.t) list

let find_plan_pkg ~(plan : Oi.Plan.t) name =
  List.find_opt
    (fun (p : Oi.Plan.package_plan) ->
      let opam_pkg = opam_pkg_of_package_plan p in
      OpamPackage.Name.to_string (OpamPackage.name opam_pkg) = name)
    plan.packages

let primary_meta_for_project ~plan ~project_deps ~cwd =
  match read_local_opams ~cwd with
  | _ :: _ as opams -> Some (From_project_opams opams)
  | [] -> (
      match project_deps with
      | first :: _ ->
          Stdlib.Option.map (fun p -> From_pkg p) (find_plan_pkg ~plan first)
      | [] -> None)

let pp_primary_meta ~(plan : Oi.Plan.t) ~targets ~project_deps ~cwd =
  match targets with
  | first :: _ ->
      Stdlib.Option.map (fun p -> From_pkg p) (find_plan_pkg ~plan first)
  | [] -> primary_meta_for_project ~plan ~project_deps ~cwd

(* Collapse a multi-line synopsis to its first line so the info page
   stays tidy. *)
let first_line s =
  match String.index_opt s '\n' with None -> s | Some i -> String.sub s 0 i

(* Print a single optional metadata field. Skipped silently when the
   value is absent or empty. The label column is fixed at 11
   characters so all rows on the info page line up. *)
let pp_meta_line label value =
  match value with
  | "" -> ()
  | v ->
      Fmt.pr "%a %s@," Oi.Style.pp_header_string
        (Fmt.str "%-11s" (label ^ ":"))
        v

(* Extract a compact, user-facing snapshot of an opam file's
   descriptive metadata fields for the info page. *)
let package_meta_text (_pkg : OpamPackage.t) (opam : OpamFile.OPAM.t) =
  let synopsis =
    Stdlib.Option.value (OpamFile.OPAM.synopsis opam) ~default:""
    |> String.trim |> first_line
  in
  let license = String.concat ", " (OpamFile.OPAM.license opam) in
  let homepage = String.concat ", " (OpamFile.OPAM.homepage opam) in
  let dev_repo =
    match OpamFile.OPAM.dev_repo opam with
    | None -> ""
    | Some u -> OpamUrl.to_string u
  in
  let maintainer = String.concat ", " (OpamFile.OPAM.maintainer opam) in
  let tags = String.concat ", " (OpamFile.OPAM.tags opam) in
  let description =
    Stdlib.Option.value (OpamFile.OPAM.descr_body opam) ~default:""
    |> String.trim
  in
  (synopsis, license, homepage, dev_repo, maintainer, tags, description)

(* List the binaries that would end up on [$PATH] when this target's
   layer is assembled into a prefix. When the layer is cached locally
   we scan [layers/<os_key>/<hash>/fs/bin] and [fs/sbin] directly;
   that's cheaper than a sqlite query and also works for layers the
   index doesn't cover (fresh builds that haven't been re-indexed
   yet). Returns [[]] for a layer that hasn't been built, for a
   purely library package, or when the fs/ tree is missing. *)
let pp_package_binaries ~cache_root ~os_key ~layer_hash =
  let layer_dir = cache_root / "layers" / os_key / layer_hash / "fs" in
  let scan sub =
    let dir = layer_dir / sub in
    if not (Sys.file_exists dir) then []
    else try Sys.readdir dir |> Array.to_list with Sys_error _ -> []
  in
  let bins = scan "bin" @ scan "sbin" in
  List.sort_uniq String.compare bins

(* Collect the (handle, version, url) tuples the user would want to
   see on the info page: when a toolchain is active, its overlay
   chain (e.g. [oxcaml + default]); otherwise the default base
   chain (relocatable / default). Plus any overlays named
   explicitly in [with_repos], in that order, deduplicated by
   handle. *)
let pp_repositories ?toolchain ~with_repos () =
  let entries =
    try Oi.Source.Reporepo.load ~path:(Terms.reporepo_path ())
    with Oi.Error.E _ -> []
  in
  let base_handles =
    match toolchain with
    | Some (info : Oi.Toolchain.info) -> info.handle :: info.dep_handles
    | None ->
        Oi.Source.Reporepo.base_entries ()
        |> List.map (fun (e : Oi.Source.Reporepo.entry) -> e.handle)
  in
  let extra_handles =
    List.filter (fun h -> not (Target.is_url_like h)) with_repos
  in
  let all = base_handles @ extra_handles |> List.sort_uniq String.compare in
  let ordered =
    let seen = Hashtbl.create 4 in
    let push acc h =
      if List.mem h all && not (Hashtbl.mem seen h) then begin
        Hashtbl.add seen h ();
        h :: acc
      end
      else acc
    in
    let acc = List.fold_left push [] base_handles in
    let acc = List.fold_left push acc extra_handles in
    List.rev acc
  in
  List.filter_map
    (fun h ->
      match Oi.Source.Reporepo.latest entries ~handle:h with
      | Some (e : Oi.Source.Reporepo.entry) ->
          let url = if e.commit = "" then e.url else e.url ^ "#" ^ e.commit in
          Some (h, e.version, url)
      | None -> (
          (* Toolchain overlay: not in reporepo, but we know its URL. *)
          match Oi.Toolchain.url_of ~handle:h with
          | Some url -> Some (h, "builtin", url)
          | None -> None))
    ordered

(* Print one indented metadata line. Mirrors [pp_meta_line]'s 11-char
   label column but at an arbitrary indent so per-package blocks line
   up under their package-name header. *)
let pp_indented_meta_line ~indent label = function
  | "" -> ()
  | v ->
      Fmt.pr "%s%a %s@," (String.make indent ' ') Oi.Style.pp_header_string
        (Fmt.str "%-11s" (label ^ ":"))
        v

(* Emit one package's metadata fields. Returns the description body so
   the caller can choose where to render it (today: at the bottom of
   the info page). [indent] is 0 for the legacy single-target layout
   and 2 for per-package blocks under a project-mode pkg header. *)
let pkg_meta_block ~indent (pkg : OpamPackage.t) (opam : OpamFile.OPAM.t) =
  let synopsis, license, homepage, dev_repo, maintainer, tags, description =
    package_meta_text pkg opam
  in
  let line = pp_indented_meta_line ~indent in
  line "Synopsis" synopsis;
  line "License" license;
  line "Homepage" homepage;
  (* Only surface dev-repo when it adds information beyond the
     homepage. Many opam files repeat the same github URL for both,
     which just makes the info page noisier. *)
  if dev_repo <> homepage then line "Source" dev_repo;
  line "Maintainer" maintainer;
  line "Tags" tags;
  description

let pp_target_line ~target_label ~target_version =
  let target_line =
    match target_version with
    | "" -> target_label
    | v -> Fmt.str "%s %s" target_label v
  in
  pp_meta_line "Target" target_line

(* Inline metadata for a single CLI target / single-package project.
   Returns the description body, if any, to render later. *)
let render_single_opam (pkg, opam) =
  let descr = pkg_meta_block ~indent:0 pkg opam in
  if descr = "" then [] else [ ("", descr) ]

(* Multi-package project mode: render each package as its own
   indented block under a name header so every *.opam in the
   directory contributes its synopsis/license/etc. *)
let render_many_opams many =
  let descrs =
    List.filter_map
      (fun (pkg, opam) ->
        let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
        Fmt.pr "@,%a@," Oi.Style.pp_header_string name;
        let descr = pkg_meta_block ~indent:2 pkg opam in
        if descr = "" then None else Some (name, descr))
      many
  in
  Fmt.pr "@,";
  descrs

let render_target_opams = function
  | [] -> []
  | [ entry ] -> render_single_opam entry
  | many -> render_many_opams many

let pp_binaries = function
  | [] -> ()
  | bs -> pp_meta_line "Binaries" (String.concat ", " bs)

let pp_overlay_tag = function
  | None -> ()
  | Some tag ->
      Fmt.kstr (pp_meta_line "Overlay") "%a" Oi.Style.pp_info_string tag

let pp_package_counts ~n_cached ~n_source =
  let n_total = n_cached + n_source in
  if n_source = 0 then
    Fmt.kstr (pp_meta_line "Packages") "%d total, all cached locally." n_total
  else begin
    Fmt.kstr (pp_meta_line "Packages") "%d total" n_total;
    Fmt.pr "              cached: %d@," n_cached;
    Fmt.pr "              build:  %d  (from source)@," n_source
  end

let pp_depexts_plain all_depexts =
  let names =
    OpamSysPkg.Set.elements all_depexts |> List.map OpamSysPkg.to_string
  in
  pp_meta_line "Depexts" (String.concat ", " names);
  Fmt.pr "            %a@," Oi.Style.pp_dim_string
    "(host check skipped because --os is set)"

(* Every depext declared, with the uninstalled ones marked.
   Missing tokens are styled in yellow so they stand out even
   when "(missing)" is the only textual marker. *)
let pp_depexts_status all_depexts (st : Oi.Depexts.status) =
  let render p =
    let name = OpamSysPkg.to_string p in
    if OpamSysPkg.Set.mem p st.missing then
      Fmt.str "%a" Oi.Style.pp_warn_string (name ^ " (missing)")
    else name
  in
  let rendered =
    OpamSysPkg.Set.elements all_depexts |> List.map render |> String.concat ", "
  in
  pp_meta_line "Depexts" rendered;
  if not (OpamSysPkg.Set.is_empty st.missing) then
    let missing_names =
      OpamSysPkg.Set.elements st.missing |> List.map OpamSysPkg.to_string
    in
    Fmt.kstr
      (Fmt.pr "            %a@," Oi.Style.pp_dim_string)
      "Run: sudo apt install %s"
      (String.concat " " missing_names)

let pp_depexts_section ~all_depexts ~dep_status =
  if OpamSysPkg.Set.is_empty all_depexts then
    pp_meta_line "Depexts" "(no depexts declared)"
  else
    match (dep_status : Oi.Depexts.status option) with
    | None -> pp_depexts_plain all_depexts
    | Some st -> pp_depexts_status all_depexts st

(* Two columns: [@handle (version)] left-padded to the longest
   token so URLs line up. *)
let pp_repositories_section = function
  | [] -> ()
  | rows ->
      Fmt.pr "@,%a@," Oi.Style.pp_header_string "Repositories:";
      let left = List.map (fun (h, v, _) -> Fmt.str "@%s (%s)" h v) rows in
      let col = List.fold_left (fun m s -> max m (String.length s)) 0 left in
      List.iter2
        (fun (_, _, url) l ->
          Fmt.pr "  %a  %s@," Oi.Style.pp_info_string (Fmt.str "%-*s" col l) url)
        rows left

let pp_description_body body =
  String.split_on_char '\n' body |> List.iter (fun line -> Fmt.pr "  %s@," line)

let pp_descriptions = function
  | [] -> ()
  | [ ("", body) ] ->
      Fmt.pr "@,%a@," Oi.Style.pp_header_string "Description:";
      pp_description_body body
  | many ->
      List.iter
        (fun (name, body) ->
          let label =
            if name = "" then "Description:"
            else Fmt.str "Description (%s):" name
          in
          Fmt.pr "@,%a@," Oi.Style.pp_header_string label;
          pp_description_body body)
        many

(* Render the default succinct info page. [target_opams] is empty when
   we have no metadata to show, length 1 for a CLI target or
   single-package project (legacy inline layout), and length >=2 for a
   multi-package project (one indented block per package, headed by
   the package name). *)
let pp_render_info ~target_label ~target_version ~target_opams ~overlay ~os_key
    ~ocaml_version ~n_cached ~n_source ~all_depexts ~dep_status ~repositories
    ~binaries =
  Fmt.pr "@[<v>";
  pp_target_line ~target_label ~target_version;
  let descriptions = render_target_opams target_opams in
  pp_binaries binaries;
  pp_overlay_tag overlay;
  pp_meta_line "Platform" os_key;
  pp_meta_line "OCaml" ocaml_version;
  Fmt.pr "@,";
  pp_package_counts ~n_cached ~n_source;
  Fmt.pr "@,";
  pp_depexts_section ~all_depexts ~dep_status;
  pp_repositories_section repositories;
  pp_descriptions descriptions;
  Fmt.pr "@]@."

(* -- overlay listing ---------------------------------------------------- *)

(* Parse [name.version] entries under a [packages/<name>/] dir, dropping
   any that don't refer to [name]. *)
let parse_pkg_version ~name pkg_s =
  Stdlib.Option.bind (OpamPackage.of_string_opt pkg_s) (fun p ->
      if OpamPackage.Name.to_string (OpamPackage.name p) = name then
        Some (OpamPackage.version p)
      else None)

let max_version acc v =
  match acc with
  | None -> Some v
  | Some best when OpamPackage.Version.compare v best > 0 -> Some v
  | _ -> acc

(* Highest [version] under [pkgs_dir/name/<name.version>/]; [None] when no
   parseable opam dir exists for [name]. *)
let latest_version_of ~pkgs_dir ~name =
  let dir = pkgs_dir / name in
  if not (Sys.is_directory dir) then None
  else
    let entries =
      try Sys.readdir dir |> Array.to_list with Sys_error _ -> []
    in
    entries
    |> List.filter_map (parse_pkg_version ~name)
    |> List.fold_left (fun acc v -> max_version acc v) None
    |> Stdlib.Option.map OpamPackage.Version.to_string

(* Binaries shipped by [hash]: every [bin/X] or [sbin/X] entry the index
   has on file. Empty when nothing was indexed. *)
let layer_binaries db ~hash =
  D10.Index.files db ~hash
  |> List.filter_map (fun path ->
      match String.split_on_char '/' path with
      | ("bin" | "sbin") :: name :: _ -> Some name
      | _ -> None)
  |> List.sort_uniq String.compare

type overlay_state =
  | Cached of string list  (** binaries *)
  | Build_failed
  | Declared

let lookup_state ?db ~os_key ~name ~ver () =
  match db with
  | None -> Declared
  | Some db -> (
      match D10.Index.find_layer db ~name ~version:ver ~os_key with
      | Some (hash, 0) -> Cached (layer_binaries db ~hash)
      | Some _ -> Build_failed
      | None -> Declared)

let render_state =
  let dim s = Tty.Span.styled Oi.Style.dim s in
  fun st ->
    let label =
      match st with
      | Cached _ -> Tty.Span.styled Oi.Style.ok "cached"
      | Build_failed -> Tty.Span.styled Oi.Style.error "build failed"
      | Declared -> dim "declared"
    in
    let bins =
      match st with
      | Cached (_ :: _ as bs) -> Tty.Span.text (String.concat ", " bs)
      | _ -> dim "—"
    in
    (label, bins)

(* Walk [<reporepo>/v2/<handle>/packages/<name>/<name.version>/] to
   enumerate every package the overlay declares, then cross-reference
   the d10 index for cached layers + binaries. One row per
   (package, latest-version): cached packages show their binaries;
   uncached ones show as "declared". *)
(* Solve every root group that the overlay declares (or every package
   it ships when [x-root-packages] is empty), elaborate each into an
   exec plan, dedup the resulting [package_plan]s by layer hash, and
   render the union as a Unicode dependency tree.

   This is the [oi show @HANDLE] companion to [oi build @HANDLE]:
   same solver inputs, same per-group elaboration, but no recipe
   emission, no fetch, no build. We merge at the [Oi.Plan.t] level
   rather than via [D10ir.Plan.merge] so the view includes every
   layer the build would touch — including [Binary] cache hits, which
   [Recipe_emitter.emit] strips because [D10ir.Direct.run] doesn't
   need to rebuild them.

   The dedup key is the layer hash, so a package like [uucp.17.0.0]
   resolved into N distinct layer hashes across the overlay's roots
   surfaces as N nodes (one per unique dep set), not one collapsed
   row. That is the whole point of the merged view.

   Per-group failures (solver errors etc.) are logged and the group
   is dropped from the merge — partial output is more useful than no
   output for a 50-root overlay where one root has a conflict. *)
let short_hash h = if String.length h > 12 then String.sub h 0 12 else h

let merged_plan_request ~handles ~refresh ~conf_host : Oi.Build_pipeline.request
    =
  {
    targets =
      List.map (fun h : Oi.Build_pipeline.target -> Overlay_all h) handles;
    with_repos = [];
    pins = [];
    extra_repos = [];
    constraints = OpamPackage.Name.Map.empty;
    toolchain_override = None;
    toolchain = None;
    conf = conf_host;
    local_packages_dir = None;
    project_root = None;
    force_source = false;
    with_test = false;
    refresh;
  }

let group_error_msg = function
  | Oi.Build_pipeline.Solve_failed { msg; _ } -> Fmt.str "solve: %s" msg
  | Cycle _ -> "dependency cycle"
  | Empty_after_strip -> "empty after toolchain strip"
  | Elaborate_failed { msg } -> Fmt.str "elaborate: %s" msg
  | Emit_failed { msg } -> Fmt.str "recipe emit: %s" msg

(* Log per-group failures from the unified pipeline. *)
let log_group_failures groups =
  List.iter
    (fun (gr : Oi.Build_pipeline.group_result) ->
      match gr.error with
      | Ok () -> ()
      | Error e ->
          Log.warn (fun m -> m "skip %s: %s" gr.group.label (group_error_msg e)))
    groups

let merged_groups groups =
  let all_plans =
    List.concat_map
      (fun (gr : Oi.Build_pipeline.group_result) ->
        match gr.exec_plan with Some p -> p.packages | None -> [])
      groups
  in
  let n_solved =
    List.fold_left
      (fun acc (gr : Oi.Build_pipeline.group_result) ->
        if Result.is_ok gr.error then acc + 1 else acc)
      0 groups
  in
  (all_plans, n_solved)

(* Dedup [package_plan]s by layer hash. Same hash from different
   groups → same logical node; first occurrence wins (preserves the
   overlay attribution we'd otherwise lose). *)
let by_hash_of plans =
  let by_hash : (string, Oi.Plan.package_plan) Hashtbl.t =
    Hashtbl.create 1024
  in
  List.iter
    (fun (p : Oi.Plan.package_plan) ->
      if not (Hashtbl.mem by_hash p.layer_hash) then
        Hashtbl.add by_hash p.layer_hash p)
    plans;
  by_hash

(* Roots: layer hashes that are not depended on by any other
   package_plan in the merged view. *)
let plan_roots by_hash =
  let dep_set = Hashtbl.create 1024 in
  Hashtbl.iter
    (fun _ (p : Oi.Plan.package_plan) ->
      List.iter
        (fun (d : Oi.Identity.dep) -> Hashtbl.replace dep_set d.hash ())
        p.dep_layers)
    by_hash;
  Hashtbl.fold
    (fun h p acc -> if Hashtbl.mem dep_set h then acc else p :: acc)
    by_hash []
  |> List.sort (fun (a : Oi.Plan.package_plan) b -> String.compare a.pkg b.pkg)

let render_merged_tree ~bar_label ~os_key ~by_hash roots =
  let label_first (p : Oi.Plan.package_plan) =
    let tag = match p.method_ with Source -> "" | Binary -> " [cached]" in
    Fmt.str "%s  %s%s" p.pkg (short_hash p.layer_hash) tag
  in
  let label_ref (p : Oi.Plan.package_plan) = p.pkg in
  let key_of (p : Oi.Plan.package_plan) = p.layer_hash in
  let children (p : Oi.Plan.package_plan) =
    List.filter_map
      (fun (d : Oi.Identity.dep) -> Hashtbl.find_opt by_hash d.hash)
      p.dep_layers
  in
  Fmt.kstr
    (Fmt.pr "%a@.@." Oi.Style.pp_header_string)
    "Merged plan for %s on %s" bar_label os_key;
  Oi.Dep_tree.render ~label_first ~label_ref ~key_of ~children roots

let print_layer_summary ~by_hash ~n_groups_solved roots =
  let n_layers = Hashtbl.length by_hash in
  let n_source =
    Hashtbl.fold
      (fun _ (p : Oi.Plan.package_plan) acc ->
        if p.method_ = Oi.Identity.Source then acc + 1 else acc)
      by_hash 0
  in
  let n_binary = n_layers - n_source in
  Fmt.pr
    "@.%d group(s) \u{2192} %d unique layer(s) (%d source, %d cached), %d \
     root(s); \u{21B0} marks a back-reference to a layer expanded earlier@."
    n_groups_solved n_layers n_source n_binary (List.length roots)

(* Compute the (pkg, variants) list of packages whose layer hash
   differs across the merged plan. *)
let divergent_variants by_hash =
  let variants_by_pkg : (string, Oi.Plan.package_plan) Hashtbl.t =
    Hashtbl.create 64
  in
  Hashtbl.iter
    (fun _ (p : Oi.Plan.package_plan) -> Hashtbl.add variants_by_pkg p.pkg p)
    by_hash;
  let pkg_set =
    let s = Hashtbl.create 64 in
    Hashtbl.iter
      (fun _ (p : Oi.Plan.package_plan) -> Hashtbl.replace s p.pkg ())
      by_hash;
    s
  in
  Hashtbl.fold
    (fun pkg () acc ->
      let variants = Hashtbl.find_all variants_by_pkg pkg in
      let unique_hashes =
        List.map (fun (p : Oi.Plan.package_plan) -> p.layer_hash) variants
        |> List.sort_uniq String.compare
      in
      if List.length unique_hashes > 1 then (pkg, variants) :: acc else acc)
    pkg_set []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

(* For each layer hash, find every consumer (a [package_plan] in
   the merged set whose [dep_layers] references this hash). *)
let consumers_index by_hash =
  let consumers_of : (string, Oi.Plan.package_plan list) Hashtbl.t =
    Hashtbl.create 1024
  in
  Hashtbl.iter
    (fun _ (p : Oi.Plan.package_plan) ->
      List.iter
        (fun (d : Oi.Identity.dep) ->
          let prev =
            Hashtbl.find_opt consumers_of d.hash
            |> Stdlib.Option.value ~default:[]
          in
          Hashtbl.replace consumers_of d.hash (p :: prev))
        p.dep_layers)
    by_hash;
  fun h ->
    Hashtbl.find_opt consumers_of h
    |> Stdlib.Option.value ~default:[]
    |> List.sort (fun (a : Oi.Plan.package_plan) b ->
        String.compare a.pkg b.pkg)

let take n xs =
  let head, _ =
    List.fold_left
      (fun (acc, i) x -> if i < n then (x :: acc, i + 1) else (acc, i + 1))
      ([], 0) xs
  in
  List.rev head

let inline_names ~max_inline names =
  if List.length names <= max_inline then String.concat ", " names
  else
    Fmt.str "%s, +%d more"
      (String.concat ", " (take max_inline names))
      (List.length names - max_inline)

let consumer_label = function
  | [] -> "(root)"
  | cons ->
      let names = List.map (fun (c : Oi.Plan.package_plan) -> c.pkg) cons in
      inline_names ~max_inline:6 names

let print_variant_line ~consumers_of_hash (v : Oi.Plan.package_plan) =
  let cons_label = consumer_label (consumers_of_hash v.layer_hash) in
  Fmt.pr "  \u{25B8} %s [%s] used by %s@." (short_hash v.layer_hash)
    (Oi.Identity.string_of_method v.method_)
    cons_label

let dep_map (p : Oi.Plan.package_plan) =
  let m = Hashtbl.create (List.length p.dep_layers) in
  List.iter
    (fun (d : Oi.Identity.dep) ->
      Hashtbl.replace m (Oi.Identity.to_string d.id) d.hash)
    p.dep_layers;
  m

let union_keys ma mb =
  let s = Hashtbl.create 32 in
  Hashtbl.iter (fun k _ -> Hashtbl.replace s k ()) ma;
  Hashtbl.iter (fun k _ -> Hashtbl.replace s k ()) mb;
  Hashtbl.fold (fun k () acc -> k :: acc) s [] |> List.sort String.compare

let print_dep_diff_entry n = function
  | Some x, Some y ->
      Fmt.pr "    %s: %s \u{2192} %s@." n (short_hash x) (short_hash y)
  | Some x, None -> Fmt.pr "    %s: %s \u{2192} (absent)@." n (short_hash x)
  | None, Some y -> Fmt.pr "    %s: (absent) \u{2192} %s@." n (short_hash y)
  | None, None -> ()

let print_dep_diff (a : Oi.Plan.package_plan) (b : Oi.Plan.package_plan) =
  let ma = dep_map a and mb = dep_map b in
  let names = union_keys ma mb in
  let diffs =
    List.filter_map
      (fun n ->
        let ah = Hashtbl.find_opt ma n in
        let bh = Hashtbl.find_opt mb n in
        match (ah, bh) with
        | Some x, Some y when String.equal x y -> None
        | _ -> Some (n, ah, bh))
      names
  in
  if diffs <> [] then begin
    Fmt.pr "  dep diff %s vs %s:@." (short_hash a.layer_hash)
      (short_hash b.layer_hash);
    List.iter (fun (n, ah, bh) -> print_dep_diff_entry n (ah, bh)) diffs
  end

(* Pairwise dep diff between consecutive variants — for 2 variants this
   is the full diff; for N>2 it shows the chain A→B, B→C, … which is
   enough to spot the changing axis without N² output. *)
let rec diff_pairs = function
  | [] | [ _ ] -> ()
  | a :: (b :: _ as rest) ->
      print_dep_diff a b;
      diff_pairs rest

let print_variant_block ~consumers_of_hash (pkg, variants) =
  (* Sort variants by hash so the diff output is stable across runs. *)
  let variants =
    List.sort
      (fun (a : Oi.Plan.package_plan) b ->
        String.compare a.layer_hash b.layer_hash)
      variants
  in
  Fmt.pr "%a (%d variants):@." Oi.Style.pp_accent_string pkg
    (List.length variants);
  List.iter (print_variant_line ~consumers_of_hash) variants;
  diff_pairs variants;
  Fmt.pr "@."

(* Divergence summary: any [name.version] that resolved to more
   than one layer hash across the overlay's roots. The hash differs
   because the transitive dep set differs (different version of a
   grandchild, different patch set, etc.); for each variant we
   show its consumers and the entries in [dep_layers] that diverge
   between variants. *)
let report_divergence by_hash =
  let divergent = divergent_variants by_hash in
  if divergent = [] then ()
  else begin
    let consumers_of_hash = consumers_index by_hash in
    Fmt.kstr
      (Fmt.pr "@.%a@.@." Oi.Style.pp_header_string)
      "Divergent layers (%d package%s with multiple variants)"
      (List.length divergent)
      (if List.length divergent = 1 then "" else "s");
    List.iter (print_variant_block ~consumers_of_hash) divergent
  end

let solve_merged ~harness ~handles ~refresh =
  let {
    Harness.proc_mgr;
    fs;
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
  let env : Oi.Build_pipeline.env =
    { proc_mgr; fs; clock; sys; os_key; cache; data_dir; http_session }
  in
  let conf_host =
    Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  let bar_label = String.concat " " (List.map (fun h -> "@" ^ h) handles) in
  let req = merged_plan_request ~handles ~refresh ~conf_host in
  let solved =
    Progress_ui.with_ui ~target:bar_label
      ~clock:(clock :> _ Eio.Resource.t)
      ~enabled:(Tty.is_tty ())
    @@ fun ui_reporter -> Oi.Build_pipeline.solve env ~reporter:ui_reporter req
  in
  (bar_label, solved)

let merged_plan_cmd ~(harness : Harness.env) ~handles ~refresh =
  let os_key = harness.Harness.os_key in
  let bar_label, solved = solve_merged ~harness ~handles ~refresh in
  log_group_failures solved.groups;
  let all_plans, n_groups_solved = merged_groups solved.groups in
  if all_plans = [] then begin
    Fmt.pr "No solvable groups for %s.@." bar_label;
    exit 1
  end;
  let by_hash = by_hash_of all_plans in
  let roots = plan_roots by_hash in
  render_merged_tree ~bar_label ~os_key ~by_hash roots;
  print_layer_summary ~by_hash ~n_groups_solved roots;
  report_divergence by_hash

(* Best-effort open of the layer index; absent / corrupt index → [None],
   downstream renders rows as "declared". *)
let try_open_index ~fs ~cache_root =
  let index_path = cache_root / "layers" / "index.db" in
  if Sys.file_exists index_path then
    try Some (D10.Index.open_ ~fs ~path:index_path)
    with Failure _ | Sys_error _ -> None
  else None

let overlay_latest_packages ~pkgs_dir =
  let names =
    try Sys.readdir pkgs_dir |> Array.to_list with Sys_error _ -> []
  in
  List.filter_map
    (fun name ->
      Stdlib.Option.map (fun v -> (name, v)) (latest_version_of ~pkgs_dir ~name))
    names
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let overlay_row ~db ~os_key (name, ver) =
  let st = lookup_state ?db ~os_key ~name ~ver () in
  let state_span, bins_span = render_state st in
  let row =
    [
      Tty.Span.text name;
      Tty.Span.styled Oi.Style.dim ver;
      state_span;
      bins_span;
    ]
  in
  (row, st)

let overlay_rows ~db ~os_key latest =
  List.fold_right
    (fun pair (rows, c, d) ->
      let row, st = overlay_row ~db ~os_key pair in
      match st with
      | Cached _ -> (row :: rows, c + 1, d)
      | Build_failed | Declared -> (row :: rows, c, d + 1))
    latest ([], 0, 0)

let overlay_table_cols =
  [
    Tty.Table.column "PACKAGE";
    Tty.Table.column "VERSION";
    Tty.Table.column "STATE";
    Tty.Table.column ~max_width:48 "BINARIES";
  ]

let overlay_cmd ~fs ~cache_root ~os_key ~handle =
  let reporepo = Terms.reporepo_path () in
  let pkgs_dir =
    Oi.Source.Reporepo.overlay_packages_dir ~path:reporepo ~handle
  in
  if not (Sys.file_exists pkgs_dir) then begin
    Fmt.kstr
      (Fmt.pr "@[<v>Overlay %a is not materialised under %s.@,Run %a first.@]@."
         Oi.Style.pp_accent_string ("@" ^ handle) reporepo
         Oi.Style.pp_header_string)
      "oi repo bump %s" handle;
    exit 1
  end;
  let latest = overlay_latest_packages ~pkgs_dir in
  if latest = [] then begin
    Fmt.pr "Overlay %a has no packages.@." Oi.Style.pp_accent_string
      ("@" ^ handle);
    exit 0
  end;
  let db = try_open_index ~fs ~cache_root in
  let rows, cached, declared = overlay_rows ~db ~os_key latest in
  Stdlib.Option.iter D10.Index.close db;
  Fmt.kstr
    (Fmt.pr "%a@.@." Oi.Style.pp_header_string)
    "Overlay @%s on %s" handle os_key;
  Oi.Style.pp_table Fmt.stdout
    (Tty.Table.of_rows ~header_style:Oi.Style.header overlay_table_cols rows);
  Fmt.pr "@.%a %d package(s) — %d cached, %d declared@."
    Oi.Style.pp_header_string "Total:" (List.length latest) cached declared

(* -- cache listing ------------------------------------------------------- *)

(* Render every cached layer in [<cache>/layers/<os_key>/], optionally
   filtered to a single overlay handle. Rows: handle, package.version,
   short hash, on-disk size. Sorted by handle then package name.
   Overlay attribution comes from each layer's [provenance.json] sidecar
   — [layer.json] no longer carries it. *)
let overlay_matches_handle ~handle = function
  | None -> handle = None
  | Some (o : D10.Overlay.t) -> (
      match handle with None -> true | Some h -> o.handle = h)

(* Load one cached layer's metadata, dropping rows whose overlay
   doesn't match the optional [handle] filter or whose [layer.json] is
   absent / corrupt. *)
let cache_row_of ~fs ~sys ~layers_dir ~cache_root ~os_key ~handle hash =
  match
    D10.Layer.load_meta Eio.Path.(fs / layers_dir / hash / "layer.json")
  with
  | None -> None
  | Some m ->
      let overlay =
        Oi.Provenance.overlay_of_layer ~fs ~cache_root ~os_key ~hash
      in
      if overlay_matches_handle ~handle overlay then
        let sz = Oi.Cache.size ~sys Eio.Path.(fs / layers_dir / hash / "fs") in
        Some (m, overlay, hash, sz)
      else None

let cache_rows ~fs ~sys ~cache_root ~os_key ~handle =
  let layers_dir = cache_root / "layers" / os_key in
  if not (Sys.file_exists layers_dir) then []
  else
    let entries =
      try Sys.readdir layers_dir |> Array.to_list with Sys_error _ -> []
    in
    List.filter_map
      (cache_row_of ~fs ~sys ~layers_dir ~cache_root ~os_key ~handle)
      entries

let handle_str_of = function
  | Some (o : D10.Overlay.t) -> "@" ^ o.handle
  | None -> ""

let cache_table_cols =
  [
    Tty.Table.column "OVERLAY";
    Tty.Table.column "PACKAGE";
    Tty.Table.column "HASH";
    Tty.Table.column ~align:`Right "SIZE";
  ]

let print_empty_cache_hint = function
  | Some h ->
      Fmt.kstr
        (Fmt.pr "(no cached layers in @%s; run %a to populate)@." h
           Oi.Style.pp_header_string)
        "oi build @%s" h
  | None ->
      Fmt.pr "(no cached layers in the cache; run %a to populate)@."
        Oi.Style.pp_header_string "oi build --all"

let print_cache_table ~os_key ~handle rows =
  let handle_of (o : D10.Overlay.t option) =
    match o with Some o -> o.handle | None -> ""
  in
  let rows =
    List.sort
      (fun ((a : D10.Layer.meta), oa, _, _) (b, ob, _, _) ->
        match String.compare (handle_of oa) (handle_of ob) with
        | 0 -> String.compare a.package b.package
        | c -> c)
      rows
  in
  let header =
    match handle with
    | Some h -> Fmt.str "Cached layers for @%s on %s" h os_key
    | None -> Fmt.str "Cached layers on %s" os_key
  in
  Fmt.pr "%a@.@." Oi.Style.pp_header_string header;
  let total = ref 0L in
  let table_rows =
    List.map
      (fun ((m : D10.Layer.meta), overlay, hash, sz) ->
        total := Int64.add !total sz;
        let short = String.sub hash 0 (min 12 (String.length hash)) in
        [
          Tty.Span.styled Oi.Style.info (handle_str_of overlay);
          Tty.Span.text m.package;
          Tty.Span.styled Oi.Style.dim short;
          Fmt.kstr Tty.Span.text "%a" Oi.Cache.pp_size sz;
        ])
      rows
  in
  let table =
    Tty.Table.of_rows ~header_style:Oi.Style.header cache_table_cols table_rows
  in
  Oi.Style.pp_table Fmt.stdout table;
  Fmt.pr "@.%a %d layer(s), %a total@." Oi.Style.pp_header_string "Summary:"
    (List.length rows) Oi.Cache.pp_size !total

let cache_cmd ~fs ~sys ~cache_root ~os_key ~handle =
  let rows = cache_rows ~fs ~sys ~cache_root ~os_key ~handle in
  if rows = [] then print_empty_cache_hint handle
  else print_cache_table ~os_key ~handle rows

(* All-bare-handle inputs route to the merged-plan view: [oi show @avsm
   @samoht] solves every overlay's roots and renders a single deduped
   tree. Mixed inputs ([@HANDLE] + [pkg.version]) fall through to the
   regular per-target flow. *)
let bare_handles_of = function
  | [] -> None
  | targets ->
      let hs = List.filter_map Target.bare_handle targets in
      if List.compare_lengths hs targets = 0 then Some hs else None

let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  if nl = 0 || nl > sl then false
  else
    let rec loop i =
      if i + nl > sl then false
      else if String.sub s i nl = needle then true
      else loop (i + 1)
    in
    loop 0

let suggest_for ~pkgs_dirs target =
  let lower = String.lowercase_ascii target in
  if String.length lower < 4 then []
  else
    List.concat_map
      (fun dir -> try Sys.readdir dir |> Array.to_list with Sys_error _ -> [])
      pkgs_dirs
    |> List.sort_uniq String.compare
    |> List.filter (fun name ->
        let ln = String.lowercase_ascii name in
        String.length ln >= 4
        && ln <> lower
        && (contains ~needle:lower ln || contains ~needle:ln lower))

let did_you_mean_hint ~pkgs_dirs targets =
  let extras =
    targets
    |> List.concat_map (suggest_for ~pkgs_dirs)
    |> List.sort_uniq String.compare
  in
  match extras with
  | [] -> ""
  | xs ->
      let shown, rest =
        if List.length xs > 8 then
          (List.filteri (fun i _ -> i < 8) xs, List.length xs - 8)
        else (xs, 0)
      in
      Fmt.str "\n\nDid you mean one of these packages?\n  %s%s"
        (String.concat " " shown)
        (if rest > 0 then Fmt.str " (+%d more)" rest else "")

let exec_plan_or_fail ~(group : Oi.Build_pipeline.group_result) ~targets =
  match group.error with
  | Error (Solve_failed { msg; _ }) ->
      let hint = did_you_mean_hint ~pkgs_dirs:group.pkgs_dir targets in
      Oi.Error.no_solution (msg ^ hint)
  | Error (Cycle cycles) ->
      Oi.Error.fail_config_error "dependency cycle in solved packages:@\n%a"
        Oi.Plan.pp_cycles cycles
  | Error (Empty_after_strip | Elaborate_failed _ | Emit_failed _) ->
      Oi.Error.fail_msg "oi show: solve produced no plan"
  | Ok () -> (
      match group.exec_plan with
      | Some xp -> xp
      | None -> Oi.Error.fail_msg "oi show: empty solve result")

let json_plan_node (p : Oi.Plan.package_plan) =
  let opam_pkg = opam_pkg_of_package_plan p in
  let name = OpamPackage.Name.to_string (OpamPackage.name opam_pkg) in
  let version = OpamPackage.Version.to_string (OpamPackage.version opam_pkg) in
  let method_ = Oi.Identity.string_of_method p.method_ in
  let deps = List.map (fun (d : Oi.Identity.dep) -> d.id.name) p.dep_layers in
  (name, version, method_, p.layer_hash, deps)

let json_node_codec =
  let open Jsont in
  Object.map ~kind:"plan_node" (fun name version method_ layer_hash deps ->
      (name, version, method_, layer_hash, deps))
  |> Object.mem "name" string ~enc:(fun (n, _, _, _, _) -> n)
  |> Object.mem "version" string ~enc:(fun (_, v, _, _, _) -> v)
  |> Object.mem "method" string ~enc:(fun (_, _, m, _, _) -> m)
  |> Object.mem "layer_hash" string ~enc:(fun (_, _, _, h, _) -> h)
  |> Object.mem "deps" (list string) ~dec_absent:[]
       ~enc:(fun (_, _, _, _, d) -> d)
       ~enc_omit:(( = ) [])
  |> Object.finish

let json_envelope_codec =
  let open Jsont in
  Object.map ~kind:"oi_show"
    (fun
      _schema_version target os_key ocaml_version toolchain packages depexts ->
      (target, os_key, ocaml_version, toolchain, packages, depexts))
  |> Object.mem "schema_version" string ~enc:(fun _ ->
      Oi.Stamp.json_schema_version)
  |> Object.mem "target" string ~enc:(fun (t, _, _, _, _, _) -> t)
  |> Object.mem "os_key" string ~enc:(fun (_, o, _, _, _, _) -> o)
  |> Object.mem "ocaml_version" string ~enc:(fun (_, _, v, _, _, _) -> v)
  |> Object.opt_mem "toolchain" string ~enc:(fun (_, _, _, t, _, _) -> t)
  |> Object.mem "packages" (list json_node_codec)
       ~enc:(fun (_, _, _, _, p, _) -> p)
  |> Object.mem "depexts" (list string) ~dec_absent:[]
       ~enc:(fun (_, _, _, _, _, d) -> d)
       ~enc_omit:(( = ) [])
  |> Object.finish

let json_target_label ~targets ~project_local_packages =
  match targets with
  | [] -> (
      match project_local_packages with
      | [ p ] -> p
      | _ :: _ :: _ -> "(project)"
      | [] -> "(project)")
  | _ -> String.concat " " targets

let render_json_plan ~(conf : Oi.Solver.Ctx.conf) ~packages_dirs
    ~(exec_plan : Oi.Plan.t) ~os_override ~targets ~project_local_packages
    ~toolchain ~os_key =
  let all_depexts, _ =
    pp_depexts ~conf ~packages_dirs ~plan:exec_plan ~os_override
  in
  let depexts =
    OpamSysPkg.Set.fold
      (fun p acc -> OpamSysPkg.to_string p :: acc)
      all_depexts []
    |> List.sort String.compare
  in
  let nodes = List.map json_plan_node exec_plan.packages in
  let target_label = json_target_label ~targets ~project_local_packages in
  let toolchain_handle =
    Stdlib.Option.map (fun (info : Oi.Toolchain.info) -> info.handle) toolchain
  in
  match
    Jsont_bytesrw.encode_string ~format:Jsont.Indent json_envelope_codec
      ( target_label,
        os_key,
        conf.ocaml_version,
        toolchain_handle,
        nodes,
        depexts )
  with
  | Ok s ->
      print_string s;
      print_newline ()
  | Error e -> Oi.Error.fail_config_error "json encode failed: %s" e

(* Build the [layer_hash -> package_plan] dedup index. Same hash from
   different groups → same node; first occurrence wins. *)
let tree_by_hash nodes =
  let h = Hashtbl.create 64 in
  List.iter
    (fun (p : Oi.Plan.package_plan) ->
      if not (Hashtbl.mem h p.layer_hash) then Hashtbl.add h p.layer_hash p)
    nodes;
  h

let tree_consumed nodes =
  let consumed = Hashtbl.create 64 in
  List.iter
    (fun (p : Oi.Plan.package_plan) ->
      List.iter
        (fun (d : Oi.Identity.dep) -> Hashtbl.replace consumed d.hash ())
        p.dep_layers)
    nodes;
  consumed

let tree_by_name nodes =
  let h = Hashtbl.create 64 in
  List.iter
    (fun (p : Oi.Plan.package_plan) ->
      let n =
        OpamPackage.Name.to_string
          (OpamPackage.name (opam_pkg_of_package_plan p))
      in
      if not (Hashtbl.mem h n) then Hashtbl.add h n p)
    nodes;
  h

let tree_topo_pos nodes =
  let h = Hashtbl.create 64 in
  List.iteri
    (fun i (p : Oi.Plan.package_plan) ->
      if not (Hashtbl.mem h p.layer_hash) then Hashtbl.add h p.layer_hash i)
    nodes;
  h

(* Position in the topologically-sorted plan: deps come first,
   dependents later. Sorting roots by this index makes leaf-roots
   expand at top level before any dependent root reaches them, so
   e.g. on a project depending on both [dune] and [dockerfile],
   [dune] gets a full subtree of its own instead of being rendered
   first as a back-reference under [dockerfile]. *)
let tree_compare_topo topo_pos a b =
  let pos h =
    Stdlib.Option.value (Hashtbl.find_opt topo_pos h) ~default:max_int
  in
  compare (pos a.Oi.Plan.layer_hash) (pos b.Oi.Plan.layer_hash)

let tree_roots ~by_name ~consumed ~topo_pos ~nodes ~names =
  let solver_root_names =
    List.map OpamPackage.Name.to_string names |> List.sort_uniq String.compare
  in
  let from_solver =
    List.filter_map (fun n -> Hashtbl.find_opt by_name n) solver_root_names
    |> List.sort (tree_compare_topo topo_pos)
  in
  if from_solver <> [] then from_solver
  else
    (* Fallback for plans built without a meaningful root set
       (rare — would require [names = []], which the upstream
       "nothing to show" guard already rejects). Keep the graph-root
       behaviour so the tree still renders something useful. *)
    List.filter
      (fun (p : Oi.Plan.package_plan) ->
        not (Hashtbl.mem consumed p.layer_hash))
      nodes

(* Render the elaborated plan as a Unicode dep tree. Each [package_plan]
   keys by its layer_hash (so two identical packages from different
   solve groups collapse to one back-reference); children come from
   [dep_layers].

   Tree roots come from [names] — the solver-root set the user actually
   asked for (CLI targets, [project_deps] from local *.opam files,
   [--with] script deps, URL-project roots). *)
let render_tree_view ~(exec_plan : Oi.Plan.t) ~names =
  let nodes = exec_plan.packages in
  let by_hash = tree_by_hash nodes in
  let consumed = tree_consumed nodes in
  let by_name = tree_by_name nodes in
  let topo_pos = tree_topo_pos nodes in
  let roots = tree_roots ~by_name ~consumed ~topo_pos ~nodes ~names in
  let label_first (p : Oi.Plan.package_plan) =
    Fmt.str "%s  %s" p.pkg (short_hash p.layer_hash)
  in
  let label_ref (p : Oi.Plan.package_plan) = p.pkg in
  let key_of (p : Oi.Plan.package_plan) = p.layer_hash in
  let children (p : Oi.Plan.package_plan) =
    List.filter_map
      (fun (d : Oi.Identity.dep) -> Hashtbl.find_opt by_hash d.hash)
      p.dep_layers
  in
  Fmt.pr "%a@.@." Oi.Style.pp_header_string "Dependency tree";
  Oi.Dep_tree.render ~label_first ~label_ref ~key_of ~children roots;
  Fmt.pr
    "@.%d packages, %d root(s); \u{21B0} marks a back-reference to a layer \
     expanded earlier in the tree@."
    (List.length nodes) (List.length roots)

let render_plan_view (group : Oi.Build_pipeline.group_result) =
  match group.exec_plan with
  | Some plan -> Fmt.pr "%a@." Oi.Plan.pp plan
  | None -> Oi.Error.fail_msg "oi show --plan: no exec plan available"

(* Always print every depext, one per line, with no status marking.
   Intended for piping into a package manager; the caller handles
   which ones are already installed. *)
let render_only_depexts all_depexts =
  OpamSysPkg.Set.iter
    (fun p -> Fmt.pr "%s@." (OpamSysPkg.to_string p))
    all_depexts

let target_primary_fields = function
  | None -> ("", [], None)
  | Some (From_pkg p) ->
      let opam_pkg = opam_pkg_of_package_plan p in
      ( OpamPackage.Version.to_string (OpamPackage.version opam_pkg),
        [ (opam_pkg, p.opam) ],
        Some p.layer_hash )
  | Some (From_project_opams pkgs) ->
      (* Project *.opam files rarely pin a real version; "dev" isn't
         useful on a user-facing line, so we suppress the version
         column here. *)
      ("", pkgs, None)

let render_summary_view ~(conf : Oi.Solver.Ctx.conf) ~targets ~with_repos
    ~project_local_packages ~project_deps ~cwd_s ~toolchain ~cache_root ~os_key
    ~(exec_plan : Oi.Plan.t) ~all_depexts ~dep_status =
  let target_label =
    target_label_of ~targets ~local_packages:project_local_packages
  in
  let overlay = overlay_label_of ~with_repos in
  let n_cached, n_source = counts_of exec_plan in
  let primary =
    pp_primary_meta ~plan:exec_plan ~targets ~project_deps ~cwd:cwd_s
  in
  let target_version, target_opams, target_layer_hash =
    target_primary_fields primary
  in
  let repositories = pp_repositories ?toolchain ~with_repos () in
  let binaries =
    match target_layer_hash with
    | None -> []
    | Some h -> pp_package_binaries ~cache_root ~os_key ~layer_hash:h
  in
  pp_render_info ~target_label ~target_version ~target_opams ~overlay ~os_key
    ~ocaml_version:conf.ocaml_version ~n_cached ~n_source ~all_depexts
    ~dep_status ~repositories ~binaries

let pick_view ~tree:_ ~plan_view ~summary ~only_depexts ~show_all =
  (* [--tree] is the default, so the flag is accepted but never
     branched on — present so users who explicitly type it get the
     same behavior as the default. *)
  if only_depexts || show_all then `Existing
  else if plan_view then `Plan
  else if summary then `Summary
  else `Tree

let render_view ~view ~conf ~packages_dirs ~exec_plan ~os_override
    ~(group : Oi.Build_pipeline.group_result) ~targets ~with_repos
    ~project_local_packages ~project_deps ~cwd_s ~toolchain ~cache_root ~os_key
    ~names ~only_depexts =
  match view with
  | `Plan -> render_plan_view group
  | `Tree -> render_tree_view ~exec_plan ~names
  | (`Existing | `Summary) as _v ->
      let all_depexts, dep_status =
        pp_depexts ~conf ~packages_dirs ~plan:exec_plan ~os_override
      in
      if only_depexts then render_only_depexts all_depexts
      else
        render_summary_view ~conf ~targets ~with_repos ~project_local_packages
          ~project_deps ~cwd_s ~toolchain ~cache_root ~os_key ~exec_plan
          ~all_depexts ~dep_status

type project_state = {
  extras : Oi.Project.extra_repo list;
  pins : Oi.Project.pin list;
  overlays : string list;
  deps : string list;
  local_packages : string list;
  packages_dir : string option;
}

let empty_project_state =
  {
    extras = [];
    pins = [];
    overlays = [];
    deps = [];
    local_packages = [];
    packages_dir = None;
  }

(* Only consult the local project's declarations when the user did not
   name an explicit target; otherwise [oi show pkg] inside a project
   would silently pull the project's own deps into the solve and produce
   misleading output. *)
let load_project_state ~fs ~cwd_s ~targets ~skip_local =
  if targets <> [] || skip_local then empty_project_state
  else
    match Oi.Project.load ~fs cwd_s with
    | exception Sys_error _ -> empty_project_state
    | exception Eio.Exn.Io _ -> empty_project_state
    | p ->
        {
          extras = p.extra_repos;
          pins = p.pins;
          overlays = p.overlays;
          deps = p.deps;
          local_packages = p.local_packages;
          packages_dir = p.packages_dir;
        }

(* [pkg.version] is a common shorthand at the CLI ("show me uchar at
   exactly 0.0.2"). Split each [TARGET] via [parse_pkg_target] so a
   dotted name routes its version into [extra_constraints], leaving
   [names] as the bare package names the solver expects. *)
let split_target_constraints targets =
  let names, cs =
    List.fold_left
      (fun (names, cs) s ->
        let name, vc = Target.parse_pkg_target s in
        let cs =
          match vc with
          | None -> cs
          | Some vc -> OpamPackage.Name.Map.add name vc cs
        in
        (name :: names, cs))
      ([], OpamPackage.Name.Map.empty)
      targets
  in
  (List.rev names, cs)

let extra_names_of extra_deps =
  List.filter_map
    (fun (d : Oi.Project.Script.dep) ->
      if OpamPackage.Name.to_string d.name = "ocaml" then None else Some d.name)
    extra_deps

let solve_label_of = function [] -> "." | xs -> String.concat " " xs

let pipeline_solve ~pipeline_env ~clock ~solve_label req =
  Progress_ui.with_ui ~target:solve_label
    ~clock:(clock :> _ Eio.Resource.t)
    ~enabled:(Tty.is_tty ())
  @@ fun ui_reporter ->
  Oi.Build_pipeline.solve pipeline_env ~reporter:ui_reporter req

let single_group_or_fail (solved : Oi.Build_pipeline.solved) =
  match solved.groups with
  | [ gr ] -> gr
  | _ -> Oi.Error.fail_msg "oi show: unexpected solve group count"

(* Inputs and outputs the [oi show] solve flow threads through the
   bigger render step. Bundled into a record so [cmd]'s argument list
   doesn't balloon. *)
type solve_outputs = {
  conf : Oi.Solver.Ctx.conf;
  exec_plan : Oi.Plan.t;
  group : Oi.Build_pipeline.group_result;
  packages_dirs : string list;
  names : OpamPackage.Name.t list;
  targets : string list;
  with_repos : string list;
  project_local_packages : string list;
  project_deps : string list;
  toolchain : Oi.Toolchain.info option;
  cwd_s : string;
}

let pipeline_env_of ~harness =
  let {
    Harness.proc_mgr;
    fs;
    clock;
    sys;
    os_key;
    cache;
    data_dir;
    http_session;
    _;
  } =
    harness
  in
  ({ proc_mgr; fs; clock; sys; os_key; cache; data_dir; http_session }
    : Oi.Build_pipeline.env)

let build_request ~with_repos ~project_pins ~all_extras ~extra_constraints
    ~toolchain_override ~toolchain ~conf ~local_packages_dir ~refresh ~names :
    Oi.Build_pipeline.request =
  let token_strs = List.map OpamPackage.Name.to_string names in
  {
    targets = [ Group { tokens = token_strs; handles = [] } ];
    with_repos;
    pins = project_pins;
    extra_repos = all_extras;
    constraints = extra_constraints;
    toolchain_override;
    toolchain;
    conf;
    local_packages_dir;
    project_root = None;
    force_source = false;
    with_test = false;
    refresh;
  }

(* Pick the toolchain and assemble [(extras, with_repos, all_extras,
   local_packages_dir)] from the project + cli + url-project sources. *)
let resolve_overlays_and_toolchain ~fs ~sys ~data_dir ~conf ~toolchain_override
    ~handle_pins ~with_repos ~project_extras ~project_overlays
    ~project_packages_dir ~url_project =
  let tc_handles =
    Target.pin_handles handle_pins
    @ Target.handles_of_tokens with_repos
    @ project_overlays
    |> List.sort_uniq String.compare
  in
  let toolchain =
    Oi.Pipeline.pick_toolchain ~fs ~sys ~data_dir ~conf ~install:false
      ~override:toolchain_override ~handles:tc_handles ()
  in
  let project_overlays =
    Oi.Pipeline.filter_compatible_overlays
      ~reporepo_path:(Terms.reporepo_path ()) ~override:toolchain_override
      ~toolchain project_overlays
  in
  let with_repos = project_overlays @ with_repos in
  let cli_extras = Target.cli_extra_repos ~fs ~sys ?toolchain with_repos in
  let all_extras =
    Target.merge_extras ~cli:cli_extras ~project:project_extras
  in
  let local_packages_dir =
    match project_packages_dir with
    | Some _ -> project_packages_dir
    | None ->
        let url : Oi.Project.Url.t = url_project in
        url.packages_dir
  in
  (toolchain, with_repos, cli_extras, all_extras, local_packages_dir)

let solve_names ~toolchain_override ~toolchain ~target_names ~project_dep_names
    ~extra_names ~url_names =
  target_names @ project_dep_names @ extra_names @ url_names
  |> Oi.Pipeline.strip_compiler_roots_for_override ~override:toolchain_override
       ~toolchain

(* Run the [oi show] solve flow end-to-end (toolchain pick, project
   load, classify, request build, solve, exec-plan extraction). Returns
   the bundle the render step needs. *)
(* All the inputs the solver needs, gathered from CLI args, project,
   and url-classified [--with] deps. *)
type solve_request = {
  conf : Oi.Solver.Ctx.conf;
  cwd_s : string;
  targets : string list;
  names : OpamPackage.Name.t list;
  with_repos : string list;
  project_pins : Oi.Project.pin list;
  all_extras : Oi.Project.extra_repo list;
  extra_constraints : OpamFormula.version_constraint OpamPackage.Name.Map.t;
  toolchain : Oi.Toolchain.info option;
  local_packages_dir : string option;
  proj : project_state;
}

let conf_for_os ~platform ~os_override =
  let conf_host =
    Oi.Pipeline.conf ~platform ~ocaml_version:Workspace.ocaml_version
  in
  match os_override with
  | None -> conf_host
  | Some os -> Os_override.resolve conf_host os

(* Build the constraints map: project script constraints + handle-pin
   constraints from [--with] / overlay handles + per-target pkg.version
   constraints. *)
let collect_constraints ~fs ~data_dir ~refresh ~cli_extras ~handle_pins
    ~extra_deps ~target_constraints =
  let base = Oi.Project.Script.constraints extra_deps in
  let handle_constraints =
    Target.handle_pin_constraints ~fs ~data_dir ~refresh ~cli_extras handle_pins
  in
  let cs = OpamPackage.Name.Map.union (fun a _ -> a) handle_constraints base in
  OpamPackage.Name.Map.union (fun existing _ -> existing) cs target_constraints

let collect_solve_request ~harness ~refresh ~skip_local ~toolchain_override
    ~targets ~with_repos ~with_deps ~os_override ~data_dir =
  let { Harness.fs; sys; platform; cache; _ } = harness in
  let conf = conf_for_os ~platform ~os_override in
  let cwd_s, _ = Workspace.resolved_cwd fs in
  let targets, with_repos, target_pins =
    Target.extract_handle_pins ~with_repos targets
  in
  let with_deps, with_repos, with_pins =
    Target.extract_handle_pins ~with_repos with_deps
  in
  let handle_pins = target_pins @ with_pins in
  let extra_deps, url_project =
    Oi.Pipeline.classify_with_args ~fs ~sys ~cache ~refresh with_deps
  in
  let proj = load_project_state ~fs ~cwd_s ~targets ~skip_local in
  let project_extras = proj.extras @ url_project.extra_repos in
  let project_pins = proj.pins @ url_project.pins in
  let project_overlays = proj.overlays @ url_project.overlays in
  let toolchain, with_repos, cli_extras, all_extras, local_packages_dir =
    resolve_overlays_and_toolchain ~fs ~sys ~data_dir ~conf ~toolchain_override
      ~handle_pins ~with_repos ~project_extras ~project_overlays
      ~project_packages_dir:proj.packages_dir ~url_project
  in
  let target_names, target_constraints = split_target_constraints targets in
  let extra_constraints =
    collect_constraints ~fs ~data_dir ~refresh ~cli_extras ~handle_pins
      ~extra_deps ~target_constraints
  in
  let names =
    solve_names ~toolchain_override ~toolchain ~target_names
      ~project_dep_names:(List.map OpamPackage.Name.of_string proj.deps)
      ~extra_names:(extra_names_of extra_deps)
      ~url_names:(List.map OpamPackage.Name.of_string url_project.roots)
  in
  if names = [] then
    Oi.Error.fail_config_error
      "oi show: nothing to show (no TARGET, no --with, and no *.opam files in \
       %s)"
      cwd_s;
  {
    conf;
    cwd_s;
    targets;
    names;
    with_repos;
    project_pins;
    all_extras;
    extra_constraints;
    toolchain;
    local_packages_dir;
    proj;
  }

(* Drive the solve through [Build_pipeline.solve] — same path as [oi
   build TARGET] so target classification, toolchain pickup, and
   pin/extra-repo handling match. *)
let prepare_and_solve ~harness ~refresh ~skip_local ~toolchain_override ~targets
    ~with_repos ~with_deps ~os_override ~data_dir =
  let req_inputs =
    collect_solve_request ~harness ~refresh ~skip_local ~toolchain_override
      ~targets ~with_repos ~with_deps ~os_override ~data_dir
  in
  let pipeline_env = pipeline_env_of ~harness in
  let req =
    build_request ~with_repos:req_inputs.with_repos
      ~project_pins:req_inputs.project_pins ~all_extras:req_inputs.all_extras
      ~extra_constraints:req_inputs.extra_constraints ~toolchain_override
      ~toolchain:req_inputs.toolchain ~conf:req_inputs.conf
      ~local_packages_dir:req_inputs.local_packages_dir ~refresh
      ~names:req_inputs.names
  in
  let { Harness.clock; _ } = harness in
  let solve_label = solve_label_of req_inputs.targets in
  let solved = pipeline_solve ~pipeline_env ~clock ~solve_label req in
  let group = single_group_or_fail solved in
  let exec_plan = exec_plan_or_fail ~group ~targets:req_inputs.targets in
  {
    conf = req_inputs.conf;
    exec_plan;
    group;
    packages_dirs = group.pkgs_dir;
    names = req_inputs.names;
    targets = req_inputs.targets;
    with_repos = req_inputs.with_repos;
    project_local_packages = req_inputs.proj.local_packages;
    project_deps = req_inputs.proj.deps;
    toolchain = req_inputs.toolchain;
    cwd_s = req_inputs.cwd_s;
  }

let man_block =
  [
    `S Manpage.s_description;
    `P
      "Solve for $(b,TARGET) and print its plan, metadata, and depexts. No \
       sources fetched, no builds run.";
    `P "With no $(b,TARGET), reads $(b,*.opam) in the cwd.";
    `P
      "Multiple $(b,TARGET)s are solved as a single group. Bare $(b,@HANDLE) \
       targets switch to the merged-overlay view: every overlay root is solved \
       and rendered as one deduped tree.";
    `S "MODES";
    `I ("(default)", "Dependency tree.");
    `I ("$(b,--tree)", "Same as the default.");
    `I
      ( "$(b,--summary)",
        "Metadata, package counts, binaries, depexts, pinned overlays." );
    `I
      ( "$(b,--plan)",
        "Full per-package plan: layer hashes, source URLs, build/install \
         commands." );
    `I ("$(b,--only-depexts)", "Depexts only, one per line.");
    `I
      ( "bare $(b,@HANDLE)…",
        "Merged build plan across every overlay root, deduped by layer hash." );
    `I
      ( "bare $(b,@HANDLE) $(b,--cache)",
        "Per-overlay package listing (cache state, known binaries)." );
    `I
      ( "$(b,--all)",
        "Cached layers across the whole cache. With bare $(b,@HANDLE), filter \
         to that overlay." );
    `S Manpage.s_examples;
    `Pre
      "  oi show utop\n\
      \  oi show --tree utop\n\
      \  sudo apt install \\$(oi show --only-depexts @avsm/tangled)\n\
      \  oi show --only-depexts --os=fedora-43\n\
      \  oi show --all\n\
      \  oi show @avsm @samoht";
  ]

(* Bare [@HANDLE] dispatch:
   - [--all]: walk every cached layer (handle-filtered).
   - [--cache]: legacy overlay listing of declared / cached pkgs.
   - default: solve every overlay root (or every package, if
     [x-root-packages] is empty), merge the recipes, and print the
     deduped layer-hash dependency tree. Same view that
     [oi build @HANDLE] would execute. Returns [true] if a dispatch
     fired (caller should exit). *)
let dispatch_bare_handle ~harness ~refresh ~fs ~sys ~cache_root ~os_key
    ~bare_handles ~show_all ~show_cache_listing =
  match (bare_handles, show_all, show_cache_listing) with
  | _, true, _ ->
      let h = match bare_handles with Some [ h ] -> Some h | _ -> None in
      cache_cmd ~fs ~sys ~cache_root ~os_key ~handle:h;
      exit 0
  | Some [ h ], false, true ->
      overlay_cmd ~fs ~cache_root ~os_key ~handle:h;
      exit 0
  | Some _, false, true ->
      Oi.Error.fail_config_error
        "oi show --cache: pass exactly one bare @HANDLE to filter the listing"
  | Some handles, false, false ->
      merged_plan_cmd ~harness ~handles ~refresh;
      exit 0
  | None, false, _ -> ()

type view_flags = {
  tree : bool;
  plan_view : bool;
  summary : bool;
  only_depexts : bool;
  show_all : bool;
}

(* Render the chosen view from a solved plan. Pulled out of [cmd] so the
   Cmdliner term stays compact. *)
let run_render ~(common : Terms.common) ~os_override ~os_key ~cache_root
    ~(flags : view_flags) ~(out : solve_outputs) =
  (match common.format with
  | Json ->
      render_json_plan ~conf:out.conf ~packages_dirs:out.packages_dirs
        ~exec_plan:out.exec_plan ~os_override ~targets:out.targets
        ~project_local_packages:out.project_local_packages
        ~toolchain:out.toolchain ~os_key;
      exit 0
  | Text -> ());
  let view =
    pick_view ~tree:flags.tree ~plan_view:flags.plan_view ~summary:flags.summary
      ~only_depexts:flags.only_depexts ~show_all:flags.show_all
  in
  render_view ~view ~conf:out.conf ~packages_dirs:out.packages_dirs
    ~exec_plan:out.exec_plan ~os_override ~group:out.group ~targets:out.targets
    ~with_repos:out.with_repos
    ~project_local_packages:out.project_local_packages
    ~project_deps:out.project_deps ~cwd_s:out.cwd_s ~toolchain:out.toolchain
    ~cache_root ~os_key ~names:out.names ~only_depexts:flags.only_depexts

let run_show (c : Terms.common) refresh ~skip_local ~toolchain_override ~targets
    ~with_repos ~with_deps ~os_override ~(flags : view_flags)
    ~show_cache_listing =
  Harness.run @@ fun ~sw env ->
  let harness =
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  let { Harness.fs; sys; os_key; cache; _ } = harness in
  let data_dir = c.data_dir in
  let bare_handles = bare_handles_of targets in
  let cache_root = Oi.Cache.root_s cache in
  dispatch_bare_handle ~harness ~refresh ~fs ~sys ~cache_root ~os_key
    ~bare_handles ~show_all:flags.show_all ~show_cache_listing;
  Oi.Pipeline.init_opam_root ~fs ~data_dir;
  ignore (Oi.Source.Reporepo.ensure_base ~fs ~sys ~data_dir ~refresh ());
  let out =
    prepare_and_solve ~harness ~refresh ~skip_local ~toolchain_override ~targets
      ~with_repos ~with_deps ~os_override ~data_dir
  in
  run_render ~common:c ~os_override ~os_key ~cache_root ~flags ~out

let targets_arg =
  Arg.(
    value & pos_all string []
    & info ~docv:"TARGET"
        ~doc:
          "Opam package, binary name, $(b,pkg.VERSION), bare $(b,@HANDLE), or \
           $(b,@HANDLE/PKG). Repeatable. Omitted: read $(b,*.opam) in the cwd."
        [])

let tree_arg =
  Arg.(
    value & flag
    & info
        ~doc:
          "Render the dependency graph as a Unicode tree (default view). \
           Back-references to layers expanded earlier are prefixed with \
           $(b,\u{21B0})."
        [ "tree"; "graph" ])

let plan_view_arg =
  Arg.(
    value & flag
    & info
        ~doc:
          "Print the full per-package plan: layer hashes, source URLs, \
           resolved build/install commands."
        [ "plan"; "details" ])

let summary_arg =
  Arg.(
    value & flag
    & info
        ~doc:
          "Print metadata, package counts, depexts (missing marked), and \
           pinned overlays."
        [ "summary"; "meta" ])

let only_depexts_arg =
  Arg.(
    value & flag
    & info ~doc:"Print depexts only, one per line." [ "only-depexts" ])

let os_override_arg =
  Arg.(
    value
    & opt (some string) None
    & info ~docv:"OS"
        ~doc:
          "Evaluate depexts for $(b,OS) instead of the host. Any \
           $(b,dockerfile-opam) tag ($(b,alpine-3.23), $(b,ubuntu-22.04)) or \
           bare distro name. Skips the host-installed check."
        [ "os" ])

let all_arg =
  Arg.(
    value & flag
    & info
        ~doc:
          "List every cached layer. With bare $(b,@HANDLE), filter to that \
           overlay."
        [ "all" ])

let cache_listing_arg =
  Arg.(
    value & flag
    & info
        ~doc:
          "With bare $(b,@HANDLE), list the overlay's declared and cached \
           packages instead of the merged plan. Ignored otherwise."
        [ "cache" ])

let view_flags_term =
  Term.(
    const (fun tree plan_view summary only_depexts show_all ->
        { tree; plan_view; summary; only_depexts; show_all })
    $ tree_arg $ plan_view_arg $ summary_arg $ only_depexts_arg $ all_arg)

let run_show_term =
  Term.(
    const
      (fun
        c
        refresh
        skip_local
        toolchain_override
        targets
        with_repos
        with_deps
        flags
        os_override
        show_cache_listing
      ->
        run_show c refresh ~skip_local ~toolchain_override ~targets ~with_repos
          ~with_deps ~os_override ~flags ~show_cache_listing)
    $ Terms.common $ Terms.refresh $ Terms.skip_local $ Terms.toolchain
    $ targets_arg $ Terms.with_repos $ Terms.with_deps $ view_flags_term
    $ os_override_arg $ cache_listing_arg)

let cmd =
  let info =
    Cmd.info "show" ~doc:"Summarise a target's build plan and depexts"
      ~man:man_block
  in
  Cmd.v info run_show_term
