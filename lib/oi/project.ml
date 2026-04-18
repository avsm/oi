[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Read project metadata from the *.opam files in a directory. *)

let log_src = Logs.Src.create "oi.project"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

type extra_repo = { name : string; url : string }
type pin = { pkg : OpamPackage.t; url : OpamUrl.t; declared_in : string }

type t = {
  deps : string list;
  local_packages : string list;
  extra_repos : extra_repo list;
  pins : pin list;
  overlays : string list;
}

(* -- x-opam-repositories parsing ----------------------------------------- *)

(* Extract a literal string from a FullPos value. *)
let string_of_value (v : OpamParserTypes.FullPos.value) : string option =
  match v.pelem with OpamParserTypes.FullPos.String s -> Some s | _ -> None

(* Parse the value of [x-opam-repositories:] into a list of
   [(name, url)] pairs. The expected shape is
   [[ [name url] [name url] ... ]] — a list of two-element string
   lists. Returns [None] on any deviation from that shape. *)
let parse_extra_repos_value (v : OpamParserTypes.FullPos.value) :
    (string * string) list option =
  match v.pelem with
  | OpamParserTypes.FullPos.List { pelem = items; _ } ->
      let rec loop acc = function
        | [] -> Some (List.rev acc)
        | entry :: rest -> (
            match entry.OpamParserTypes.FullPos.pelem with
            | OpamParserTypes.FullPos.List { pelem = [ n; u ]; _ } -> (
                match (string_of_value n, string_of_value u) with
                | Some name, Some url -> loop ((name, url) :: acc) rest
                | _ -> None)
            | _ -> None)
      in
      loop [] items
  | _ -> None

(* Parse the value of [x-reporepo:] into a list of handle strings.
   Accepts either a single string ([x-reporepo: "avsm"]) or a list of
   strings ([x-reporepo: ["avsm" "samoht"]]). Returns [None] on any
   deviation. *)
let parse_reporepo_value (v : OpamParserTypes.FullPos.value) :
    string list option =
  match v.pelem with
  | OpamParserTypes.FullPos.String s -> Some [ s ]
  | OpamParserTypes.FullPos.List { pelem = items; _ } ->
      let rec loop acc = function
        | [] -> Some (List.rev acc)
        | v :: rest -> (
            match string_of_value v with
            | Some s -> loop (s :: acc) rest
            | None -> None)
      in
      loop [] items
  | _ -> None

(* -- Per-file loading ---------------------------------------------------- *)

type raw = {
  raw_deps : string list;
  raw_extra_repos : (string * string) list;
  raw_pins : pin list;
  raw_overlays : string list;
}

let read_opam_file ~filename path : OpamFile.OPAM.t option =
  try Some (OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path)))
  with exn ->
    Log.warn (fun m ->
        m "Could not parse %s: %s" filename (Printexc.to_string exn));
    None

let load_one ~filename (opam : OpamFile.OPAM.t) : raw =
  (* Direct dependency names from [depends:]. *)
  (* Accumulates in reverse; [load] sorts alphabetically so order doesn't
     matter here. *)
  let raw_deps =
    OpamFormula.fold_left
      (fun acc (name, _) -> OpamPackage.Name.to_string name :: acc)
      []
      (OpamFile.OPAM.depends opam)
  in
  (* [x-opam-repositories:] from the extensions map. *)
  let raw_extra_repos =
    match
      OpamStd.String.Map.find_opt "x-opam-repositories"
        (OpamFile.OPAM.extensions opam)
    with
    | None -> []
    | Some v -> (
        match parse_extra_repos_value v with
        | Some pairs -> pairs
        | None ->
            Error.config_error
              "%s: x-opam-repositories must be a list of [name url] pairs"
              filename)
  in
  (* [pin-depends:] — already typed as [(package * url) list]. *)
  let raw_pins =
    OpamFile.OPAM.pin_depends opam
    |> List.map (fun (pkg, url) -> { pkg; url; declared_in = filename })
  in
  (* [x-reporepo:] — reporepo overlay handles this project wants pulled
     into the solver set at sync/build time. *)
  let raw_overlays =
    match
      OpamStd.String.Map.find_opt "x-reporepo"
        (OpamFile.OPAM.extensions opam)
    with
    | None -> []
    | Some v -> (
        match parse_reporepo_value v with
        | Some hs -> hs
        | None ->
            Error.config_error
              "%s: x-reporepo must be a handle string or a list of handle \
               strings"
              filename)
  in
  { raw_deps; raw_extra_repos; raw_pins; raw_overlays }

(* -- Merging across *.opam files ----------------------------------------- *)

let merge_extra_repos entries =
  (* Preserve first-occurrence order; detect conflicts on repeated name
     with a different URL. *)
  let seen : (string, string * string) Hashtbl.t = Hashtbl.create 8 in
  let ordered = ref [] in
  List.iter
    (fun (declared_in, name, url) ->
      match Hashtbl.find_opt seen name with
      | None ->
          Hashtbl.add seen name (declared_in, url);
          ordered := { name; url } :: !ordered
      | Some (_, prev_url) when prev_url = url -> ()
      | Some (prev_file, prev_url) ->
          Error.config_error
            "package %s and %s disagree on x-opam-repositories entry %s: %s vs \
             %s"
            prev_file declared_in name prev_url url)
    entries;
  List.rev !ordered

let merge_pins entries =
  let seen : (OpamPackage.Name.t, pin) Hashtbl.t = Hashtbl.create 8 in
  let ordered = ref [] in
  List.iter
    (fun (pin : pin) ->
      let name = OpamPackage.name pin.pkg in
      match Hashtbl.find_opt seen name with
      | None ->
          Hashtbl.add seen name pin;
          ordered := pin :: !ordered
      | Some prev
        when OpamPackage.equal prev.pkg pin.pkg
             && OpamUrl.to_string prev.url = OpamUrl.to_string pin.url ->
          ()
      | Some prev ->
          Error.config_error
            "package %s and %s disagree on pin-depends entry %s: %s (%s) vs %s \
             (%s)"
            prev.declared_in pin.declared_in
            (OpamPackage.Name.to_string name)
            (OpamPackage.version_to_string prev.pkg)
            (OpamUrl.to_string prev.url)
            (OpamPackage.version_to_string pin.pkg)
            (OpamUrl.to_string pin.url))
    entries;
  List.rev !ordered

(* -- Public entry point -------------------------------------------------- *)

let load ~fs dir =
  let files =
    Eio.Path.read_dir Eio.Path.(fs / dir)
    |> List.filter (fun f -> Filename.check_suffix f ".opam")
    |> List.sort String.compare
  in
  let local_packages =
    List.map (fun f -> Filename.chop_suffix f ".opam") files
  in
  let local_set = Hashtbl.create 16 in
  List.iter (fun n -> Hashtbl.replace local_set n ()) local_packages;
  let per_file_raws =
    List.filter_map
      (fun filename ->
        let path = dir / filename in
        match read_opam_file ~filename path with
        | None -> None
        | Some opam -> Some (filename, load_one ~filename opam))
      files
  in
  (* Dependencies: sorted, deduplicated, excluding local packages and
     "ocaml". *)
  let dep_set = Hashtbl.create 64 in
  List.iter
    (fun (_, raw) ->
      List.iter
        (fun name ->
          (* compiler handled separately by the solver *)
          if name <> "ocaml" && not (Hashtbl.mem local_set name) then
            Hashtbl.replace dep_set name ())
        raw.raw_deps)
    per_file_raws;
  let deps =
    Hashtbl.fold (fun k () acc -> k :: acc) dep_set []
    |> List.sort String.compare
  in
  (* Extra repos: carry declaring-file along for conflict messages. *)
  let repo_entries =
    List.concat_map
      (fun (filename, raw) ->
        List.map (fun (n, u) -> (filename, n, u)) raw.raw_extra_repos)
      per_file_raws
  in
  let extra_repos = merge_extra_repos repo_entries in
  (* Pins: flatten and merge. *)
  let pin_entries =
    List.concat_map (fun (_, raw) -> raw.raw_pins) per_file_raws
  in
  let pins = merge_pins pin_entries in
  (* Overlays: union across *.opam, preserving first-occurrence order. *)
  let overlays =
    let seen = Hashtbl.create 4 in
    List.concat_map (fun (_, raw) -> raw.raw_overlays) per_file_raws
    |> List.filter (fun h ->
           if Hashtbl.mem seen h then false
           else begin
             Hashtbl.add seen h ();
             true
           end)
  in
  { deps; local_packages; extra_repos; pins; overlays }
