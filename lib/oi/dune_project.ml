[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

(** Minimal dune-project reader/writer. *)

module Sexp = Sexplib0.Sexp

type t = { sexps : Sexp.t list; path : string }

let ( / ) = Filename.concat

let load ~fs ~cwd =
  let path = cwd / "dune-project" in
  let content =
    try Eio.Path.load Eio.Path.(fs / path)
    with Eio.Exn.Io _ -> Error.config_error "no dune-project at %s" path
  in
  match Parsexp.Many.parse_string content with
  | Ok sexps -> { sexps; path }
  | Error e ->
      Error.config_error "failed to parse %s: %s" path
        (Parsexp.Parse_error.message e)

(* -- Small sexp helpers -------------------------------------------------- *)

(* Match [(head ...)] where [head] is a bare atom equal to [name]. *)
let is_form name = function
  | Sexp.List (Atom h :: _) when h = name -> true
  | _ -> false

(* Extract the value of [(name X)] inside a package stanza, where X is
   a single atom (the typical shape for [(package (name foo) …)]). *)
let package_name_of_stanza = function
  | Sexp.List items ->
      List.find_map
        (function Sexp.List [ Atom "name"; Atom n ] -> Some n | _ -> None)
        items
  | _ -> None

(* -- Public queries ------------------------------------------------------ *)

let generate_opam_files t =
  List.exists
    (fun s ->
      match s with
      | Sexp.List [ Atom "generate_opam_files" ] -> true
      | Sexp.List [ Atom "generate_opam_files"; Atom "true" ] -> true
      | _ -> false)
    t.sexps

let package_names t =
  List.filter_map
    (fun s -> if is_form "package" s then package_name_of_stanza s else None)
    t.sexps

(* -- Modification -------------------------------------------------------- *)

let dep_sexp ~name ~constraint_ =
  match constraint_ with
  | None -> Sexp.Atom name
  | Some (op, ver) -> Sexp.List [ Atom name; Sexp.List [ Atom op; Atom ver ] ]

(* True if [existing] is already a dep entry for [name], regardless of
   any constraint it carries. Used to keep [add_dependency] idempotent
   on repeated runs. *)
let dep_matches ~name existing =
  match existing with
  | Sexp.Atom n -> n = name
  | Sexp.List (Atom n :: _) -> n = name
  | _ -> false

(* Splice [new_dep] into the [(depends …)] list of [stanza]. If no
   [(depends …)] form exists, append one. *)
let add_to_stanza ~new_dep ~name stanza =
  match stanza with
  | Sexp.List items ->
      let had_depends = ref false in
      let items' =
        List.map
          (function
            | Sexp.List (Atom "depends" :: existing) as orig ->
                had_depends := true;
                if List.exists (dep_matches ~name) existing then orig
                else Sexp.List ((Sexp.Atom "depends" :: existing) @ [ new_dep ])
            | other -> other)
          items
      in
      let items' =
        if !had_depends then items'
        else items' @ [ Sexp.List [ Atom "depends"; new_dep ] ]
      in
      Sexp.List items'
  | other -> other

let add_dependency t ?package ~name ~constraint_ () =
  let new_dep = dep_sexp ~name ~constraint_ in
  (* Locate the target (package …) stanza. *)
  let names = package_names t in
  let target_name =
    match (package, names) with
    | Some n, _ -> n
    | None, [ one ] -> one
    | None, [] ->
        Error.config_error
          "no (package …) stanzas in dune-project; nothing to edit"
    | None, many ->
        Error.config_error
          "multiple packages in dune-project (%s); re-run with -p PKG to pick \
           one"
          (String.concat ", " many)
  in
  let found = ref false in
  let sexps' =
    List.map
      (fun s ->
        if is_form "package" s && package_name_of_stanza s = Some target_name
        then begin
          found := true;
          add_to_stanza ~new_dep ~name s
        end
        else s)
      t.sexps
  in
  if not !found then
    Error.config_error "no (package (name %s) …) stanza in %s" target_name
      t.path;
  { t with sexps = sexps' }

(* -- Writing ------------------------------------------------------------ *)

let save ~fs t =
  let content =
    String.concat "\n\n" (List.map Sexp.to_string_hum t.sexps) ^ "\n"
  in
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / t.path) content
