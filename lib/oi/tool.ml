[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

let ( / ) = Filename.concat

type trigger =
  | Always
  | Ocamlformat_file
  | Dune_project_using of string

type spec = { name : string; binary : string; trigger : trigger }

let registry =
  [
    { name = "odoc"; binary = "odoc"; trigger = Always };
    { name = "merlin"; binary = "ocamlmerlin"; trigger = Always };
    { name = "ocaml-lsp-server"; binary = "ocamllsp"; trigger = Always };
    { name = "mdx"; binary = "ocaml-mdx"; trigger = Dune_project_using "mdx" };
    { name = "ocamlformat"; binary = "ocamlformat"; trigger = Ocamlformat_file };
  ]

type result = {
  spec : spec;
  hit : bool;
  version : string option;
  detail : string;
}

let hit spec ?version detail = { spec; hit = true; version; detail }
let miss spec detail = { spec; hit = false; version = None; detail }

let read_file path =
  In_channel.with_open_text path In_channel.input_all

let is_regular_file ~fs path =
  match Eio.Path.kind ~follow:true Eio.Path.(fs / path) with
  | `Regular_file -> true
  | _ -> false

(* -- .ocamlformat probe -------------------------------------------------- *)

(* A trimmed-down parser for [.ocamlformat]: we only need the [version =
   X.Y.Z] line. The file is a list of [key = value] lines; comments
   start with [#]. Whitespace around [=] is optional. *)
let parse_ocamlformat_version path =
  let is_version_line line =
    let line = String.trim line in
    if line = "" || line.[0] = '#' then None
    else
      match String.index_opt line '=' with
      | None -> None
      | Some i ->
          let k = String.trim (String.sub line 0 i) in
          let v =
            String.trim (String.sub line (i + 1) (String.length line - i - 1))
          in
          if k = "version" && v <> "" then Some v else None
  in
  String.split_on_char '\n' (read_file path) |> List.find_map is_version_line

let probe_ocamlformat ~fs dir spec =
  let path = dir / ".ocamlformat" in
  if not (is_regular_file ~fs path) then miss spec ".ocamlformat: missing"
  else
    match parse_ocamlformat_version path with
    | Some ver -> hit spec ~version:ver (Fmt.str ".ocamlformat: version = %s" ver)
    | None -> miss spec ".ocamlformat: no 'version = ...' line"

(* -- dune-project (using ...) probe ---------------------------------------- *)

(* Inspect top-level [(using <name> <ver>)] stanzas in [dune-project].
   Uses [parsexp] since dune-project is a sexp. Missing file means
   "not using it". *)
let dune_project_using ~fs dir name =
  let path = dir / "dune-project" in
  if not (is_regular_file ~fs path) then false
  else
    match Parsexp.Many.parse_string (read_file path) with
    | Error _ -> false
    | Ok sexps ->
        List.exists
          (function
            | Sexplib0.Sexp.List (Atom "using" :: Atom plugin :: _)
              when plugin = name ->
                true
            | _ -> false)
          sexps

let probe_using ~fs dir spec plugin =
  if dune_project_using ~fs dir plugin then
    hit spec (Fmt.str "dune-project: (using %s ...)" plugin)
  else miss spec (Fmt.str "dune-project: no (using %s ...)" plugin)

(* -- Entry points -------------------------------------------------------- *)

let probe_one ~fs dir spec =
  match spec.trigger with
  | Always -> hit spec "always"
  | Ocamlformat_file -> probe_ocamlformat ~fs dir spec
  | Dune_project_using plugin -> probe_using ~fs dir spec plugin

let probe ~fs dir = List.map (probe_one ~fs dir) registry
let hits = List.filter (fun r -> r.hit)
