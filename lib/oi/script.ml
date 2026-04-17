[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

let ( / ) = Filename.concat

type dep = {
  name : OpamPackage.Name.t;
  constraint_ : OpamFormula.version_constraint option;
}

let parse_dep s =
  let name, constraint_ = OpamFormula.atom_of_string s in
  { name; constraint_ }

let parse_deps_from_line line =
  let line = String.trim line in
  let prefix = "[@@@opam " in
  if
    String.length line > String.length prefix + 1
    && String.sub line 0 (String.length prefix) = prefix
    && line.[String.length line - 1] = ']'
  then
    let inner =
      String.sub line (String.length prefix)
        (String.length line - String.length prefix - 1)
    in
    let tokens =
      String.split_on_char ' ' inner
      |> List.map String.trim
      |> List.map (fun s ->
          if
            String.length s >= 2 && s.[0] = '"' && s.[String.length s - 1] = '"'
          then String.sub s 1 (String.length s - 2)
          else s)
      |> List.filter (fun s -> s <> "")
    in
    List.map parse_dep tokens
  else []

let parse_deps_from_file path =
  let ic = open_in path in
  let line = try input_line ic with End_of_file -> "" in
  close_in ic;
  parse_deps_from_line line

let name_s d = OpamPackage.Name.to_string d.name

(* First-occurrence dedup by package name. Keeps the original constraint when
   a later duplicate is unconstrained; if both have a constraint, the first
   wins (callers can reorder to prefer a specific source). *)
let dedup deps =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun d ->
      let n = name_s d in
      if Hashtbl.mem seen n then false
      else begin
        Hashtbl.add seen n ();
        true
      end)
    deps

let script_hash path deps =
  let content = In_channel.with_open_bin path In_channel.input_all in
  let dep_str =
    List.map
      (fun d ->
        let c =
          match d.constraint_ with
          | Some (op, v) ->
              OpamFormula.string_of_relop op ^ OpamPackage.Version.to_string v
          | None -> ""
        in
        name_s d ^ c)
      deps
    |> String.concat ","
  in
  let hash = Digest.string (content ^ "\000" ^ dep_str) |> Digest.to_hex in
  String.sub hash 0 12

let constraints deps =
  List.fold_left
    (fun acc d ->
      match d.constraint_ with
      | None -> acc
      | Some c -> OpamPackage.Name.Map.add d.name c acc)
    OpamPackage.Name.Map.empty deps

let generate_project ~script ~deps ~dir =
  let content = In_channel.with_open_bin script In_channel.input_all in
  Out_channel.with_open_bin (dir / "main.ml") (fun oc ->
      output_string oc content);
  let libraries =
    List.filter_map
      (fun d ->
        let n = name_s d in
        if n = "ocaml" then None else Some n)
      deps
  in
  Out_channel.with_open_text (dir / "dune-project") (fun oc ->
      Printf.fprintf oc "(lang dune 3.0)\n");
  Out_channel.with_open_text (dir / "dune") (fun oc ->
      Printf.fprintf oc "(executable\n (name main)\n";
      if libraries <> [] then
        Printf.fprintf oc " (libraries %s)\n" (String.concat " " libraries);
      Printf.fprintf oc ")\n")
