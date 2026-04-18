[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

let log_src = Logs.Src.create "oi.script"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

type dep = {
  name : OpamPackage.Name.t;
  findlib_name : string;
  constraint_ : OpamFormula.version_constraint option;
}

let split_at_relop s =
  let n = String.length s in
  let rec loop i =
    if i >= n then None
    else
      match s.[i] with
      | '<' | '>' | '=' | '!' -> Some i
      | _ -> loop (i + 1)
  in
  match loop 0 with
  | None -> (s, "")
  | Some i -> (String.sub s 0 i, String.sub s i (n - i))

let parse_dep s =
  let findlib_name, constraint_text = split_at_relop s in
  let opam_name =
    match String.index_opt findlib_name '.' with
    | None -> findlib_name
    | Some i -> String.sub findlib_name 0 i
  in
  let name, constraint_ =
    OpamFormula.atom_of_string (opam_name ^ constraint_text)
  in
  { name; findlib_name; constraint_ }

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

(* First-occurrence dedup keyed on [(opam package, findlib name)]. A
   single script that references both [ppx_deriving.show] and
   [ppx_deriving.eq] — same opam package, different findlib libs —
   produces two entries so both land in [(pps …)]; a literal repeat of
   either entry collapses to one. *)
let dedup deps =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun d ->
      let key = name_s d ^ "#" ^ d.findlib_name in
      if Hashtbl.mem seen key then false
      else begin
        Hashtbl.add seen key ();
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

(* The trailing underscore is deliberate: [ppxlib] is the rewriter
   library itself and belongs in [(libraries …)], not [(pps …)]. *)
let is_ppx name = String.length name >= 4 && String.sub name 0 4 = "ppx_"

let generate_project ~script ~deps ~dir =
  let content = In_channel.with_open_bin script In_channel.input_all in
  Out_channel.with_open_bin (dir / "main.ml") (fun oc ->
      output_string oc content);
  let rev_libs, rev_pps =
    List.fold_left
      (fun (libs, pps) d ->
        let n = name_s d in
        if n = "ocaml" then (libs, pps)
        else if is_ppx n then (libs, d.findlib_name :: pps)
        else (d.findlib_name :: libs, pps))
      ([], []) deps
  in
  let libraries = List.rev rev_libs in
  let pps = List.rev rev_pps in
  Out_channel.with_open_text (dir / "dune-project") (fun oc ->
      Printf.fprintf oc "(lang dune 3.0)\n");
  let dune_content =
    let buf = Buffer.create 256 in
    Printf.bprintf buf "(executable\n (name main)\n";
    if libraries <> [] then
      Printf.bprintf buf " (libraries %s)\n" (String.concat " " libraries);
    if pps <> [] then
      Printf.bprintf buf " (preprocess (pps %s))\n" (String.concat " " pps);
    Printf.bprintf buf ")\n";
    Buffer.contents buf
  in
  Log.debug (fun m ->
      m "generated dune for %s (in %s):@.%s" script dir dune_content);
  Out_channel.with_open_text (dir / "dune") (fun oc ->
      output_string oc dune_content)
