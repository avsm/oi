(* Hand-rolled tree-indent walk. We don't use [Tty.Tree] because its
   prefix composition appends parent branch chars instead of replacing
   them, so any tree of depth > 1 ends up with [├── │   ├── ] runs in
   the prefix that should have been [│   ├── ]. The standard algorithm
   is small enough to inline. *)

let branch_string ~is_root ~is_last =
  if is_root then "" else if is_last then "└── " else "├── "

let child_indent_string ~is_root ~is_last parent_indent =
  if is_root then ""
  else parent_indent ^ if is_last then "    " else "│   "

let label_of ~label_first ~label_ref ~repeated n =
  if repeated then
    Fmt.str "%a"
      (fun ppf () ->
        Fmt.kstr (Style.pp_dim_string ppf) "\u{21B0} %s" (label_ref n))
      ()
  else label_first n

let walk_children ~child_indent ~walk kids =
  let n_kids = List.length kids in
  List.iteri
    (fun i child ->
      let is_last_child = i = n_kids - 1 in
      walk ~parent_indent:child_indent ~is_last:is_last_child ~is_root:false
        child)
    kids

let render ~label_first ~label_ref ~key_of ~children roots =
  match roots with
  | [] -> Fmt.pr "  (no roots)@."
  | rs ->
      let expanded : (string, unit) Hashtbl.t = Hashtbl.create 64 in
      let rec walk ~parent_indent ~is_last ~is_root n =
        let key = key_of n in
        let repeated = Hashtbl.mem expanded key in
        let branch = branch_string ~is_root ~is_last in
        let label = label_of ~label_first ~label_ref ~repeated n in
        Fmt.pr "%s%s%s@." parent_indent branch label;
        if not repeated then begin
          Hashtbl.add expanded key ();
          let child_indent = child_indent_string ~is_root ~is_last parent_indent in
          walk_children ~child_indent ~walk (children n)
        end
      in
      let n_roots = List.length rs in
      List.iteri
        (fun i r ->
          walk ~parent_indent:"" ~is_last:true ~is_root:true r;
          if i < n_roots - 1 then Fmt.pr "@.")
        rs
