(** [oix]: the global-tool counterpart to [oi]. Wraps [oi run --skip-local] so
    the user can write [oix patdiff --help] without a [--] separator — the
    post-target arguments are handed straight through to the tool.

    Cmdliner can't natively "stop parsing at first positional", so we preprocess
    [Sys.argv]: consume oix's own flags (and their values) until we hit the
    first non-flag token, treat that as [TARGET], and inject a synthetic [--]
    before the rest so cmdliner stops parsing. *)

(* Flags that consume the next argv slot as their value. Must match the
   Cmdliner flag declarations in [Terms] / [Run]. The [--key=val] and
   [-jN] inline-value forms are handled implicitly: the token contains
   its own value, so no slot is consumed. *)
let value_flags =
  [
    "--with";
    "--with-repo";
    "--toolchain";
    "--registry";
    "--cache-dir";
    "--data-dir";
    "--jobs";
    "--color";
    "--verbosity";
    "-c";
    "-j";
  ]

(* Returns [(oix_flags, target_and_after)]. [argv.(0)] is dropped. *)
let split_at_target argv =
  let n = Array.length argv in
  let take_rest i = Array.sub argv i (n - i) |> Array.to_list in
  let is_flag s = String.length s >= 2 && s.[0] = '-' in
  let takes_value tok =
    (* Long [--k]: the bare key must be in [value_flags]. The [--k=v]
       form has its value inline and consumes no slot. Short [-x] is
       checked by its first two chars; [-xN] inlines the value. *)
    if String.contains tok '=' then false
    else if String.length tok >= 2 && tok.[1] = '-' then
      List.mem tok value_flags
    else List.mem (String.sub tok 0 2) value_flags && String.length tok = 2
  in
  let rec loop acc i =
    if i >= n then (List.rev acc, [])
    else
      match argv.(i) with
      | "--" -> (List.rev acc, take_rest (i + 1))
      | tok when not (is_flag tok) -> (List.rev acc, take_rest i)
      | tok when takes_value tok && i + 1 < n ->
          loop (argv.(i + 1) :: tok :: acc) (i + 2)
      | tok -> loop (tok :: acc) (i + 1)
  in
  loop [] 1

(* Subcommand names that exist on [oi] but not on [oix] (oix IS the
   runner — no subcommand). Pulled at run time from
   {!Oi_cmd.Subcommands.names} so adding a new oi subcommand
   automatically updates this guard. *)
let maybe_hint_subcommand target_and_after =
  match target_and_after with
  | first :: rest when List.mem first (Oi_cmd.Subcommands.names ()) ->
      let suggested =
        match (first, rest) with
        | "run", real_target :: _ -> Fmt.str "oix %s" real_target
        | _, real_target :: _ -> Fmt.str "oi %s %s" first real_target
        | _, [] -> Fmt.str "oi %s" first
      in
      Printf.eprintf
        "oix: %S isn't a subcommand — oix is itself the runner.\n\
         Try: %s\n\
         (Use `oi %s …` if you wanted the real `%s` subcommand.)\n"
        first suggested first first;
      exit (Oi.Error.code (Oi.Error.Config_error { msg = "" }))
  | _ -> ()

let () =
  let oix_flags, target_and_after = split_at_target Sys.argv in
  maybe_hint_subcommand target_and_after;
  let argv =
    let head = Sys.argv.(0) :: oix_flags in
    Array.of_list
      (match target_and_after with
      | [] -> head
      | target :: rest -> head @ (target :: "--" :: rest))
  in
  exit (Cmdliner.Cmd.eval ~argv Oi_cmd.Run.cmd_x)
