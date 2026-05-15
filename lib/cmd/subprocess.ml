let ( / ) = Filename.concat

let path_of_env env =
  Array.find_map
    (fun s ->
      if String.starts_with ~prefix:"PATH=" s then
        Some (String.sub s 5 (String.length s - 5))
      else None)
    env

let is_executable p =
  try
    Unix.access p [ Unix.X_OK ];
    true
  with Unix.Unix_error _ -> false

let find_in_path path exe =
  String.split_on_char ':' path
  |> List.find_map (function
    | "" -> None
    | d ->
        let candidate = d / exe in
        if is_executable candidate then Some candidate else None)

let resolve_in_env ~env exe =
  if String.contains exe '/' then exe
  else
    match path_of_env env with
    | None -> exe
    | Some path -> Stdlib.Option.value (find_in_path path exe) ~default:exe

let run proc_mgr ~env cmd =
  let cmd =
    match cmd with exe :: rest -> resolve_in_env ~env exe :: rest | [] -> cmd
  in
  Eio.Switch.run @@ fun sw ->
  let child = Eio.Process.spawn ~sw proc_mgr ~env cmd in
  match Eio.Process.await child with `Exited n -> n | `Signaled n -> 128 + n

let exec ~env cmd =
  match cmd with
  | [] -> invalid_arg "Subprocess.exec: empty argv"
  | exe :: _ -> (
      let prog = resolve_in_env ~env exe in
      try Unix.execve prog (Array.of_list (prog :: List.tl cmd)) env
      with Unix.Unix_error (e, _, _) ->
        Printf.eprintf "oi: cannot exec %s: %s\n%!" prog (Unix.error_message e);
        exit 126)
