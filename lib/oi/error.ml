[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

type t =
  | Not_found of { target : string; msg : string }
  | No_solution of { msg : string }
  | Config_error of { msg : string }
  | Build_failed of { pkg : string; cmd : string; output : string }
  | Fetch_failed of { url : string; msg : string }
  | Msg of string

exception E of t

let raise e = Stdlib.raise (E e)

let pp fmt = function
  | Not_found { target; msg } ->
      Fmt.pf fmt "%a %s: %s" Fmt.(styled `Red string) "error:" target msg
  | No_solution { msg } ->
      Fmt.pf fmt "%a no solution found@,%s"
        Fmt.(styled `Red string)
        "error:" msg
  | Config_error { msg } ->
      Fmt.pf fmt "%a %s" Fmt.(styled `Red string) "error:" msg
  | Build_failed { pkg; cmd; output } ->
      Fmt.pf fmt "%a package %s failed@,  command: %s@,@,%s"
        Fmt.(styled `Red string)
        "error:" pkg cmd output
  | Fetch_failed { url; msg } ->
      Fmt.pf fmt "%a fetch %s: %s" Fmt.(styled `Red string) "error:" url msg
  | Msg s -> Fmt.pf fmt "%a %s" Fmt.(styled `Red string) "error:" s

let () =
  Printexc.register_printer (function
    | E e -> Some (Fmt.str "%a" pp e)
    | _ -> None)

let not_found target fmt =
  Fmt.kstr (fun msg -> raise (Not_found { target; msg })) fmt

let msg fmt = Fmt.kstr (fun s -> raise (Msg s)) fmt
let no_solution diagnostic = raise (No_solution { msg = diagnostic })
let config_error fmt = Fmt.kstr (fun msg -> raise (Config_error { msg })) fmt
let build_failed ~pkg ~cmd ~output = raise (Build_failed { pkg; cmd; output })
