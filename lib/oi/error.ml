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

type kind =
  | K_generic
  | K_system
  | K_config
  | K_no_solution
  | K_fetch
  | K_build
  | K_depext
  | K_not_found
  | K_internal

exception E of t

let raise e = Stdlib.raise (E e)

let kind = function
  | Not_found _ -> K_not_found
  | No_solution _ -> K_no_solution
  | Config_error _ -> K_config
  | Build_failed _ -> K_build
  | Fetch_failed _ -> K_fetch
  | Msg _ -> K_generic

let kind_string = function
  | K_generic -> "generic"
  | K_system -> "system"
  | K_config -> "config"
  | K_no_solution -> "no_solution"
  | K_fetch -> "fetch_failed"
  | K_build -> "build_failed"
  | K_depext -> "depext_missing"
  | K_not_found -> "not_found"
  | K_internal -> "internal"

(* Exit codes mirror opam where the meaning aligns; see [error.mli] for
   the table. Consumers should refer to {!kind_string} or the numeric
   code, never to OCaml constructor names which can be renamed. *)
let code_of_kind = function
  | K_generic -> 1
  | K_system -> 5
  | K_config -> 10
  | K_no_solution -> 20
  | K_fetch -> 30
  | K_build -> 31
  | K_depext -> 50
  | K_not_found -> 65
  | K_internal -> 99

let code e = code_of_kind (kind e)

let pp fmt = function
  | Not_found { target; msg } ->
      (* The constructor's [msg] is the full, user-shaped body — the
         producer already names [target] in context (e.g. "no binary
         `foo` …"), so the printer doesn't re-prepend it. Leaving
         [target] in the constructor record keeps JSON consumers'
         routing intact without leaking duplication into the human
         render. *)
      let _ = target in
      Fmt.pf fmt "%a %s" Style.pp_error_string "error:" msg
  | No_solution { msg } ->
      Fmt.pf fmt "%a no solution found@,%s" Style.pp_error_string "error:" msg
  | Config_error { msg } ->
      Fmt.pf fmt "%a %s" Style.pp_error_string "error:" msg
  | Build_failed { pkg; cmd; output } ->
      (* [pkg] is the full noun phrase ("foo.1.0 in phase build", or
         "3 packages") so the format string just appends "failed".
         Avoids "package 3 packages failed" pluralisation accidents.

         Wrap in [@[<v>...@]] so [@,] becomes a real newline — without
         the vertical box [@,] is a no-op separator and the whole
         message collapses onto one line. *)
      Fmt.pf fmt "@[<v>%a %s failed@,  command: %s@,@,%s@]"
        Style.pp_error_string "error:" pkg cmd output
  | Fetch_failed { url; msg } ->
      Fmt.pf fmt "%a fetch %s: %s" Style.pp_error_string "error:" url msg
  | Msg s -> Fmt.pf fmt "%a %s" Style.pp_error_string "error:" s

let () =
  Printexc.register_printer (function
    | E e -> Fmt.kstr (fun s -> Some s) "%a" pp e
    | _ -> None)

(* One-line summary suitable for the [message] field in JSON. Drops ANSI
   colour, multi-line build logs, and structural prefixes like the
   "no solution found" header that {!pp} adds on its own line. *)
let message = function
  | Not_found { target; msg } -> Fmt.str "%s: %s" target msg
  | No_solution { msg } -> msg
  | Config_error { msg } -> msg
  | Build_failed { pkg; _ } -> Fmt.str "%s failed" pkg
  | Fetch_failed { url; msg } -> Fmt.str "fetch %s: %s" url msg
  | Msg s -> s

(* -- JSON envelope ------------------------------------------------------ *)

(* The [details] subobject is per-kind. We model it as a flat optional
   record carrying every possible field so the codec stays one shape;
   the encoder fills only what's relevant per constructor and the
   [enc_omit] hooks drop the unused ones. New per-kind detail fields
   are additive. *)
type details = {
  pkg : string option;
  command : string option;
  log : string option;
  url : string option;
  target : string option;
}

let empty_details =
  { pkg = None; command = None; log = None; url = None; target = None }

let details_of = function
  | Build_failed { pkg; cmd; output } ->
      {
        empty_details with
        pkg = Some pkg;
        command = Some cmd;
        log = Some output;
      }
  | Fetch_failed { url; _ } -> { empty_details with url = Some url }
  | Not_found { target; _ } -> { empty_details with target = Some target }
  | _ -> empty_details

let details_codec =
  let open Jsont in
  Object.map ~kind:"error_details" (fun pkg command log url target ->
      { pkg; command; log; url; target })
  |> Object.opt_mem "pkg" string ~enc:(fun d -> d.pkg)
  |> Object.opt_mem "command" string ~enc:(fun d -> d.command)
  |> Object.opt_mem "log" string ~enc:(fun d -> d.log)
  |> Object.opt_mem "url" string ~enc:(fun d -> d.url)
  |> Object.opt_mem "target" string ~enc:(fun d -> d.target)
  |> Object.finish

type body = { code : int; kind : string; message : string; details : details }

let body_codec =
  let open Jsont in
  Object.map ~kind:"error_body" (fun code kind message details ->
      { code; kind; message; details })
  |> Object.mem "code" int ~enc:(fun b -> b.code)
  |> Object.mem "kind" string ~enc:(fun b -> b.kind)
  |> Object.mem "message" string ~enc:(fun b -> b.message)
  |> Object.mem "details" details_codec ~dec_absent:empty_details
       ~enc:(fun b -> b.details)
       ~enc_omit:(( = ) empty_details)
  |> Object.finish

type envelope = { schema_version : string; error : body }

let envelope_codec =
  let open Jsont in
  Object.map ~kind:"error_envelope" (fun schema_version error ->
      { schema_version; error })
  |> Object.mem "schema_version" string ~enc:(fun e -> e.schema_version)
  |> Object.mem "error" body_codec ~enc:(fun e -> e.error)
  |> Object.finish

let envelope_of e =
  {
    schema_version = Stamp.json_schema_version;
    error =
      {
        code = code e;
        kind = kind_string (kind e);
        message = message e;
        details = details_of e;
      };
  }

let codec : t Jsont.t =
  Jsont.map
    ~dec:(fun _ -> failwith "Error.codec: decode not supported")
    ~enc:envelope_of envelope_codec

let to_json e =
  match
    Jsont_bytesrw.encode_string ~format:Jsont.Indent envelope_codec
      (envelope_of e)
  with
  | Ok s -> s ^ "\n"
  | Error _ ->
      Fmt.str
        "{\"schema_version\":%S,\"error\":{\"code\":%d,\"kind\":%S,\"message\":%S}}\n"
        Stamp.json_schema_version (code e)
        (kind_string (kind e))
        (message e)

(* -- Constructors -------------------------------------------------------- *)

(* {!Fmt.kstr} formats through a fresh buffer-backed formatter whose
   style renderer defaults to [None] — every [%a Style.pp_*] in the
   message body silently collapses to plain text before the string is
   even built, so by the time {!pp} hands the message to a styled
   stderr there's nothing left to colourise.

   [kstr_styled] pre-sets the buffer formatter's renderer from
   [Fmt.stderr] (which {!Fmt_tty.setup_std_outputs} configures at
   startup per [--color]). The result: ANSI escapes get baked into the
   message string when stderr is styled, and stripped when it isn't —
   matching what the user asked for. *)
let kstr_styled k fmt =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.set_style_renderer ppf (Fmt.style_renderer Fmt.stderr);
  Format.kfprintf
    (fun _ ->
      Format.pp_print_flush ppf ();
      k (Buffer.contents buf))
    ppf fmt

let fail_not_found target fmt =
  kstr_styled (fun msg -> raise (Not_found { target; msg })) fmt

let fail_msg fmt = kstr_styled (fun s -> raise (Msg s)) fmt
let no_solution diagnostic = raise (No_solution { msg = diagnostic })

let fail_config_error fmt =
  kstr_styled (fun msg -> raise (Config_error { msg })) fmt

let build_failed ~pkg ~cmd ~output = raise (Build_failed { pkg; cmd; output })

let fail_fetch_failed ~url fmt =
  kstr_styled (fun msg -> raise (Fetch_failed { url; msg })) fmt
