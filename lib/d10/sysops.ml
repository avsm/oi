[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

let log_src = Logs.Src.create "sysops"

module Log = (val Logs.src_log log_src : Logs.LOG)

type tools = { tar : string }
type pm = [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t

type t = {
  proc_mgr : pm;
  fs : Eio.Fs.dir_ty Eio.Path.t; [@warning "-69"]
  stdout : Eio.Flow.sink_ty Eio.Resource.t option;
  stderr : Eio.Flow.sink_ty Eio.Resource.t option;
  tools : tools;
}

(* -- Low-level helpers --------------------------------------------------- *)

let native p = Eio.Path.native_exn p

let run_quiet t cmd =
  Log.debug (fun m -> m "$ %s" (String.concat " " cmd));
  Eio.Switch.run @@ fun sw ->
  let buf = Buffer.create 256 in
  let sink = Eio.Flow.buffer_sink buf in
  let child = Eio.Process.spawn ~sw t.proc_mgr ~stdout:sink ~stderr:sink cmd in
  match Eio.Process.await child with
  | `Exited 0 ->
      let output = String.trim (Buffer.contents buf) in
      if output <> "" then Log.debug (fun m -> m "%s" output)
  | `Exited n ->
      let output = String.trim (Buffer.contents buf) in
      if output <> "" then Log.debug (fun m -> m "%s" output);
      Fmt.failwith "command exited %d: %s" n (String.concat " " cmd)
  | `Signaled n ->
      Fmt.failwith "command killed by signal %d: %s" n (String.concat " " cmd)

let run_capture t cmd =
  Log.debug (fun m -> m "$ %s" (String.concat " " cmd));
  let out =
    String.trim (Eio.Process.parse_out t.proc_mgr Eio.Buf_read.take_all cmd)
  in
  Log.debug (fun m -> m "%s" out);
  out

(* Spawn [which NAME] with both stdout and stderr routed into a
   throwaway buffer. Nix's [which] writes "no NAME in PATH" to stderr
   on miss; [Eio.Process.parse_out] only captures stdout, so without
   the buffered stderr sink that line leaks to the CLI's own stderr. *)
let has_cmd t name =
  Eio.Switch.run @@ fun sw ->
  let buf = Buffer.create 64 in
  let sink = Eio.Flow.buffer_sink buf in
  let child =
    Eio.Process.spawn ~sw t.proc_mgr ~stdout:sink ~stderr:sink
      [ "which"; name ]
  in
  match Eio.Process.await child with
  | `Exited 0 -> true
  | `Exited _ | `Signaled _ -> false

(* -- Initialisation ------------------------------------------------------ *)

let resolve_tools t =
  let tar = if has_cmd t "gtar" then "gtar" else "tar" in
  { tar }

let create ?stdout ?stderr ~proc_mgr ~fs () =
  let stdout =
    Option.map (fun s -> (s :> Eio.Flow.sink_ty Eio.Resource.t)) stdout
  in
  let stderr =
    Option.map (fun s -> (s :> Eio.Flow.sink_ty Eio.Resource.t)) stderr
  in
  let t_partial =
    { proc_mgr :> pm; fs; stdout; stderr; tools = { tar = "tar" } }
  in
  let tools = resolve_tools t_partial in
  { t_partial with tools }

(* -- File queries -------------------------------------------------------- *)

let file_exists path =
  try
    ignore (Eio.Path.stat ~follow:true path);
    true
  with Eio.Exn.Io _ -> false

(* -- File copying -------------------------------------------------------- *)

let copy_tree t ~src ~dst =
  let src_s = native src and dst_s = native dst in
  try run_quiet t [ "cp"; "-ac"; src_s; dst_s ]
  with Eio.Exn.Io _ ->
    Eio.Path.rmtree ~missing_ok:true dst;
    run_quiet t [ "cp"; "-a"; src_s; dst_s ]

let link_tree t ~src ~dst =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 dst;
  let src_s = native src and dst_s = native dst in
  try run_quiet t [ "cp"; "-Rflv"; src_s ^ "/."; dst_s ^ "/" ]
  with Failure _ -> ()

(* -- Low-level command execution ----------------------------------------- *)

(* Run [cmd] with stdout/stderr inherited from the parent terminal so
   any progress or error output the subprocess writes is shown to the
   user as it happens. Used for interactive-feeling commands like git
   pull/push where hiding the subprocess output would leave the user
   guessing. Falls back to [run_quiet] when no stdout/stderr resources
   were registered on [t]. *)
let run_inherit t cmd =
  Log.debug (fun m -> m "$ %s" (String.concat " " cmd));
  match (t.stdout, t.stderr) with
  | None, _ | _, None -> run_quiet t cmd
  | Some stdout, Some stderr -> (
      Eio.Switch.run @@ fun sw ->
      let child =
        Eio.Process.spawn ~sw t.proc_mgr ~stdout ~stderr cmd
      in
      match Eio.Process.await child with
      | `Exited 0 -> ()
      | `Exited n -> Fmt.failwith "%s exited %d" (List.hd cmd) n
      | `Signaled n ->
          Fmt.failwith "%s killed by signal %d" (List.hd cmd) n)

module Cmd = struct
  let run t cmd = run_quiet t cmd
  let run_out t cmd = run_capture t cmd
  let run_inherit t cmd = run_inherit t cmd
end

(* -- Archive operations -------------------------------------------------- *)

module Tar = struct
  let extract t ~archive ~dst ?(strip = 0) () =
    let cmd =
      [ t.tools.tar; "xf"; native archive; "-C"; native dst ]
      @ if strip > 0 then [ Fmt.str "--strip-components=%d" strip ] else []
    in
    run_quiet t cmd

  let create_zstd t ~src ~dst =
    run_quiet t
      [ t.tools.tar; "--zstd"; "-cf"; native dst; "-C"; native src; "." ]
end

module Curl = struct
  let fetch t ~url ~dst =
    try
      run_quiet t [ "curl"; "-fsSL"; "-o"; native dst; url ];
      true
    with Failure _ -> false
end

(* -- Git operations ------------------------------------------------------ *)

module Git = struct
  let head_short t ~dir =
    run_capture t [ "git"; "-C"; native dir; "rev-parse"; "--short"; "HEAD" ]
end
