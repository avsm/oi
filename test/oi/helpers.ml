(* Per-test filesystem fixtures.

   Tests that need a writable directory create one under the dune
   sandbox's cwd ([_build/default/test/oi/...]) rather than [/tmp] or
   [~/scratch/tmp/]. Dune already isolates each test in its own
   working directory, so we get host-OS-independent paths without
   coordinating with the user's global temp policy. *)

let rng = lazy (Random.State.make_self_init ())

let mkdir_p path =
  try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let fresh_dir ?(prefix = "t") () =
  let base = Filename.concat (Sys.getcwd ()) "_fixtures" in
  mkdir_p base;
  let st = Lazy.force rng in
  let rec attempt () =
    let suffix = Random.State.bits st in
    let p =
      Filename.concat base
        (Printf.sprintf "%s-%d-%d" prefix (Unix.getpid ()) suffix)
    in
    try
      Unix.mkdir p 0o755;
      p
    with Unix.Unix_error (Unix.EEXIST, _, _) -> attempt ()
  in
  attempt ()

let rec rm_rf path =
  match (Unix.lstat path).st_kind with
  | exception Unix.Unix_error _ -> ()
  | Unix.S_DIR -> (
      let entries = try Sys.readdir path with Sys_error _ -> [||] in
      Array.iter (fun n -> rm_rf (Filename.concat path n)) entries;
      try Unix.rmdir path with Unix.Unix_error _ -> ())
  | _ -> ( try Unix.unlink path with Unix.Unix_error _ -> ())

(* Run [f dir] with [dir] a fresh empty directory. The directory is
   removed afterwards, even on test failure. *)
let with_temp_dir ?prefix f =
  let dir = fresh_dir ?prefix () in
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)

(* Same as {!with_temp_dir} but threads an Eio environment in. Used
   for the manifest writers that take an [Eio.Fs.dir_ty Eio.Path.t]. *)
let with_eio_temp_dir ?prefix f =
  with_temp_dir ?prefix (fun dir ->
      Eio_main.run @@ fun env ->
      let fs = Eio.Stdenv.fs env in
      let clock = Eio.Stdenv.clock env in
      f ~fs ~clock ~dir env)
