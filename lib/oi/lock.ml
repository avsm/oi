let ( / ) = Filename.concat

(* Sited at [<data_dir>/.oi.lock] — alongside [<data_dir>/.reporepo.lock] —
   not under [<data_dir>/reporepo/] where a [git clean] inside the working
   tree could sweep it. *)
let path ~data_dir = data_dir / ".oi.lock"

(* The returned [D10.Lock.t] is intentionally discarded: the fd is owned
   by [sw] (via [Eio_unix.Fd.of_unix ~close_unix:true] inside
   [D10.Lock.acquire]), so the lock auto-releases on switch teardown.
   There is no scenario where the caller wants to release early — the
   contract is "hold until the command ends" — so [unit] is the more
   honest return type than handing back a handle no one should touch. *)
let acquire_global ~sw ~clock ~fs ~data_dir () =
  let (_ : D10.Lock.t) =
    D10.Lock.acquire ~sw ~clock ~fs ~path:(path ~data_dir)
      ~mode:D10.Lock.Exclusive ~strategy:D10.Lock.Block ()
  in
  ()
