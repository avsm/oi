let ( / ) = Filename.concat

(* The reporepo lockfile lives next to (not inside) [<data_dir>/reporepo/]
   so [git clean] inside the working tree never sweeps it. *)
let reporepo_path ~data_dir = data_dir / ".reporepo.lock"

let prefix_path ~cache_root ~prefix_hash =
  cache_root / "prefixes" / (prefix_hash ^ ".lock")

let pin_set_path ~cache_root ~set_id =
  cache_root / "pins" / "sets" / (set_id ^ ".lock")

let acquire_reporepo ?(mode = D10.Lock.Exclusive) ?(strategy = D10.Lock.Block)
    ~sw ~clock ~fs ~data_dir () =
  D10.Lock.acquire ~sw ~clock ~fs ~path:(reporepo_path ~data_dir) ~mode
    ~strategy ()

let with_reporepo ?(mode = D10.Lock.Exclusive) ?(strategy = D10.Lock.Block)
    ~clock ~fs ~data_dir f =
  D10.Lock.with_lock ~clock ~fs ~path:(reporepo_path ~data_dir) ~mode ~strategy
    f

let acquire_prefix ?(mode = D10.Lock.Exclusive) ?(strategy = D10.Lock.Block) ~sw
    ~clock ~fs ~cache_root ~prefix_hash () =
  D10.Lock.acquire ~sw ~clock ~fs
    ~path:(prefix_path ~cache_root ~prefix_hash)
    ~mode ~strategy ()

let with_prefix ?(mode = D10.Lock.Exclusive) ?(strategy = D10.Lock.Block) ~clock
    ~fs ~cache_root ~prefix_hash f =
  D10.Lock.with_lock ~clock ~fs
    ~path:(prefix_path ~cache_root ~prefix_hash)
    ~mode ~strategy f

let acquire_pin_set ?(mode = D10.Lock.Exclusive) ?(strategy = D10.Lock.Block)
    ~sw ~clock ~fs ~cache_root ~set_id () =
  D10.Lock.acquire ~sw ~clock ~fs
    ~path:(pin_set_path ~cache_root ~set_id)
    ~mode ~strategy ()

let with_pin_set ?(mode = D10.Lock.Exclusive) ?(strategy = D10.Lock.Block)
    ~clock ~fs ~cache_root ~set_id f =
  D10.Lock.with_lock ~clock ~fs
    ~path:(pin_set_path ~cache_root ~set_id)
    ~mode ~strategy f
