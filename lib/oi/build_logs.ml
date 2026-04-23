(** Eio-backed helpers for writing build / solve / fetch diagnostic
    logs under [<cache_root>/build/logs/]. All writers are
    best-effort: I/O failures are swallowed so a full disk or
    permission error on the logs dir can't abort a build report. *)

let ( / ) = Filename.concat

let dir ~cache_root = cache_root / "build" / "logs"

let ensure ~fs ~cache_root =
  try Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dir ~cache_root)
  with _ -> ()

let short_hash h = String.sub h 0 (min 12 (String.length h))

(* Canonical log path: [<cache_root>/build/logs/<kind>-<name>-<short>.log].
   [kind] is a short tag like "build", "fetch", or "solve"; [name] is
   the package or target name; [hash] is a hash string (layer hash
   for builds, or an arbitrary identifier hash for solves) whose
   first 12 chars keep logs from different solve contexts of the same
   [name.version] from clobbering one another. *)
let path ~cache_root ~kind ~name ~hash =
  dir ~cache_root / (kind ^ "-" ^ name ^ "-" ^ short_hash hash ^ ".log")

let write ~fs ~cache_root path content =
  ensure ~fs ~cache_root;
  try Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(fs / path) content
  with _ -> ()
