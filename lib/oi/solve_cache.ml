(** Persistent memoisation of {!Solve.solve} — see the [.mli] for the
    design. *)

let log_src = Logs.Src.create "oi.solve_cache"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

(* Bumped whenever the cache key layout or the marshal value shape
   changes. Old entries are simply ignored on key mismatch and get
   GC'd by [oi cache clean]. *)
let schema_version = "v4"

(* Process-wide memo: [oi registry build --all] asks for the same
   dir's HEAD across many solve groups. Without this, we'd fork a git
   process per group. *)
let head_memo : (string, string option) Hashtbl.t = Hashtbl.create 16

(* Parent of [packages_dir] is the repo root; that's where we ask git
   for HEAD. Returns [None] for non-git dirs or if git isn't on the
   [$PATH]. *)
let compute_git_head repo =
  let cmd =
    Printf.sprintf "git -C %s rev-parse HEAD 2>/dev/null"
      (Filename.quote repo)
  in
  let ic = Unix.open_process_in cmd in
  let line = try Some (String.trim (input_line ic)) with End_of_file -> None in
  match (Unix.close_process_in ic, line) with
  | Unix.WEXITED 0, Some h when h <> "" -> Some h
  | _ -> None

let git_head_for_packages_dir packages_dir =
  match Hashtbl.find_opt head_memo packages_dir with
  | Some r -> r
  | None ->
      let r = compute_git_head (Filename.dirname packages_dir) in
      Hashtbl.add head_memo packages_dir r;
      r

let key ~(conf : Opam_ctx.conf) ~packages_dirs ~constraints ~names =
  let heads =
    List.map (fun d -> (d, git_head_for_packages_dir d)) packages_dirs
  in
  if List.exists (fun (_, h) -> h = None) heads then begin
    List.iter
      (fun (d, h) ->
        if h = None then
          Log.info (fun m ->
              m "solve cache disabled: %s is not a git working tree" d))
      heads;
    None
  end
  else
    let buf = Buffer.create 1024 in
    let add k v =
      Buffer.add_string buf k;
      Buffer.add_char buf '=';
      Buffer.add_string buf v;
      Buffer.add_char buf '\n'
    in
    add "schema" schema_version;
    add "arch" conf.arch;
    add "os" conf.os;
    add "os_distribution" conf.os_distribution;
    add "os_version" conf.os_version;
    add "os_family" conf.os_family;
    add "ocaml_version" conf.ocaml_version;
    (* [packages_dirs] order matters (first-wins enumeration in the
       solver), so preserve it. Path is folded in alongside HEAD so
       two clones of the same repo at the same commit at different
       filesystem paths still share a key. *)
    List.iteri
      (fun i (d, head) ->
        add (Fmt.str "dir%d_path" i) d;
        add
          (Fmt.str "dir%d_head" i)
          (match head with Some h -> h | None -> assert false))
      heads;
    let cs =
      OpamPackage.Name.Map.bindings constraints
      |> List.map (fun (name, (relop, version)) ->
             OpamPackage.Name.to_string name
             ^ " "
             ^ OpamFormula.string_of_relop relop
             ^ " "
             ^ OpamPackage.Version.to_string version)
      |> List.sort String.compare
    in
    List.iter (add "constraint") cs;
    let ns =
      List.map OpamPackage.Name.to_string names |> List.sort String.compare
    in
    List.iter (add "name") ns;
    Some (Digest.to_hex (Digest.string (Buffer.contents buf)))

(* ------------------------------------------------------------------ *)
(* Cache file I/O                                                      *)
(* ------------------------------------------------------------------ *)

let cache_dir ~cache_root = cache_root / "solve-cache"

let key_path ~cache_root ~sub key =
  let shard = String.sub key 0 2 in
  cache_dir ~cache_root / sub / shard / (key ^ ".marshal")

let short_key k = String.sub k 0 (min 12 (String.length k))

(* Generic marshal-backed file read: returns [Some v] on a clean
   decode, [None] otherwise (unlinking the offending file so the next
   lookup falls straight through to the slow path). *)
let read_marshal ~label path =
  if not (Sys.file_exists path) then None
  else
    match
      try
        let ic = open_in_bin path in
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () -> Some (Marshal.from_channel ic))
      with _ -> None
    with
    | Some _ as r ->
        Log.debug (fun m -> m "%s hit %s" label path);
        r
    | None ->
        Log.debug (fun m -> m "%s corrupt %s; unlinking" label path);
        (try Sys.remove path with _ -> ());
        None

let write_marshal ~fs ~label path v =
  let dir = Filename.dirname path in
  try
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dir);
    let tmp = path ^ ".tmp" in
    Out_channel.with_open_bin tmp (fun oc -> Marshal.to_channel oc v []);
    Sys.rename tmp path
  with _ -> Log.debug (fun m -> m "%s store failed for %s" label path)

let lookup ~cache_root ~key =
  match read_marshal ~label:"solve-cache" (key_path ~cache_root ~sub:"pkgs" key) with
  | Some v -> Some (v : OpamPackage.t list)
  | None -> None

let store ~fs ~cache_root ~key pkgs =
  let path = key_path ~cache_root ~sub:"pkgs" key in
  write_marshal ~fs ~label:"solve-cache" path (pkgs : OpamPackage.t list);
  Log.debug (fun m ->
      m "solve-cache stored %s (%d pkgs)" (short_key key) (List.length pkgs))

let lookup_layers ~cache_root ~key =
  match
    read_marshal ~label:"layer-cache" (key_path ~cache_root ~sub:"layers" key)
  with
  | Some v -> Some (v : string list)
  | None -> None

let store_layers ~fs ~cache_root ~key hashes =
  let path = key_path ~cache_root ~sub:"layers" key in
  write_marshal ~fs ~label:"layer-cache" path (hashes : string list);
  Log.debug (fun m ->
      m "layer-cache stored %s (%d layers)" (short_key key)
        (List.length hashes))
