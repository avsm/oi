[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

let log_src = Logs.Src.create "d10.layer" ~doc:"d10 layer operations"

module Log = (val Logs.src_log log_src : Logs.LOG)

let ( / ) = Filename.concat

(* Find the first packages_dir in which [<dir>/<name>/<name.version>/opam] exists,
   read it, and hash its effective_part. Returns [None] only if no dir has the file;
   [hash] below treats that as fatal, mirroring day10's [Option.get]. *)
let hash_opam_file packages_dirs pkg =
  let name_s = OpamPackage.Name.to_string (OpamPackage.name pkg) in
  let pkg_s = OpamPackage.to_string pkg in
  List.find_map
    (fun dir ->
      let p = dir / name_s / pkg_s / "opam" in
      if Sys.file_exists p then
        Some
          (OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw p))
          |> OpamFile.OPAM.effective_part |> OpamFile.OPAM.write_to_string
          |> OpamHash.compute_from_string |> OpamHash.to_string)
      else None)
    packages_dirs

let hash ~packages_dirs pkgs =
  let hashes =
    List.map
      (fun pkg ->
        match hash_opam_file packages_dirs pkg with
        | Some h -> h
        | None ->
            Fmt.failwith "layer_hash: no opam file for %s in packages_dirs [%s]"
              (OpamPackage.to_string pkg)
              (String.concat "; " packages_dirs))
      pkgs
  in
  let concat = String.concat " " hashes in
  if Sys.getenv_opt "DAY10_DEBUG_HASH" <> None then
    Fmt.epr "layer_hash input: [%s] -> %S\n%!"
      (String.concat "," (List.map OpamPackage.to_string pkgs))
      concat;
  Digest.string concat |> Digest.to_hex

type meta = {
  package : string;
  exit_status : int;
  deps : string list;
  hashes : string list;
  created : float;
}

let meta_jsont : meta Jsont.t =
  let open Jsont in
  Object.map ~kind:"layer meta" (fun package exit_status deps hashes created ->
      { package; exit_status; deps; hashes; created })
  |> Object.mem "package" string ~enc:(fun i -> i.package)
  |> Object.mem "exit_status" int ~enc:(fun i -> i.exit_status)
  |> Object.mem "deps" (list string) ~dec_absent:[] ~enc:(fun i -> i.deps)
  |> Object.mem "hashes" (list string) ~dec_absent:[] ~enc:(fun i -> i.hashes)
  |> Object.mem "created" number ~enc:(fun i -> i.created)
  |> Object.finish

let meta_codec = meta_jsont

let save_meta path meta =
  let dir = Eio.Path.split path |> Option.map fst in
  Option.iter (Eio.Path.mkdirs ~exists_ok:true ~perm:0o755) dir;
  match Jsont_bytesrw.encode_string meta_jsont meta with
  | Ok s -> Eio.Path.save ~create:(`Or_truncate 0o644) path s
  | Error e -> Fmt.failwith "layer.json encode: %s" e

let load_meta path =
  try
    let s = Eio.Path.load path in
    let file =
      match Eio.Path.native path with Some s -> s | None -> "<layer.json>"
    in
    match Jsont_bytesrw.decode_string ~locs:true ~file meta_jsont s with
    | Ok meta -> Some meta
    | Error e ->
        Log.warn (fun m -> m "Bad layer.json: %s" e);
        None
  with Eio.Exn.Io _ -> None

let dir (c : Config.t) ~hash = Eio.Path.(c.root / "layers" / c.os_key / hash)
let json_path c ~hash = Eio.Path.(dir c ~hash / "layer.json")
let fs_path c ~hash = Eio.Path.(dir c ~hash / "fs")
let recipe_path c ~hash = Eio.Path.(dir c ~hash / "recipe.json")
let native p = Eio.Path.native_exn p
let dir_s c ~hash = native (dir c ~hash)
let exists c ~hash = Sysops.file_exists (json_path c ~hash)
let has_recipe c ~hash = Sysops.file_exists (recipe_path c ~hash)

let load_recipe_json c ~hash =
  if has_recipe c ~hash then
    try Some (Eio.Path.load (recipe_path c ~hash)) with Eio.Exn.Io _ -> None
  else None

let succeeded c ~hash =
  match load_meta (json_path c ~hash) with
  | Some m -> m.exit_status = 0
  | None -> false

(* Streaming copy that preserves the source file's mode bits. Only used
   as the [EXDEV] fallback for [link_or_copy] below — for the same-FS
   common case, [Unix.link] is cheaper and avoids the byte-pump. *)
let copy_file_preserve_mode ~fs ~src ~dst =
  let src_p = Eio.Path.(fs / src) in
  let dst_p = Eio.Path.(fs / dst) in
  let perm = (Eio.Path.stat ~follow:false src_p).perm in
  Eio.Path.with_open_in src_p @@ fun i ->
  Eio.Path.with_open_out ~create:(`Or_truncate perm) dst_p @@ fun o ->
  Eio.Flow.copy i o

(* Hardlink fast-path with a cross-device fallback. [Unix.link] fails with
   [EXDEV] when [src] and [dst] live on different filesystems — typical
   inside containers where the d10 cache is on a BuildKit cache mount and
   the per-build prefix is on the image's overlay. Fall back to a
   streaming copy so the layer's [fs/] tree still gets populated.
   The fallback is per-file, not per-tree, so a single cross-mount build
   doesn't disable hardlinks for the rest of the run.

   Eio has no hardlink primitive, so [Unix.link] stays here — the byte-pump
   fallback is Eio-native. *)
let link_or_copy ~fs ~src ~dst =
  try Unix.link src dst
  with Unix.Unix_error (Unix.EXDEV, _, _) ->
    copy_file_preserve_mode ~fs ~src ~dst

let store (c : Config.t) ~hash ~prefix ~files ~package ~deps ~parent_hashes
    ~exit_status ?recipe_json () =
  let d = dir_s c ~hash in
  let fs_dir = d / "fs" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(c.fs / fs_dir);
  let replace_existing dst_p f =
    try f ()
    with
    | Unix.Unix_error (Unix.EEXIST, _, _)
    | Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _)
    ->
      (try Eio.Path.unlink dst_p with Eio.Exn.Io _ -> ());
      f ()
  in
  List.iter
    (fun rel_path ->
      let src = prefix / rel_path in
      let dst = fs_dir / rel_path in
      let src_p = Eio.Path.(c.fs / src) in
      let dst_p = Eio.Path.(c.fs / dst) in
      let kind =
        try Some (Eio.Path.stat ~follow:false src_p).kind
        with Eio.Exn.Io _ -> None
      in
      match kind with
      | Some `Symbolic_link ->
          let target = Eio.Path.read_link src_p in
          Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
            Eio.Path.(c.fs / Filename.dirname dst);
          replace_existing dst_p (fun () ->
              Eio.Path.symlink ~link_to:target dst_p)
      | Some `Regular_file ->
          Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
            Eio.Path.(c.fs / Filename.dirname dst);
          replace_existing dst_p (fun () -> link_or_copy ~fs:c.fs ~src ~dst)
      | _ -> ())
    files;
  save_meta (json_path c ~hash)
    {
      package;
      exit_status;
      deps;
      hashes = parent_hashes;
      created = Eio.Time.now c.clock;
    };
  match recipe_json with
  | None -> ()
  | Some s -> Eio.Path.save ~create:(`Or_truncate 0o644) (recipe_path c ~hash) s

let restore (c : Config.t) ~hash ~prefix =
  let fs_dir = fs_path c ~hash in
  if Sysops.file_exists fs_dir then
    Sysops.link_tree c.sys ~src:fs_dir ~dst:Eio.Path.(c.fs / prefix)

(* -- Remote registry ----------------------------------------------------- *)

type remote = [ `Http_remote of string ]

(* -- Index --------------------------------------------------------------- *)

type index_entry = { sha256 : string; size : int64 }
type remote_index = (string, index_entry) Hashtbl.t

(* [fetch_remote_index] lives in {!Remote_index} to break the
   [Layer ↔ Index] cycle: reading the remote [index.db] needs
   [Index], and [Index.rebuild] already needs [Layer.load_meta].
   In-tree callers reach for {!Remote_index.fetch} directly. *)

(* -- Remote pull --------------------------------------------------------- *)

type fetch_phase = Fetching | Verifying | Extracting

let verify_sha256 ~hash ~tmp_file ~sha256 =
  match sha256 with
  | None -> true
  | Some expected ->
      let actual =
        OpamHash.contents
          (OpamHash.compute ~kind:`SHA256 (Eio.Path.native_exn tmp_file))
      in
      if actual = expected then true
      else begin
        Log.warn (fun m ->
            m "Checksum mismatch for %s: expected %s, got %s" hash expected
              actual);
        false
      end

let extract_tarball (c : Config.t) ~hash ~tmp_file ~staging_dir =
  try
    Sysops.Tar.extract c.sys ~archive:tmp_file ~dst:staging_dir ();
    true
  with Eio.Io (Sysops.Cmd_failed _, _) as exn ->
    Log.warn (fun m ->
        m "Failed to extract %s: %s" hash (Printexc.to_string exn));
    false

let publish_staging (c : Config.t) ~hash ~staging_dir ~layer_dir =
  (* [rename(2)] on Linux fails with ENOTEMPTY if the destination is a
     non-empty directory; blow away any pre-existing [<hash>/] (already
     proved not [succeeded]) before publishing. *)
  (try Eio.Path.rmtree ~missing_ok:true layer_dir with Eio.Exn.Io _ -> ());
  try
    Eio.Path.rename staging_dir layer_dir;
    succeeded c ~hash
  with exn ->
    Log.warn (fun m ->
        m "Failed to publish layer %s: %s" hash (Printexc.to_string exn));
    false

(* Verify + extract + publish the [<hash>.partial/] staging dir, returning
   [true] iff the layer is now durably present. *)
let finalize_remote_layer (c : Config.t) ~hash ~tmp_file ~staging_dir
    ~layer_dir ~sha256 ~phase =
  phase Verifying;
  let cleanup_tmp () =
    try Eio.Path.unlink tmp_file with Eio.Exn.Io _ -> ()
  in
  let cleanup_staging () =
    try Eio.Path.rmtree ~missing_ok:true staging_dir with Eio.Exn.Io _ -> ()
  in
  if not (verify_sha256 ~hash ~tmp_file ~sha256) then begin
    cleanup_tmp ();
    false
  end
  else begin
    phase Extracting;
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 staging_dir;
    let extract_ok = extract_tarball c ~hash ~tmp_file ~staging_dir in
    cleanup_tmp ();
    if not extract_ok then begin
      cleanup_staging ();
      false
    end
    else if publish_staging c ~hash ~staging_dir ~layer_dir then true
    else begin
      cleanup_staging ();
      false
    end
  end

let pull_remote (c : Config.t) ~session ~remote ~hash ?on_progress ?on_phase
    ?sha256 () =
  if succeeded c ~hash then true
  else
    let url =
      match remote with
      | `Http_remote base_url ->
          Fmt.str "%s/%s/%s.tar.zst" base_url c.os_key hash
    in
    let os_layer_dir = Eio.Path.(c.root / "layers" / c.os_key) in
    let layer_dir = dir c ~hash in
    (* Atomic publication: extract into [<hash>.partial/] and rename to
       [<hash>/] only after the tar exits 0. A torn [tar xf] into [<hash>/]
       would leave the dir looking "succeeded" while [fs/] is half-empty —
       exactly the bug that produced corrupt [ppx_sexp_value] layers in the
       wild. Renaming a sibling sidesteps that: either [<hash>/] is whole,
       or it doesn't exist. *)
    let staging_dir = Eio.Path.(os_layer_dir / (hash ^ ".partial")) in
    let tmp_file = Eio.Path.(os_layer_dir / (hash ^ ".tar.zst.tmp")) in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 os_layer_dir;
    (* Wipe any leftover staging dir from a prior interrupted run. *)
    (try Eio.Path.rmtree ~missing_ok:true staging_dir with Eio.Exn.Io _ -> ());
    let phase p = match on_phase with Some f -> f p | None -> () in
    phase Fetching;
    if Sysops.Http.fetch_session ?on_progress session ~url ~dst:tmp_file then
      finalize_remote_layer c ~hash ~tmp_file ~staging_dir ~layer_dir ~sha256
        ~phase
    else begin
      (try Eio.Path.unlink tmp_file with Eio.Exn.Io _ -> ());
      false
    end

(* -- Export -------------------------------------------------------------- *)

let list_files_under root =
  let root_s = Eio.Path.native_exn root in
  let prefix_len = String.length root_s + 1 in
  let files = ref [] in
  let record_file path =
    files :=
      String.sub path prefix_len (String.length path - prefix_len) :: !files
  in
  let rec visit dir name =
    let path = Filename.concat dir name in
    if Sys.is_directory path then scan path else record_file path
  and scan dir =
    if Sys.file_exists dir && Sys.is_directory dir then
      Array.iter (visit dir) (Sys.readdir dir)
  in
  scan root_s;
  List.sort String.compare !files

let write_file_listing (c : Config.t) ~hash ~os_dir =
  let fs_dir = fs_path c ~hash in
  if not (Sysops.file_exists fs_dir) then ()
  else
    let files = list_files_under fs_dir in
    let content = String.concat "\n" files ^ "\n" in
    let txt_tmp = Eio.Path.(os_dir / (hash ^ ".txt.tmp")) in
    Eio.Path.save ~create:(`Or_truncate 0o644) txt_tmp content;
    Sysops.Cmd.run c.sys
      [
        "zstd";
        "-q";
        "-f";
        Eio.Path.native_exn txt_tmp;
        "-o";
        Eio.Path.native_exn Eio.Path.(os_dir / (hash ^ ".txt.zst"));
      ];
    try Eio.Path.unlink txt_tmp with Eio.Exn.Io _ -> ()

let export (c : Config.t) ~hash ~dst =
  if not (exists c ~hash) then false
  else
    let os_dir = Eio.Path.(dst / c.os_key) in
    let dst_file = Eio.Path.(os_dir / (hash ^ ".tar.zst")) in
    if Sysops.file_exists dst_file then false
    else begin
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 os_dir;
      (* Atomic publication: stage into .tar.zst.tmp, rename on success. If
         the process is interrupted mid-tar, the next export sees no
         [dst_file] and retries rather than treating a truncated archive as
         authoritative. *)
      let tmp_file = Eio.Path.(os_dir / (hash ^ ".tar.zst.tmp")) in
      (try Eio.Path.unlink tmp_file with Eio.Exn.Io _ -> ());
      Sysops.Tar.create_zstd c.sys ~src:(dir c ~hash) ~dst:tmp_file;
      Eio.Path.rename tmp_file dst_file;
      write_file_listing c ~hash ~os_dir;
      true
    end

let export_one_if_succeeded (c : Config.t) ~os_key ~dst count hash =
  if String.contains hash '.' then count
  else
    let c = { c with os_key } in
    if not (succeeded c ~hash) then count
    else if export c ~hash ~dst then count + 1
    else count

let export_for_os_key (c : Config.t) ~layers_dir ~dst count os_key =
  let os_layer_dir = Eio.Path.(layers_dir / os_key) in
  let hashes =
    try Eio.Path.read_dir os_layer_dir with Eio.Exn.Io _ -> []
  in
  List.fold_left (export_one_if_succeeded c ~os_key ~dst) count hashes

let export_all (c : Config.t) ~dst =
  let layers_dir = Eio.Path.(c.root / "layers") in
  if not (Sysops.file_exists layers_dir) then 0
  else
    let os_keys = Eio.Path.read_dir layers_dir in
    List.fold_left (export_for_os_key c ~layers_dir ~dst) 0 os_keys
