[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

let ( / ) = Filename.concat

let hash_opam_file packages_dirs pkg =
  let name_s = OpamPackage.Name.to_string (OpamPackage.name pkg) in
  let pkg_s = OpamPackage.to_string pkg in
  let path =
    List.find_map
      (fun dir ->
        let p = dir / name_s / pkg_s / "opam" in
        if Sys.file_exists p then Some p else None)
      packages_dirs
  in
  match path with
  | Some p ->
      OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw p))
      |> OpamFile.OPAM.effective_part |> OpamFile.OPAM.write_to_string
      |> OpamHash.compute_from_string |> OpamHash.to_string
  | None -> Digest.string (OpamPackage.to_string pkg) |> Digest.to_hex

let hash ~packages_dirs pkgs =
  let hashes = List.map (hash_opam_file packages_dirs) pkgs in
  String.concat " " hashes |> Digest.string |> Digest.to_hex

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
        Logs.warn (fun m -> m "Bad layer.json: %s" e);
        None
  with Eio.Exn.Io _ -> None

let dir (c : Config.t) ~hash = Eio.Path.(c.root / "layers" / c.os_key / hash)
let json_path c ~hash = Eio.Path.(dir c ~hash / "layer.json")
let fs_path c ~hash = Eio.Path.(dir c ~hash / "fs")
let native p = Eio.Path.native_exn p
let dir_s c ~hash = native (dir c ~hash)
let exists c ~hash = Sysops.file_exists (json_path c ~hash)

let succeeded c ~hash =
  match load_meta (json_path c ~hash) with
  | Some m -> m.exit_status = 0
  | None -> false

let store (c : Config.t) ~hash ~prefix ~files ~package ~deps ~parent_hashes
    ~exit_status =
  let d = dir_s c ~hash in
  let fs_dir = d / "fs" in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(c.fs / fs_dir);
  let replace_existing f dst =
    try f dst
    with Unix.Unix_error (Unix.EEXIST, _, _) ->
      Sys.remove dst;
      f dst
  in
  List.iter
    (fun rel_path ->
      let src = prefix / rel_path in
      let dst = fs_dir / rel_path in
      match
        try Some (Unix.lstat src).Unix.st_kind with Unix.Unix_error _ -> None
      with
      | Some Unix.S_LNK ->
          let target = Unix.readlink src in
          Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
            Eio.Path.(c.fs / Filename.dirname dst);
          replace_existing (Unix.symlink target) dst
      | Some Unix.S_REG ->
          Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
            Eio.Path.(c.fs / Filename.dirname dst);
          replace_existing (Unix.link src) dst
      | _ -> ())
    files;
  save_meta (json_path c ~hash)
    {
      package;
      exit_status;
      deps;
      hashes = parent_hashes;
      created = Eio.Time.now c.clock;
    }

let restore (c : Config.t) ~hash ~prefix =
  let fs_dir = fs_path c ~hash in
  if Sysops.file_exists fs_dir then
    Sysops.link_tree c.sys ~src:fs_dir ~dst:Eio.Path.(c.fs / prefix)

(* -- Remote registry ----------------------------------------------------- *)

type remote = [ `Http_remote of string ]

(* -- Index --------------------------------------------------------------- *)

(* OINDEX.txt format: one line per layer, sha256sum-compatible.
   <sha256>  <hash>.tar.zst  <size_bytes>
   The size field is informational (for future progress display). *)

type index_entry = { sha256 : string; size : int64 }
type remote_index = (string, index_entry) Hashtbl.t

let parse_index contents =
  let idx = Hashtbl.create 64 in
  String.split_on_char '\n' contents
  |> List.iter (fun line ->
      let parts =
        String.split_on_char ' ' line |> List.filter (fun s -> s <> "")
      in
      match parts with
      | [ sha; filename; size_s ] | [ sha; filename; size_s; _ ] ->
          if Filename.check_suffix filename ".tar.zst" then begin
            let hash = Filename.chop_suffix filename ".tar.zst" in
            let size = try Int64.of_string size_s with Failure _ -> 0L in
            Hashtbl.replace idx hash { sha256 = sha; size }
          end
      | [ sha; filename ] ->
          if Filename.check_suffix filename ".tar.zst" then begin
            let hash = Filename.chop_suffix filename ".tar.zst" in
            Hashtbl.replace idx hash { sha256 = sha; size = 0L }
          end
      | _ -> ());
  idx

let write_index ~dst os_key =
  let os_dir = Eio.Path.(dst / os_key) in
  let files = try Eio.Path.read_dir os_dir with Eio.Exn.Io _ -> [] in
  let entries =
    List.filter_map
      (fun f ->
        if Filename.check_suffix f ".tar.zst" then
          let path = Eio.Path.native_exn Eio.Path.(os_dir / f) in
          let sha256 =
            OpamHash.contents (OpamHash.compute ~kind:`SHA256 path)
          in
          let size = (Unix.stat path).Unix.st_size |> Int64.of_int in
          Some (sha256, f, size)
        else None)
      (List.sort String.compare files)
  in
  let content =
    String.concat ""
      (List.map
         (fun (sha, f, size) -> Fmt.str "%s  %s  %Ld\n" sha f size)
         entries)
  in
  Eio.Path.save ~create:(`Or_truncate 0o644)
    Eio.Path.(os_dir / "OINDEX.txt")
    content

let fetch_remote_index (c : Config.t) ~remote =
  let url =
    match remote with
    | `Http_remote base -> Fmt.str "%s/%s/OINDEX.txt" base c.os_key
  in
  let os_layer_dir = Eio.Path.(c.root / "layers" / c.os_key) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 os_layer_dir;
  let tmp = Eio.Path.(os_layer_dir / "OINDEX.txt.tmp") in
  let idx = Hashtbl.create 64 in
  if Sysops.Curl.fetch c.sys ~url ~dst:tmp then begin
    let contents = Eio.Path.load tmp in
    let parsed = parse_index contents in
    Hashtbl.iter (Hashtbl.replace idx) parsed;
    try Eio.Path.unlink tmp with _ -> ()
  end;
  idx

(* -- Remote pull --------------------------------------------------------- *)

let pull_remote (c : Config.t) ~remote ~hash ?sha256 () =
  if succeeded c ~hash then true
  else begin
    let url =
      match remote with
      | `Http_remote base_url ->
          Fmt.str "%s/%s/%s.tar.zst" base_url c.os_key hash
    in
    let os_layer_dir = Eio.Path.(c.root / "layers" / c.os_key) in
    let layer_dir = dir c ~hash in
    let tmp_file = Eio.Path.(os_layer_dir / (hash ^ ".tar.zst.tmp")) in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 os_layer_dir;
    let ok = Sysops.Curl.fetch c.sys ~url ~dst:tmp_file in
    let cleanup_tmp () = try Eio.Path.unlink tmp_file with _ -> () in
    if ok then begin
      (* Verify sha256 if provided *)
      let checksum_ok =
        match sha256 with
        | None -> true
        | Some expected ->
            let actual =
              OpamHash.contents
                (OpamHash.compute ~kind:`SHA256 (Eio.Path.native_exn tmp_file))
            in
            if actual = expected then true
            else begin
              Logs.warn (fun m ->
                  m "Checksum mismatch for %s: expected %s, got %s" hash
                    expected actual);
              false
            end
      in
      if checksum_ok then begin
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 layer_dir;
        (try Sysops.Tar.extract c.sys ~archive:tmp_file ~dst:layer_dir ()
         with Failure msg -> (
           Logs.warn (fun m -> m "Failed to extract %s: %s" hash msg);
           try Eio.Path.rmtree ~missing_ok:true layer_dir with _ -> ()));
        cleanup_tmp ();
        succeeded c ~hash
      end
      else begin
        cleanup_tmp ();
        false
      end
    end
    else begin
      cleanup_tmp ();
      false
    end
  end

(* -- Export -------------------------------------------------------------- *)

(* Recursively list files under [root], returning paths relative to [root]. *)
let list_files_under root =
  let root_s = Eio.Path.native_exn root in
  let prefix_len = String.length root_s + 1 in
  let files = ref [] in
  let rec scan dir =
    if Sys.file_exists dir && Sys.is_directory dir then
      Array.iter
        (fun name ->
          let path = Filename.concat dir name in
          if Sys.is_directory path then scan path
          else
            files :=
              String.sub path prefix_len (String.length path - prefix_len)
              :: !files)
        (Sys.readdir dir)
  in
  scan root_s;
  List.sort String.compare !files

let export (c : Config.t) ~hash ~dst =
  if not (exists c ~hash) then false
  else
    let os_dir = Eio.Path.(dst / c.os_key) in
    let dst_file = Eio.Path.(os_dir / (hash ^ ".tar.zst")) in
    if Sysops.file_exists dst_file then false
    else begin
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 os_dir;
      (* Atomic publication: stage into .tar.zst.tmp, then rename on
         success. If the process is interrupted mid-tar, the next
         export sees no [dst_file] and retries rather than treating a
         truncated archive as authoritative. *)
      let tmp_file = Eio.Path.(os_dir / (hash ^ ".tar.zst.tmp")) in
      (try Eio.Path.unlink tmp_file with _ -> ());
      Sysops.Tar.create_zstd c.sys ~src:(dir c ~hash) ~dst:tmp_file;
      Eio.Path.rename tmp_file dst_file;
      (* Write compressed file listing relative to fs/ *)
      let fs_dir = fs_path c ~hash in
      if Sysops.file_exists fs_dir then begin
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
        try Eio.Path.unlink txt_tmp with _ -> ()
      end;
      true
    end

let export_all (c : Config.t) ~dst =
  let layers_dir = Eio.Path.(c.root / "layers") in
  if not (Sysops.file_exists layers_dir) then 0
  else
    let os_keys = Eio.Path.read_dir layers_dir in
    let count =
      List.fold_left
        (fun count os_key ->
          let os_layer_dir = Eio.Path.(layers_dir / os_key) in
          let hashes =
            try Eio.Path.read_dir os_layer_dir with Eio.Exn.Io _ -> []
          in
          List.fold_left
            (fun count hash ->
              if String.contains hash '.' then count
              else
                let c = { c with os_key } in
                if succeeded c ~hash then
                  if export c ~hash ~dst then count + 1 else count
                else count)
            count hashes)
        0 os_keys
    in
    (* Write OINDEX.txt for each os_key that has exported layers *)
    List.iter
      (fun os_key ->
        let os_dir = Eio.Path.(dst / os_key) in
        if Sysops.file_exists os_dir then write_index ~dst os_key)
      os_keys;
    count
