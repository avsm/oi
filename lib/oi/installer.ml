[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

let ( / ) = Filename.concat

module S = OpamFile.Dot_install

let src = Logs.Src.create "oi.installer"

module Log = (val Logs.src_log src : Logs.LOG)

(* "/abs/foo.install" or "foo.install" → "foo". *)
let pkgname_of_install_file path =
  let b = Filename.basename path in
  if Filename.check_suffix b ".install" then Filename.chop_suffix b ".install"
  else b

(* True iff [p] resolves to a path under [base]. Both are canonicalised
   to resolve [..] segments and symlinks before comparison. [p] need not
   exist yet — the comparison is done on the directory portion when the
   leaf is missing. *)
let is_under ~base p =
  let canon p =
    try Unix.realpath p
    with Unix.Unix_error _ -> (
      try Unix.realpath (Filename.dirname p) / Filename.basename p
      with Unix.Unix_error _ -> p)
  in
  let base = canon base in
  let p = canon p in
  let n = String.length base in
  String.length p = n
  || String.length p > n
     && String.sub p 0 n = base
     && p.[n] = Filename.dir_sep.[0]

(* Stream-copy [src_s] to [dst_s] and set the exec bit via Unix.chmod
   (Eio's create perm applies only to newly-created files). Parent
   directories are created with 0o755. Returns false when the source is
   missing and [optional] is true, true on a successful copy. Raises
   [Error.build_failed] on any other I/O failure. *)
let copy_file ~fs ~pkg ~optional ~exec ~src:src_s ~dst:dst_s =
  let perm = if exec then 0o755 else 0o644 in
  let src_path = Eio.Path.(fs / src_s) in
  let dst_path = Eio.Path.(fs / dst_s) in
  match Eio.Path.stat ~follow:true src_path with
  | exception Eio.Exn.Io _ ->
      if optional then false
      else
        Error.build_failed ~pkg ~cmd:"install"
          ~output:(Fmt.str "required source file not found: %s" src_s)
  | _ ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
        Eio.Path.(fs / Filename.dirname dst_s);
      (try
         Eio.Path.with_open_in src_path @@ fun i ->
         Eio.Path.with_open_out ~create:(`Or_truncate perm) dst_path @@ fun o ->
         Eio.Flow.copy i o
       with Eio.Exn.Io (e, _) ->
         Error.build_failed ~pkg ~cmd:"install"
           ~output:(Fmt.str "%s -> %s: %a" src_s dst_s Eio.Exn.pp_err e));
      (try Unix.chmod dst_s perm with Unix.Unix_error _ -> ());
      true

(* Resolve one section. [dst_dir] is the absolute directory the section
   installs into; [dst_opt] supplies an optional basename override. *)
let install_entry ~fs ~pkg ~build_dir ~dst_dir ~exec (base, dst_opt) =
  let base_s = OpamFilename.Base.to_string base.OpamTypes.c in
  let src_s = build_dir / base_s in
  let dst_name =
    match dst_opt with
    | Some d -> OpamFilename.Base.to_string d
    | None -> Filename.basename base_s
  in
  let dst_s = dst_dir / dst_name in
  let _ : bool =
    copy_file ~fs ~pkg ~optional:base.OpamTypes.optional ~exec ~src:src_s
      ~dst:dst_s
  in
  ()

let install ~fs ~prefix ~build_dir ~install_file =
  let pkg = pkgname_of_install_file install_file in
  let inst =
    S.safe_read (OpamFile.make (OpamFilename.of_string install_file))
  in
  let pkg_dir sub = prefix / sub / pkg in
  let global_dir sub = prefix / sub in
  let sections =
    [
      (global_dir "bin", S.bin inst, true);
      (global_dir "sbin", S.sbin inst, true);
      (pkg_dir "lib", S.lib inst, false);
      (pkg_dir "lib", S.libexec inst, true);
      (global_dir "lib", S.lib_root inst, false);
      (global_dir "lib", S.libexec_root inst, true);
      (prefix / "lib" / "toplevel", S.toplevel inst, false);
      (prefix / "lib" / "stublibs", S.stublibs inst, true);
      (global_dir "man", S.man inst, false);
      (pkg_dir "share", S.share inst, false);
      (global_dir "share", S.share_root inst, false);
      (pkg_dir "etc", S.etc inst, false);
      (pkg_dir "doc", S.doc inst, false);
    ]
  in
  List.iter
    (fun (dst_dir, entries, exec) ->
      List.iter (install_entry ~fs ~pkg ~build_dir ~dst_dir ~exec) entries)
    sections;
  (* [misc] entries carry absolute destinations. Only install those that
     land under [prefix]; files outside it cannot be captured by the
     layer diff, so warn and skip. *)
  List.iter
    (fun (base, dst) ->
      let dst_s = OpamFilename.to_string dst in
      if is_under ~base:prefix dst_s then
        let base_s = OpamFilename.Base.to_string base.OpamTypes.c in
        let src_s = build_dir / base_s in
        let _ : bool =
          copy_file ~fs ~pkg ~optional:base.OpamTypes.optional ~exec:false
            ~src:src_s ~dst:dst_s
        in
        ()
      else
        Log.warn (fun m ->
            m "%s: skipping misc file outside prefix: %s" pkg dst_s))
    (S.misc inst)
