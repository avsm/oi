(* [oi dist repo]: sync the per-target artefacts ([oi dist pkg --build]'s
   output) into a published repo tree. Idempotent across runs — adding
   v0.13.5 next to an existing v0.13.4 keeps both versions in the indices.

   Layout under --into REPO/:
     REPO/apt/                 reprepro pool + dists/<codename>/Release
     REPO/rpm/<target>/        createrepo_c metadata + signed .rpm
     REPO/bin/                 static binary tarballs + sha256
     REPO/install.sh           one-shot installer (baked baseurl)
     REPO/INSTALL.md           copy-paste client snippets
     REPO/<pubkey>.asc         armored gpg pubkey

   Every subprocess (gpg / rpmsign / reprepro / createrepo_c) runs inside a
   single throwaway container ([osdist-signer], built once from an embedded
   Dockerfile) with the maintainer's [~/.gnupg] and gpg-agent socket bind-
   mounted in. The host only needs [docker]; gpg / rpm / reprepro /
   createrepo_c do not have to be installed there. Every [docker run] is
   driven through {!D10.Sysops.Cmd} (argv-only, no shell expansion) so a
   subprocess failure raises an [Oi.Error] envelope with the command-line
   preserved. *)

open Cmdliner

let ( / ) = Filename.concat

let absolutize p =
  if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p

let mkdir_p d =
  let rec aux d =
    if d <> "" && d <> "/" && d <> "." && not (Sys.file_exists d) then begin
      aux (Filename.dirname d);
      try Unix.mkdir d 0o755 with Unix.Unix_error _ -> ()
    end
  in
  aux d

let write_file ~path s ~mode =
  mkdir_p (Filename.dirname path);
  let tmp = Fmt.str "%s.tmp.%d" path (Unix.getpid ()) in
  Out_channel.with_open_bin tmp (fun oc -> Out_channel.output_string oc s);
  Unix.chmod tmp mode;
  Sys.rename tmp path

let copy_file ~src ~dst =
  mkdir_p (Filename.dirname dst);
  let ic = open_in_bin src in
  let oc = open_out_bin dst in
  let buf = Bytes.create 65536 in
  Fun.protect
    ~finally:(fun () ->
      close_in ic;
      close_out oc)
    (fun () ->
      let rec loop () =
        let n = input ic buf 0 (Bytes.length buf) in
        if n > 0 then begin
          output oc buf 0 n;
          loop ()
        end
      in
      loop ())

let files_in ?(suffix = "") dir =
  if not (Sys.file_exists dir) then []
  else
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun n -> suffix = "" || Filename.check_suffix n suffix)
    |> List.sort String.compare
    |> List.map (fun n -> dir / n)

(* -- Sysops wrappers ----------------------------------------------------
   Every external command goes through one of these. They share the same
   error-envelope translation — an [Eio.Io] from a non-zero subprocess
   becomes an [Oi.Error.fail_config_error] with the full argv preserved
   for diagnosis. *)

let pp_argv ppf argv =
  Fmt.pf ppf "%s" (String.concat " " (List.map Filename.quote argv))

let cmd_run ~sys argv =
  try D10.Sysops.Cmd.run sys argv
  with Eio.Io _ as exn ->
    Oi.Error.fail_config_error "oi dist repo: %a failed: %s" pp_argv argv
      (Printexc.to_string exn)

let cmd_out ~sys argv =
  try D10.Sysops.Cmd.run_out sys argv
  with Eio.Io _ as exn ->
    Oi.Error.fail_config_error "oi dist repo: %a failed: %s" pp_argv argv
      (Printexc.to_string exn)

(* -- Signing primitives -------------------------------------------------
   Every gpg / rpmsign / reprepro / createrepo_c call runs inside a single
   container image, [osdist-signer], built once on first use. The maintainer's
   [~/.gnupg] and (when present) [$XDG_RUNTIME_DIR/gnupg] are bind-mounted in
   so the in-container gpg reaches the host's keyring and agent socket. This
   is a convenience choice — the container has the same key authority as the
   host — to keep host tooling requirements down to just docker. *)

let signer_image = "osdist-signer:1"

(* Bundle every CLI we need into one image so the repo step never pulls a
   second base image. Built once per host; docker layer cache makes
   subsequent [docker build]s near-instant. *)
let signer_dockerfile =
  {|FROM debian:13
RUN apt-get update \
 && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      ca-certificates gnupg reprepro createrepo-c rpm \
 && rm -rf /var/lib/apt/lists/*
|}

type signer_ctx = {
  uid : int;
  gid : int;
  home : string;
  xdg_runtime : string option;
      (** [Some d] when [$XDG_RUNTIME_DIR] (or [/run/user/<uid>]) exists on the
          host; bind-mounted so the container's gpg can reach the host's
          gpg-agent socket. [None] disables agent forwarding (CI scenarios where
          the key is passphraseless). *)
}

let build_signer_ctx () =
  let uid = Unix.getuid () in
  let gid = Unix.getgid () in
  let home = try Sys.getenv "HOME" with Not_found -> "/root" in
  let xdg_runtime =
    let from_env =
      try Some (Sys.getenv "XDG_RUNTIME_DIR") with Not_found -> None
    in
    let fallback = Fmt.str "/run/user/%d" uid in
    match from_env with
    | Some d when Sys.file_exists d -> Some d
    | _ when Sys.file_exists fallback -> Some fallback
    | _ -> None
  in
  { uid; gid; home; xdg_runtime }

(* -- GPG key auto-detection -------------------------------------------- *)

(* One signing-capable secret key as reported by [gpg --list-secret-keys
   --with-colons]. [algos] and [curves] cover the primary + every
   sign-capable subkey (in [gpg]'s listing order); a cert-only primary
   with an Ed25519 signing subkey still surfaces "ed25519" here, which
   matters because gpg picks the right subkey automatically when given
   the primary fingerprint. *)
type signing_key = {
  fingerprint : string;
  algos : string list;
  curves : string list;
  uid : string;
}

(* Parse [gpg --list-secret-keys --with-colons --with-fingerprint]. Each
   secret key starts with a [sec:] line followed by its [fpr:],
   [uid:], and zero-or-more [ssb:] (subkey) records before the next
   [sec:]. We collect sec records the whole key can sign with ([s] in
   the primary or [S] aggregated from subkeys, field 12), filter out
   expired / revoked / disabled / invalid ones (field 2), and fold in
   the algo / curve of any sign-capable subkey alongside the
   primary's. *)
let parse_signing_keys output =
  let valid_status v = not (List.mem v [ "i"; "d"; "r"; "e" ]) in
  let signs caps = String.contains caps 's' || String.contains caps 'S' in
  let lines = String.split_on_char '\n' output in
  let keys = ref [] in
  let current = ref None in
  let flush () =
    (match !current with
    | Some k when k.fingerprint <> "" -> keys := k :: !keys
    | _ -> ());
    current := None
  in
  let field arr i = if i < Array.length arr then arr.(i) else "" in
  List.iter
    (fun line ->
      let f = String.split_on_char ':' line |> Array.of_list in
      match field f 0 with
      | "sec" ->
          flush ();
          let caps = field f 11 in
          let status = field f 1 in
          if valid_status status && signs caps then
            current :=
              Some
                {
                  fingerprint = "";
                  algos = [ field f 3 ];
                  curves = [ field f 16 ];
                  uid = "";
                }
      | "ssb" -> (
          (* Sign-capable subkey: append its algo + curve so a
             cert-only primary with an Ed25519 sign subkey scores as
             ed25519 in [pick_signing_key]. *)
          match !current with
          | None -> ()
          | Some k ->
              let caps = field f 11 in
              let status = field f 1 in
              if valid_status status && signs caps then
                current :=
                  Some
                    {
                      k with
                      algos = k.algos @ [ field f 3 ];
                      curves = k.curves @ [ field f 16 ];
                    })
      | "fpr" -> (
          match !current with
          | Some k when k.fingerprint = "" ->
              current := Some { k with fingerprint = field f 9 }
          | _ -> ())
      | "uid" -> (
          match !current with
          | Some k when k.uid = "" -> current := Some { k with uid = field f 9 }
          | _ -> ())
      | _ -> ())
    lines;
  flush ();
  List.rev !keys

(* List the host's signing-capable secret keys via the host gpg. Goes
   through [D10.Sysops.Cmd.run_out_quiet] so stderr is muzzled (gpg
   prints "gpg: no secret keys" to stderr if the keyring is empty —
   not an error). Returns [[]] when gpg isn't installed or the
   process exits non-zero. *)
let list_signing_keys ~sys =
  try
    parse_signing_keys
      (D10.Sysops.Cmd.run_out_quiet sys
         [ "gpg"; "--list-secret-keys"; "--with-colons"; "--with-fingerprint" ])
  with _ -> []

(* Ed25519 anywhere in the key (primary or subkey) first; otherwise
   the first sign-capable key gpg returned. *)
let pick_signing_key keys =
  match List.find_opt (fun k -> List.mem "ed25519" k.curves) keys with
  | Some _ as r -> r
  | None -> List.nth_opt keys 0

let algo_name = function
  | "1" -> "rsa"
  | "17" -> "dsa"
  | "19" -> "ecdsa"
  | "22" -> "eddsa"
  | other -> "algo-" ^ other

(* Short label for the auto-selection log line: [ed25519] when it's
   the primary or any subkey's curve; otherwise the primary's curve
   (if named) or its algorithm code. *)
let key_kind k =
  if List.mem "ed25519" k.curves then "ed25519"
  else
    match (k.curves, k.algos) with
    | c :: _, _ when c <> "" -> c
    | _, a :: _ -> algo_name a
    | _ -> "unknown"

let no_key_help =
  "no GPG signing key found in your keyring.\n\
  \  Generate one (Ed25519 recommended) with:\n\
  \    gpg --quick-generate-key 'Your Name <you@example.com>' ed25519 default 1y\n\
  \  Then re-run; --gpg-key picks it up automatically."

(* Resolve [--gpg-key]: use what the user passed (verbatim), otherwise
   auto-pick from the host keyring (Ed25519 preferred) and log it.
   Aborts with generation instructions when neither path yields a
   usable signing key. *)
let resolve_gpg_key ~sys cli_key =
  match cli_key with
  | Some k when k <> "" -> k
  | _ -> (
      match pick_signing_key (list_signing_keys ~sys) with
      | None -> Oi.Error.fail_config_error "oi dist repo: %s" no_key_help
      | Some k ->
          Oi.Say.info "auto-selected gpg key %s (%s, %s)" k.fingerprint
            (key_kind k) k.uid;
          k.fingerprint)

(* Force the host gpg-agent to cache the unlocked signing key NOW, by
   running a throwaway [gpg --sign /dev/null] on the host with
   [GPG_TTY] pointing at the controlling terminal (resolved via the
   POSIX [tty] command so the path is correct on Linux and macOS
   alike; we deliberately don't read [/proc/self/fd/0] since /proc is
   Linux-only). After this, any [reprepro] / [gpg] call inside the
   signer container reaches the host agent through the bind-mounted
   socket and hits the cached key — no pinentry, no "Inappropriate
   ioctl for device".

   [Sys.command] (rather than [D10.Sysops.Cmd]) so the shell inherits
   our stdin/stdout/stderr: [tty(1)] reports the device of its stdin,
   and pinentry-curses takes over the terminal while it prompts. macOS
   users with [pinentry-mac] get a native dialog instead and don't
   actually need GPG_TTY, but setting it does no harm there.

   Best-effort: skipped silently when not on a TTY (CI scenarios with
   passphraseless / pre-cached keys); on failure we surface a warning
   with the manual recovery command rather than aborting — the actual
   sign might still succeed via a graphical pinentry. *)
let prewarm_gpg_agent ~key =
  if not (Unix.isatty Unix.stdin) then ()
  else begin
    Oi.Say.step "pre-unlocking gpg key %s" key;
    let cmd =
      Fmt.str
        "GPG_TTY=$(tty 2>/dev/null) gpg --yes --local-user %s --output \
         /dev/null --sign /dev/null > /dev/null"
        (Filename.quote key)
    in
    let rc = Sys.command cmd in
    if rc <> 0 then
      Oi.Say.warn
        "pre-unlock failed (rc=%d); container-side signing may still trip on \
         pinentry. To recover manually run:@\n\
        \  GPG_TTY=$(tty) gpg --local-user %s --sign /dev/null > /dev/null"
        rc key
  end

(* Build the signer image once. [docker build] is content-addressed, so a
   second call with the same Dockerfile is essentially free; we still rerun
   it so a Debian point release flows in automatically. *)
(* Best-effort cleanup of a throwaway directory: remove every direct
   entry then rmdir. Files we didn't write (or that another process
   already removed) just get skipped — we tolerate [Sys_error] from
   [Sys.remove] / [Sys.readdir] and [Unix.Unix_error] from [rmdir]. *)
let cleanup_temp_dir d =
  try
    Sys.readdir d
    |> Array.iter (fun n -> try Sys.remove (d / n) with Sys_error _ -> ());
    try Unix.rmdir d with Unix.Unix_error _ -> ()
  with Sys_error _ -> ()

let ensure_signer_image ~sys =
  let tmp = Filename.temp_dir "osdist-signer-ctx-" "" in
  Fun.protect
    ~finally:(fun () -> cleanup_temp_dir tmp)
    (fun () ->
      let df = tmp / "Dockerfile" in
      write_file ~path:df signer_dockerfile ~mode:0o644;
      cmd_run ~sys
        [ "docker"; "build"; "-q"; "-t"; signer_image; "-f"; df; tmp ])

(* [docker run] argv prefix for an invocation of [argv] inside the signer
   image. [workdirs] is the set of host paths the command needs read-write
   access to (each appears at the same path inside the container, so any
   absolute path in [argv] resolves identically on both sides). *)
let in_signer ~sx ~workdirs argv =
  let mount p = [ "-v"; p ^ ":" ^ p ] in
  let workdir_mounts = List.concat_map mount workdirs in
  let agent_mount =
    match sx.xdg_runtime with Some d -> mount d | None -> []
  in
  let agent_env =
    match sx.xdg_runtime with
    | Some d -> [ "-e"; "XDG_RUNTIME_DIR=" ^ d ]
    | None -> []
  in
  [
    "docker";
    "run";
    "--rm";
    "--network=none";
    "-u";
    Fmt.str "%d:%d" sx.uid sx.gid;
    "-e";
    "HOME=" ^ sx.home;
    "-e";
    "GNUPGHOME=" ^ sx.home ^ "/.gnupg";
  ]
  @ agent_env @ mount sx.home @ workdir_mounts @ agent_mount @ [ signer_image ]
  @ argv

let gpg_export_armored ~sys ~sx ~key =
  cmd_out ~sys
    (in_signer ~sx ~workdirs:[] [ "gpg"; "--armor"; "--export"; key ])

let gpg_clearsign ~sys ~sx ~workdir ~key ~input ~output =
  cmd_run ~sys
    (in_signer ~sx ~workdirs:[ workdir ]
       [
         "gpg"; "--local-user"; key; "--yes"; "--clearsign"; "-o"; output; input;
       ])

let gpg_detach_sig ~sys ~sx ~workdir ~key ~input ~output =
  cmd_run ~sys
    (in_signer ~sx ~workdirs:[ workdir ]
       [ "gpg"; "--local-user"; key; "--yes"; "-abs"; "-o"; output; input ])

let gpg_detach_armor ~sys ~sx ~workdir ~key ~input ~output =
  cmd_run ~sys
    (in_signer ~sx ~workdirs:[ workdir ]
       [
         "gpg";
         "--local-user";
         key;
         "--yes";
         "--detach-sign";
         "--armor";
         "-o";
         output;
         input;
       ])

let rpmsign_addsign ~sys ~sx ~workdir ~key files =
  match files with
  | [] -> ()
  | _ ->
      cmd_run ~sys
        (in_signer ~sx ~workdirs:[ workdir ]
           ([ "rpmsign"; "--define"; Fmt.str "_gpg_name %s" key; "--addsign" ]
           @ files))

(* Stage every .deb into a temp dir, then run [reprepro includedeb] per
   file inside the signer image. Sequential by design: reprepro takes a
   lock on [apt_root] so concurrent invocations would serialise anyway. *)
let reprepro_includedeb_all ~sys ~sx ~apt_root ~to_publish =
  let stage = Filename.temp_dir "osdist-apt-stage-" "" in
  Fun.protect
    ~finally:(fun () -> cleanup_temp_dir stage)
    (fun () ->
      List.iter
        (fun (_cn, src) -> copy_file ~src ~dst:(stage / Filename.basename src))
        to_publish;
      List.iter
        (fun (cn, src) ->
          cmd_run ~sys
            (in_signer ~sx ~workdirs:[ apt_root; stage ]
               [
                 "reprepro";
                 "-S";
                 "devel";
                 "-P";
                 "optional";
                 "-b";
                 apt_root;
                 "includedeb";
                 cn;
                 stage / Filename.basename src;
               ]))
        to_publish)

(* Clearsign + detach-sign every [dists/<codename>/Release] in [apt_root]
   inside the signer container (gpg reaches the host's keyring + agent
   via the bind-mounts set up in [in_signer]). *)
let sign_apt_releases ~sys ~sx ~apt_root ~key =
  let dists = apt_root / "dists" in
  if Sys.file_exists dists then
    Sys.readdir dists
    |> Array.iter (fun cn ->
        let rel = dists / cn / "Release" in
        if Sys.file_exists rel then begin
          gpg_clearsign ~sys ~sx ~workdir:apt_root ~key ~input:rel
            ~output:(dists / cn / "InRelease");
          gpg_detach_sig ~sys ~sx ~workdir:apt_root ~key ~input:rel
            ~output:(rel ^ ".gpg")
        end)

(* -- APT: reprepro inside the signer image ------------------------------
   reprepro is incremental by default: [includedeb] adds packages to the
   pool and rewrites dists/<codename>/Release. Existing versions in the
   pool are preserved. The signing of [Release] happens in the same image
   immediately after metadata generation, sharing the bind-mounted
   workdir + ~/.gnupg + gpg-agent socket. *)
let apt_assemble ~sys ~sx ~into ~pkg_dir ~spec ~targets ~cfg ~pubkey =
  let deb_targets =
    List.filter
      (fun (t : Osdist.Target.t) -> t.family = Osdist.Target.Deb)
      targets
  in
  let to_publish =
    List.concat_map
      (fun (t : Osdist.Target.t) ->
        let codename = Option.get t.codename in
        let art = pkg_dir / "artefacts" / t.tag in
        files_in ~suffix:".deb" art |> List.map (fun p -> (codename, p)))
      deb_targets
  in
  if to_publish = [] then ()
  else begin
    Oi.Say.step "assembling APT repo";
    let apt_root = into / "apt" in
    mkdir_p (apt_root / "conf");
    write_file
      ~path:(apt_root / "conf" / "distributions")
      (Osdist.Repo_index.apt_distributions cfg ~deb_targets)
      ~mode:0o644;
    reprepro_includedeb_all ~sys ~sx ~apt_root ~to_publish;
    sign_apt_releases ~sys ~sx ~apt_root ~key:cfg.Osdist.Repo_index.gpg_key_id;
    write_file
      ~path:(apt_root / cfg.Osdist.Repo_index.pubkey_filename)
      pubkey ~mode:0o644;
    ignore spec
  end

(* -- RPM: rpmsign + createrepo_c + repomd sig, all in the signer image -- *)
let rpm_assemble ~sys ~sx ~into ~pkg_dir ~spec ~targets ~cfg ~pubkey =
  let rpm_targets =
    List.filter
      (fun (t : Osdist.Target.t) -> t.family = Osdist.Target.Rpm)
      targets
  in
  let key = cfg.Osdist.Repo_index.gpg_key_id in
  List.iter
    (fun (t : Osdist.Target.t) ->
      let art = pkg_dir / "artefacts" / t.tag in
      let rpms = files_in ~suffix:".rpm" art in
      if rpms <> [] then begin
        Oi.Say.step "assembling DNF repo for %s" t.tag;
        let dst = into / "rpm" / t.tag in
        mkdir_p dst;
        let staged_rpms =
          List.map
            (fun src ->
              let d = dst / Filename.basename src in
              copy_file ~src ~dst:d;
              d)
            rpms
        in
        (* Sign every .rpm header BEFORE createrepo_c so the metadata
           checksums cover the signed bytes. Both calls run in the same
           signer image — no per-call image pull. *)
        rpmsign_addsign ~sys ~sx ~workdir:dst ~key staged_rpms;
        cmd_run ~sys
          (in_signer ~sx ~workdirs:[ dst ]
             [ "createrepo_c"; "--update"; "--revision=osdist"; dst ]);
        gpg_detach_armor ~sys ~sx ~workdir:dst ~key
          ~input:(dst / "repodata" / "repomd.xml")
          ~output:(dst / "repodata" / "repomd.xml.asc");
        write_file
          ~path:(dst / cfg.Osdist.Repo_index.pubkey_filename)
          pubkey ~mode:0o644
      end)
    rpm_targets;
  ignore spec

(* Copy one static-binary tarball (and its sha256 sidecar) into [bin],
   and refresh the [<pkg>-latest-linux-<arch>-static.tar.gz] symlink to
   point at it. *)
let publish_static_tarball ~bin ~package ~arch ~src =
  let dst = bin / Filename.basename src in
  copy_file ~src ~dst;
  let sha_src = src ^ ".sha256" in
  if Sys.file_exists sha_src then copy_file ~src:sha_src ~dst:(dst ^ ".sha256");
  let latest = Fmt.str "%s-latest-linux-%s-static.tar.gz" package arch in
  let link = bin / latest in
  (try Unix.unlink link with Unix.Unix_error _ -> ());
  Unix.symlink (Filename.basename src) link

(* -- Static binaries: copy tarballs + sha256 + latest symlink ----------- *)
let static_assemble ~into ~pkg_dir ~(spec : Osdist.Spec.t) ~targets =
  let static_targets =
    List.filter
      (fun (t : Osdist.Target.t) -> t.family = Osdist.Target.Static)
      targets
  in
  if static_targets = [] then ()
  else begin
    let bin = into / "bin" in
    mkdir_p bin;
    List.iter
      (fun (t : Osdist.Target.t) ->
        let art = pkg_dir / "artefacts" / t.tag in
        files_in ~suffix:".tar.gz" art
        |> List.iter (fun src ->
            publish_static_tarball ~bin ~package:spec.package ~arch:t.arch ~src))
      static_targets
  end

(* Locate the source bundle [<pkg>-<ver>.tar.gz] whose [.osdist.json]
   sidecar carries the Spec we need. Canonical location is the
   [<pkg_dir>/bundle/] dir [oi dist pkg] emits; fall back to the
   per-target hardlinks (which materialise_target also drops in each
   [<pkg_dir>/<tag>/] so users can build a single target standalone). *)
(* The bundle dir hosts both the source tarball and (for alpine
   targets) per-arch static tarballs named [...-static.tar.gz]. Only
   the former carries the [.osdist.json] sidecar we need, so filter
   the static ones out by suffix. *)
let is_source_bundle path =
  not (Filename.check_suffix (Filename.basename path) "-static.tar.gz")

let locate_sample_bundle ~pkg_dir =
  let bundle_dir = pkg_dir / "bundle" in
  let from_bundle_dir =
    if Sys.file_exists bundle_dir then
      files_in ~suffix:".tar.gz" bundle_dir |> List.filter is_source_bundle
    else []
  in
  match from_bundle_dir with
  | b :: _ -> b
  | [] -> (
      let per_target_candidates =
        Osdist.Target.default_targets
        |> List.map (fun (t : Osdist.Target.t) -> pkg_dir / t.tag)
        |> List.filter Sys.file_exists
      in
      match
        List.concat_map
          (fun d -> files_in ~suffix:".tar.gz" d)
          per_target_candidates
        |> List.filter is_source_bundle
      with
      | b :: _ -> b
      | [] ->
          Oi.Error.fail_config_error
            "oi dist repo: no source bundle under %s/bundle/ or %s/<target>/. \
             Run `oi dist pkg -o %s` first."
            pkg_dir pkg_dir pkg_dir)

(* Accept [--pkg-dir] pointing either at the [oi dist pkg -o] output
   root (the canonical form, containing per-target subdirs + an
   [artefacts/<tag>/] sibling) or at the [artefacts/] subdir directly
   — climb one level up in the latter case so [locate_sample_bundle]
   still finds the source bundle hardlinked into [<pkg_dir>/<tag>/]. *)
let resolve_pkg_dir pkg_dir =
  if not (Sys.file_exists pkg_dir) then
    Oi.Error.fail_config_error
      "oi dist repo: --pkg-dir not found: %s@\n\
       Pass the output directory you gave to `oi dist pkg -o DIR` (the parent \
       of the artefacts/ subdir)."
      pkg_dir
  else if not (Sys.is_directory pkg_dir) then
    Oi.Error.fail_config_error "oi dist repo: --pkg-dir is not a directory: %s"
      pkg_dir
  else if
    Filename.basename pkg_dir = "artefacts"
    && Sys.file_exists (Filename.dirname pkg_dir / "bundle")
  then (
    let climbed = Filename.dirname pkg_dir in
    Oi.Say.info "--pkg-dir points at the artefacts subdir; using %s" climbed;
    climbed)
  else pkg_dir

let run_body ~sys ~baseurl ~gpg_key ~origin ~label ~description ~pubkey_filename
    ~pkg_dir ~into =
  let pkg_dir = resolve_pkg_dir pkg_dir in
  mkdir_p into;
  let any_bundle = locate_sample_bundle ~pkg_dir in
  let sidecar = Osdist.Spec.sidecar_path ~bundle_path:any_bundle in
  let spec =
    match Osdist.Spec.read_sidecar ~path:sidecar with
    | Ok s -> s
    | Error e ->
        Oi.Error.fail_config_error "oi dist repo: cannot read %s: %s" sidecar e
  in
  let cfg : Osdist.Repo_index.config =
    {
      baseurl;
      origin = (if origin = "" then spec.package else origin);
      label = (if label = "" then spec.package else label);
      description =
        (if description = "" then Fmt.str "%s package repository" spec.package
         else description);
      gpg_key_id = gpg_key;
      pubkey_filename =
        (if pubkey_filename = "" then spec.package ^ ".asc" else pubkey_filename);
    }
  in
  Oi.Say.step "preparing osdist-signer image";
  ensure_signer_image ~sys;
  let sx = build_signer_ctx () in
  prewarm_gpg_agent ~key:gpg_key;
  let pubkey = gpg_export_armored ~sys ~sx ~key:gpg_key in
  let targets = Osdist.Target.default_targets in
  apt_assemble ~sys ~sx ~into ~pkg_dir ~spec ~targets ~cfg ~pubkey;
  rpm_assemble ~sys ~sx ~into ~pkg_dir ~spec ~targets ~cfg ~pubkey;
  static_assemble ~into ~pkg_dir ~spec ~targets;
  write_file ~path:(into / cfg.pubkey_filename) pubkey ~mode:0o644;
  write_file ~path:(into / "install.sh")
    (Osdist.Repo_index.install_sh cfg spec ~targets)
    ~mode:0o755;
  write_file ~path:(into / "INSTALL.md")
    (Osdist.Repo_index.install_md cfg spec ~targets)
    ~mode:0o644;
  Oi.Say.ok "repo updated at %s" into

let repo_run (c : Terms.common) baseurl gpg_key origin label description
    pubkey_filename pkg_dir into =
  let pkg_dir = absolutize pkg_dir in
  let into = absolutize into in
  Harness.run @@ fun ~sw env ->
  let harness =
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  let gpg_key = resolve_gpg_key ~sys:harness.Harness.sys gpg_key in
  run_body ~sys:harness.Harness.sys ~baseurl ~gpg_key ~origin ~label
    ~description ~pubkey_filename ~pkg_dir ~into

let repo_man =
  [
    `S Manpage.s_description;
    `P
      "Sign and publish the artefacts under $(i,PKGDIR)$(b,/artefacts/) \
       (produced by $(b,oi dist pkg --build)) into $(i,REPO): an APT pool for \
       .debs, a per-target DNF tree for .rpms, and a $(b,bin/) dir for \
       static-musl tarballs. Idempotent — re-runs merge new versions into the \
       existing tree without dropping older ones.";
    `P
      "$(b,gpg), $(b,rpmsign), $(b,reprepro), and $(b,createrepo_c) all run \
       inside a throwaway $(b,osdist-signer) container that bind-mounts your \
       $(b,~/.gnupg) and gpg-agent socket. Only $(b,docker) and $(b,gpg) need \
       to be on the host.";
    `S "OUTPUT LAYOUT";
    `Pre
      "  REPO/apt/                 reprepro pool + \
       dists/<codename>/Release.gpg + InRelease\n\
      \  REPO/rpm/<tag>/           signed *.rpm + createrepo_c metadata + \
       repomd.xml.asc\n\
      \  REPO/bin/                 static tarballs + *.sha256 + \
       <pkg>-latest-*.tar.gz symlink\n\
      \  REPO/<pkg>.asc            armored gpg pubkey\n\
      \  REPO/install.sh           one-shot client installer (baseurl baked in)\n\
      \  REPO/INSTALL.md           copy-paste apt / dnf / curl snippets";
    `S Manpage.s_examples;
    `Pre
      "  # auto-pick signing key from your keyring:\n\
      \  oi dist repo --pkg-dir ./pkg --into ./repo --baseurl \
       https://example.org/repo\n\
      \  # pin a specific gpg key:\n\
      \  oi dist repo --pkg-dir ./pkg --into ./repo --baseurl \
       https://example.org/repo --gpg-key 0xDEADBEEF";
    `S "SEE ALSO";
    `P "$(b,oi dist pkg)(1)";
  ]

let repo_cmd =
  let pkg_dir =
    (* [string] (not [dir]) so [resolve_pkg_dir] can emit the
       "pass the dir you gave to oi dist pkg -o" hint when the
       path is missing; cmdliner's [dir] validator would short-circuit
       with a generic "no DIR directory" instead. *)
    Arg.(
      required
      & opt (some string) None
      & info ~docv:"DIR"
          ~doc:
            "Output dir from $(b,oi dist pkg -o) (the parent of \
             $(b,artefacts/); passing $(b,artefacts/) directly is also \
             tolerated)."
          [ "pkg-dir" ])
  in
  let into =
    Arg.(
      required
      & opt (some string) None
      & info ~docv:"DIR"
          ~doc:"Target repo directory; existing contents are preserved."
          [ "into" ])
  in
  let gpg_key =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"KEYID"
          ~doc:
            "GPG fingerprint or key id. Default: auto-pick from the host \
             keyring (Ed25519 preferred over RSA). Aborts with generation \
             instructions if no sign-capable secret key is present."
          [ "gpg-key" ])
  in
  let baseurl =
    Arg.(
      required
      & opt (some string) None
      & info ~docv:"URL"
          ~doc:
            "Public base URL the repo will be served from. Baked into apt \
             sources lists, dnf .repo files, and $(b,install.sh)."
          [ "baseurl" ])
  in
  let origin =
    Arg.(
      value & opt string ""
      & info ~docv:"STR" ~doc:"APT Origin: field (default: package name)."
          [ "origin" ])
  in
  let label =
    Arg.(
      value & opt string ""
      & info ~docv:"STR" ~doc:"APT Label: field (default: package name)."
          [ "label" ])
  in
  let description =
    Arg.(
      value & opt string ""
      & info ~docv:"STR"
          ~doc:
            "One-line Release-file description (default: $(b,<pkg> package \
             repository))."
          [ "description" ])
  in
  let pubkey_filename =
    Arg.(
      value & opt string ""
      & info ~docv:"NAME"
          ~doc:
            "Filename of the armored pubkey under $(i,REPO) (default: \
             $(b,<pkg>.asc))."
          [ "pubkey-name" ])
  in
  Cmd.v
    (Cmd.info "repo"
       ~doc:
         "Assemble a signed apt / dnf / static-bin repository from $(b,oi dist \
          pkg) artefacts"
       ~man:repo_man)
    Term.(
      const repo_run $ Terms.common $ baseurl $ gpg_key $ origin $ label
      $ description $ pubkey_filename $ pkg_dir $ into)

let cmd = repo_cmd
