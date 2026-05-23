(* [oi dist pkg]: take a bundle ([oi dist bundle] output) plus signing /
   epoch / distro cmdliner args, and materialise a per-target packaging
   tree. With --build, also drive `docker buildx build` to produce the
   actual .deb / .rpm / static tarball artefacts. *)

open Cmdliner

let ( / ) = Filename.concat

let absolutize p =
  if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p

(* Atomic write of [s] to [path] with [mode]. *)
let write_file ~path s ~mode =
  let dir = Filename.dirname path in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error _ -> ());
  let tmp = Fmt.str "%s.tmp.%d" path (Unix.getpid ()) in
  Out_channel.with_open_bin tmp (fun oc -> Out_channel.output_string oc s);
  Unix.chmod tmp mode;
  Sys.rename tmp path

let rec mkdir_p d =
  if d <> "" && d <> "/" && d <> "." && not (Sys.file_exists d) then begin
    mkdir_p (Filename.dirname d);
    try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let copy_file ~src ~dst =
  let ic = open_in_bin src in
  let oc = open_out_bin dst in
  let buf = Bytes.create 65536 in
  let rec loop () =
    let n = input ic buf 0 (Bytes.length buf) in
    if n > 0 then begin
      output oc buf 0 n;
      loop ()
    end
  in
  Fun.protect ~finally:(fun () -> close_in ic; close_out oc) loop

let render_dockerfile path df =
  write_file ~path (Dockerfile.string_of_t df) ~mode:0o644

(* Current date in the two formats the per-family templates expect:
   - RFC 2822 for debian/changelog
   - "Wed May 21 2026" for rpm %changelog *)
let now_rfc2822 () =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  let day_name = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |] in
  let mon_name =
    [|
      "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct";
      "Nov"; "Dec";
    |]
  in
  Fmt.str "%s, %02d %s %04d %02d:%02d:%02d +0000" day_name.(tm.tm_wday)
    tm.tm_mday mon_name.(tm.tm_mon) (tm.tm_year + 1900) tm.tm_hour tm.tm_min
    tm.tm_sec

let now_rpm () =
  let tm = Unix.gmtime (Unix.gettimeofday ()) in
  let day_name = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |] in
  let mon_name =
    [|
      "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct";
      "Nov"; "Dec";
    |]
  in
  Fmt.str "%s %s %02d %04d" day_name.(tm.tm_wday) mon_name.(tm.tm_mon)
    tm.tm_mday (tm.tm_year + 1900)

(* Pluck the overlay depexts for [t] from the sidecar by Distro.tag.
   Includes alpine-keyed entries for the static target ([t.distro] is
   pinned to [`Alpine `V3_22]). *)
let depexts_for (s : Osdist.Spec.t) (t : Osdist.Target.t) =
  let tag = Dockerfile_opam.Distro.tag_of_distro t.distro in
  Option.value ~default:[] (List.assoc_opt tag s.depexts)

let bundle_basename ~spec =
  Fmt.str "%s-%s.tar.gz" spec.Osdist.Spec.package spec.Osdist.Spec.version

(* Materialise one target's subdir. The bundle tarball is hardlinked (or
   copied as a fallback) into each target dir so [docker buildx build]
   contexts are self-contained — the user can drop into a single subdir
   and build it standalone if they prefer. *)
let materialise_target ~out ~bundle_tarball ~(spec : Osdist.Spec.t)
    ~(t : Osdist.Target.t) ~rfc2822 ~rpm_date =
  let dir = out / t.tag in
  mkdir_p dir;
  let depexts = depexts_for spec t in
  let bundle_in_dir = dir / bundle_basename ~spec in
  (* Hardlink the tarball to keep duplication free — fall back to copy if
     the destination is on a different filesystem (EXDEV) or if any other
     link-time error trips. EEXIST = a prior run already linked it; safe
     to leave in place. ENOENT used to be swallowed here, which silently
     stranded the bundle outside the target dir; now we always fall back
     to [copy_file] (it [mkdir_p]s the parent first). *)
  (try Unix.link bundle_tarball bundle_in_dir
   with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
   | Unix.Unix_error _ -> copy_file ~src:bundle_tarball ~dst:bundle_in_dir);
  match t.family with
  | Osdist.Target.Deb ->
      let df = Osdist.Deb.dockerfile spec t ~overlay_depexts:depexts in
      render_dockerfile (dir / "Dockerfile") df;
      let deb_dir = dir / "debian" in
      mkdir_p deb_dir;
      write_file
        ~path:(deb_dir / "control")
        (Osdist.Deb.control spec t ~overlay_depexts:depexts)
        ~mode:0o644;
      write_file ~path:(deb_dir / "rules") (Osdist.Deb.rules spec t)
        ~mode:0o755;
      write_file
        ~path:(deb_dir / "changelog")
        (Osdist.Deb.changelog spec t ~date_rfc2822:rfc2822)
        ~mode:0o644;
      write_file
        ~path:(deb_dir / "copyright")
        (Osdist.Deb.copyright spec) ~mode:0o644;
      mkdir_p (deb_dir / "source");
      write_file
        ~path:(deb_dir / "source" / "format")
        Osdist.Deb.source_format ~mode:0o644
  | Osdist.Target.Rpm ->
      let df = Osdist.Rpm.dockerfile spec t ~overlay_depexts:depexts in
      render_dockerfile (dir / "Dockerfile") df;
      write_file
        ~path:(dir / Fmt.str "%s.spec" spec.package)
        (Osdist.Rpm.spec spec t ~overlay_depexts:depexts ~date_rpm:rpm_date)
        ~mode:0o644
  | Osdist.Target.Static ->
      let df = Osdist.Alpine_static.dockerfile ~overlay_depexts:depexts spec t in
      render_dockerfile (dir / "Dockerfile") df;
      write_file
        ~path:(dir / "build.sh")
        (Osdist.Alpine_static.build_sh spec t)
        ~mode:0o755

(* Static preamble of the top-level build.sh: defines the [build_one]
   shell function which [docker build]s one target's image then [docker
   run]s it with [./artefacts/<tag>/] bind-mounted to [/artefacts] so
   the runtime [CMD] writes the .deb / .rpm / static tarball directly
   to the host. Accumulates pass/fail state and never aborts the outer
   loop on a single-target failure. *)
let build_sh_preamble =
  {|#!/bin/sh
# Generated by `oi dist pkg`. For each target subdir:
#   1. `docker build` builds an image carrying the bootstrap toolchain
#      + staged sources (no compile yet — that lives in the image's CMD).
#   2. `docker run` with /artefacts bind-mounted to ./artefacts/<tag>/
#      executes the CMD: it compiles and copies the resulting .deb /
#      .rpm / static tarball into /artefacts (and hence the host dir).
#
# Re-runs are idempotent: each target writes <tag>/.built on success
# and subsequent invocations skip done targets unless --rebuild is
# passed. Errors don't stop the loop; a per-target summary is printed
# at the end and the script exits non-zero iff any target failed.
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
ART="$HERE/artefacts"
mkdir -p "$ART"
REBUILD=0
case "${1:-}" in --rebuild) REBUILD=1 ;; esac
PASS=""
FAIL=""
build_one() {
  t="$1"
  stamp="$HERE/$t/.built"
  if [ "$REBUILD" -eq 0 ] && [ -f "$stamp" ]; then
    echo "==> $t already built (--rebuild to force)"
    PASS="$PASS $t"
    return 0
  fi
  echo "==> building $t"
  rm -rf "$ART/$t"; mkdir -p "$ART/$t"; chmod 0777 "$ART/$t"
  img="osdist-build-$t:latest"
  if docker build -t "$img" "$HERE/$t" \
       && docker run --rm -v "$ART/$t:/artefacts" "$img"; then
    : > "$stamp"
    PASS="$PASS $t"
  else
    rc=$?
    echo "==> $t FAILED (exit $rc) — continuing with other targets"
    FAIL="$FAIL $t"
  fi
}
|}

(* Static postamble: per-target summary + non-zero exit on any failure. *)
let build_sh_postamble =
  {|
echo
echo "==> summary"
[ -n "$PASS" ] && echo "  passed:$PASS"
[ -n "$FAIL" ] && echo "  failed:$FAIL"
echo "==> artefacts in $ART"
[ -z "$FAIL" ]
|}

let top_level_build_sh ~targets =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf build_sh_preamble;
  let ppf = Fmt.with_buffer buf in
  List.iter
    (fun (t : Osdist.Target.t) -> Fmt.pf ppf "build_one %s@." t.tag)
    targets;
  Buffer.add_string buf build_sh_postamble;
  Buffer.contents buf

(* compose.yaml — parallel counterpart to build.sh. The intended driver:

     docker compose -f compose.yaml up --build

   builds every image (toolchain + staged sources, no compile) in
   parallel, then runs each container; each container's [CMD] does
   the actual compile and writes the resulting .deb / .rpm / static
   tarball into [/artefacts], which is bind-mounted to
   [./artefacts/<tag>/]. Containers exit cleanly once the build is
   done; compose detaches when all services have stopped. *)

(* Docker image tags accept only [A-Za-z0-9_.-]; the [~] we use for
   Debian pre-release versions (e.g. [0~dev]) is rejected. Coerce
   anything outside that alphabet to [-] for tag-name purposes only —
   the underlying spec version stays [0~dev] inside the .deb/.rpm. *)
let docker_tag_safe v =
  String.map
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '-') as c -> c
      | _ -> '-')
    v

let image_tag ~(spec : Osdist.Spec.t) (t : Osdist.Target.t) =
  Fmt.str "osdist-%s-%s:%s" spec.package t.tag (docker_tag_safe spec.version)

let compose_header =
  {|# Generated by `oi dist pkg`. Build every target in parallel:
#
#   docker compose -f compose.yaml up --build
#
# This builds each target's image (toolchain + staged sources only —
# no compile yet) and then runs each container. Every Dockerfile's
# CMD does the actual compile + copy of the .deb / .rpm / static
# tarball into /artefacts, which is bind-mounted to
# ./artefacts/<tag>/. Once each container's CMD finishes, the
# container exits; compose returns when all containers have stopped.
#
# Compare with ./build.sh (sequential but resilient to per-target failure).

services:
|}

let compose_service_block ~spec (t : Osdist.Target.t) =
  Fmt.str
    {|  %s:
    image: %s
    build:
      context: ./%s
      dockerfile: Dockerfile
    volumes:
      - ./artefacts/%s:/artefacts

|}
    t.tag (image_tag ~spec t) t.tag t.tag

let compose_yaml ~spec ~targets =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf compose_header;
  List.iter
    (fun t -> Buffer.add_string buf (compose_service_block ~spec t))
    targets;
  Buffer.contents buf

let pp_argv ppf argv =
  Fmt.pf ppf "%s" (String.concat " " (List.map Filename.quote argv))

let cmd_run_inherit ~sys argv =
  try D10.Sysops.Cmd.run_inherit sys argv
  with Eio.Io _ as exn ->
    Oi.Error.fail_config_error "oi dist pkg: %a failed: %s" pp_argv argv
      (Printexc.to_string exn)

let pkg_run (c : Terms.common) refresh registry use_registry with_repos
    with_deps _jobs toolchain_override pkg_name version_override epoch
    maintainer homepage license prefix distros_str cli_targets build_flag
    output =
  if output = "" then
    Oi.Error.fail_config_error "oi dist pkg: -o DIR is required";
  let output = absolutize output in
  mkdir_p output;
  Harness.run @@ fun ~sw env ->
  let harness =
    Harness.bootstrap ~sw ~data_dir:c.data_dir ~format:c.format env c.cache_dir
  in
  (* Stage 1: bundle. Produces <output>/bundle/<pkg>-<ver>.tar.gz +
     <pkg>-<ver>.osdist.json (with per-distro depexts inside the spec). *)
  let bundle_path, spec =
    Osdist_bundle.produce ~harness ~refresh ~registry ~use_registry ~with_repos
      ~with_deps ~toolchain_override ~targets:cli_targets ~pkg_name
      ~version:version_override ~epoch ~maintainer ~homepage ~license ~prefix
      ~output
  in
  (* Stage 2: materialise per-distro Dockerfile + family metadata. *)
  let targets =
    match distros_str with
    | None -> Osdist.Target.default_targets
    | Some s -> Osdist.Target.parse_list s
  in
  let rfc2822 = now_rfc2822 () in
  let rpm_date = now_rpm () in
  let artefacts_root = output / "artefacts" in
  mkdir_p artefacts_root;
  List.iter
    (fun t ->
      Oi.Say.step "materialising %s/" t.Osdist.Target.tag;
      materialise_target ~out:output ~bundle_tarball:bundle_path ~spec ~t
        ~rfc2822 ~rpm_date;
      (* Pre-create the per-target artefacts dir as 0777 so compose's
         bind-mount finds a writable host dir regardless of the
         container user (root for deb, [builder] UID 1000 for rpm,
         [opam] UID 1000 for static). *)
      let art_dir = artefacts_root / t.Osdist.Target.tag in
      mkdir_p art_dir;
      (try Unix.chmod art_dir 0o777 with Unix.Unix_error _ -> ()))
    targets;
  let top = output / "build.sh" in
  let compose = output / "compose.yaml" in
  write_file ~path:top (top_level_build_sh ~targets) ~mode:0o755;
  write_file ~path:compose (compose_yaml ~spec ~targets) ~mode:0o644;
  Oi.Say.ok "wrote %s" output;
  Oi.Say.info "  parallel (compose):     docker compose -f %s up --build" compose;
  Oi.Say.info "  sequential (resilient): %s" top;
  (* Stage 3 (optional): drive [docker compose up --build] for parallel
     build + extract. Project-name keyed on the package so multiple
     concurrent [oi dist pkg --build] runs (different packages) don't
     collide on container names. *)
  if build_flag then begin
    let project = Fmt.str "osdist-%s" spec.package in
    let argv =
      [ "docker"; "compose"; "-f"; compose; "-p"; project; "up"; "--build" ]
    in
    Oi.Say.step "invoking %a" pp_argv argv;
    cmd_run_inherit ~sys:harness.Harness.sys argv;
    Oi.Say.ok "all artefacts under %s/artefacts/" output
  end

let pkg_man =
  [
    `S Manpage.s_description;
    `P
      "Lay down a source bundle and per-distro packaging tree (Dockerfiles, \
       $(b,debian/), $(b,.spec)) under $(i,DIR), and optionally drive \
       $(b,docker compose up --build) to produce the actual artefacts.";
    `P
      "$(i,TARGET) names the opam package. With no $(i,TARGET), the cwd \
       project's root opam package is used.";
    `S "OUTPUT LAYOUT";
    `Pre
      "  DIR/bundle/<pkg>-<ver>.tar.gz       source tarball + .sha256 + .osdist.json\n\
      \  DIR/<tag>/                          per-target Dockerfile + debian/ or .spec\n\
      \  DIR/artefacts/<tag>/                .deb / .rpm / static tarball (filled by --build)\n\
      \  DIR/compose.yaml                    docker compose up --build driver\n\
      \  DIR/build.sh                        sequential, per-target-failure-tolerant driver";
    `S Manpage.s_examples;
    `Pre
      "  oi dist pkg -o ./pkg                       # tree only\n\
      \  oi dist pkg -o ./pkg --build               # tree + compose up --build\n\
      \  oi dist pkg utop -o ./utop-pkg --build     # an upstream opam target\n\
      \  oi dist pkg @avsm/tangled -o ./pkg --build # an overlay-handle target\n\
      \  oi dist pkg -o ./pkg --distros ubuntu-26.04,fedora-44 --build";
    `S "SEE ALSO";
    `P "$(b,oi dist repo)(1)";
  ]

let pkg_cmd =
  let output =
    Arg.(
      value & opt string ""
      & info ~docv:"DIR" ~doc:"Output directory (required)." [ "o"; "output" ])
  in
  let cli_targets =
    Arg.(
      value & pos_all string []
      & info ~docv:"TARGET"
          ~doc:
            "Opam package, or $(b,@HANDLE/PKG) for an overlay. Omit to use \
             the cwd project's root opam package."
          [])
  in
  let pkg_name =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"NAME" ~doc:"Override the derived package name."
          [ "pkg-name" ])
  in
  let version =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"VER"
          ~doc:
            "Override the derived version (default: dune-project's \
             $(b,version) or opam $(b,version:))."
          [ "pkg-version" ])
  in
  let epoch =
    Arg.(
      value
      & opt (some int) None
      & info ~docv:"N"
          ~doc:
            "Package epoch (deb $(b,N:) prefix / rpm $(b,Epoch:)). Bump when \
             a new version needs to sort above an earlier release."
          [ "epoch" ])
  in
  let maintainer =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"NAME <EMAIL>"
          ~doc:"Maintainer field (default: $(b,\\$DEBFULLNAME <\\$DEBEMAIL>))."
          [ "maintainer" ])
  in
  let homepage =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"URL" ~doc:"Homepage URL." [ "homepage" ])
  in
  let license =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"SPDX" ~doc:"License (SPDX identifier)." [ "license" ])
  in
  let prefix =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"PATH" ~doc:"Install prefix (default: $(b,/usr))."
          [ "prefix" ])
  in
  let distros =
    Arg.(
      value
      & opt (some string) None
      & info ~docv:"LIST"
          ~doc:
            "Comma-separated target tags (e.g. $(b,ubuntu-26.04,fedora-44)). \
             Default: all five ($(b,ubuntu-24.04), $(b,ubuntu-26.04), \
             $(b,debian-13), $(b,fedora-44), $(b,alpine-static))."
          [ "distros" ])
  in
  let build_flag =
    Arg.(
      value & flag
      & info
          ~doc:
            "Also run $(b,docker compose -f compose.yaml up --build): build \
             every target's image in parallel, then run each container's \
             $(b,CMD) (the actual compile), writing artefacts into \
             $(b,<DIR>/artefacts/<tag>/) via a bind-mounted volume."
          [ "build" ])
  in
  Cmd.v
    (Cmd.info "pkg"
       ~doc:"Emit source bundle + per-distro packaging tree (and optionally \
             build the artefacts)"
       ~man:pkg_man)
    Term.(
      const pkg_run $ Terms.common $ Terms.refresh $ Terms.registry
      $ Terms.use_registry $ Terms.with_repos $ Terms.with_deps $ Terms.jobs
      $ Terms.toolchain $ pkg_name $ version $ epoch $ maintainer $ homepage
      $ license $ prefix $ distros $ cli_targets $ build_flag $ output)

let cmd = pkg_cmd
