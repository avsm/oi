[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

module DF = Dockerfile
module Distro = Dockerfile_opam.Distro

(* Shadow stdlib [@@] with Dockerfile's concat in this module. *)
let ( @@ ) = Dockerfile.( @@ )

(* -- Per-package-manager install command helpers ------------------------- *)

(* Bootstrap toolchain needed before any opam-driven build can run.
   Library [-dev] depexts and package-specific bits (libssl-dev,
   libsqlite3-dev, libblosc-dev, Apache Arrow, …) are intentionally NOT
   listed here — they belong in the [depexts:] field of the opam package
   that needs them, and flow into the Dockerfile via
   [compute_overlay_depexts]. This list is just "what does the host need
   to invoke a C compiler and fetch sources before opam takes over". *)
let build_depexts = function
  | `Apk ->
      (* coreutils: oxcaml-compiler's Makefile uses [cp -l -R] to set up
         [_build/runtime_stdlib_install]; busybox cp drops the file
         contents on directory hardlinks, leaving Makefile.config missing.
         s3cmd is Alpine's package for the [s3cmd] CLI used by the
         final sync step in [--all] builds. *)
      "build-base m4 perl pkgconf autoconf git curl bash patch tar xz zstd \
       rsync sudo coreutils ca-certificates linux-headers s3cmd"
  | `Apt ->
      "build-essential m4 perl pkg-config autoconf git curl bash patch tar \
       xz-utils zstd rsync sudo ca-certificates s3cmd"
  | `Yum ->
      "gcc gcc-c++ make m4 perl pkgconf-pkg-config autoconf git curl bash \
       patch tar xz zstd rsync sudo ca-certificates findutils which diffutils \
       s3cmd"
  | _ -> failwith "unsupported package manager"

(* -- Distro → opam platform variables ----------------------------------- *)

type opam_vars = {
  os_distribution : string;
  os_family : string;
  os_version : string;
}

(* Parse [tag_of_distro] output (e.g. "alpine-3.23", "ubuntu-25.10") into
   a [distribution-name, version] pair. Distros whose tags contain a
   dash in the distribution segment (e.g. "opensuse-leap-15.6") are
   handled by matching on the variant itself below. *)
let parse_tag tag =
  match String.index_opt tag '-' with
  | Some i ->
      (String.sub tag 0 i, String.sub tag (i + 1) (String.length tag - i - 1))
  | None -> (tag, "")

let opam_vars_of_distro (d : Distro.t) =
  let resolved = Distro.resolve_alias d in
  let tag = Distro.tag_of_distro (resolved :> Distro.t) in
  let name, version = parse_tag tag in
  (* opam's own [os-family] lookup uses /etc/os-release's [ID_LIKE]
     on Linux. The mapping below mirrors what opam would see for each
     canonical distro family. *)
  let os_family =
    match name with
    | "ubuntu" -> "debian"
    | "debian" -> "debian"
    | "centos" | "oraclelinux" -> "rhel"
    | "fedora" -> "fedora"
    | "rhel" -> "rhel"
    | "alpine" -> "alpine"
    | "opensuse" -> "suse"
    | "archlinux" | "arch" -> "arch"
    | other -> other
  in
  let os_distribution =
    match name with "archlinux" -> "arch" | other -> other
  in
  { os_distribution; os_family; os_version = version }

let install_cmd mgr pkgs =
  match mgr with
  | `Apk -> Fmt.str "apk add --no-cache %s" pkgs
  | `Apt ->
      Fmt.str
        "apt-get update -qq && DEBIAN_FRONTEND=noninteractive apt-get install \
         -y --no-install-recommends %s && rm -rf /var/lib/apt/lists/*"
        pkgs
  | `Yum -> Fmt.str "dnf install -y %s && dnf clean all" pkgs
  | _ -> failwith "unsupported package manager"

(* -- Stage 0: static-linked oi binary built on alpine/musl --------------- *)

(* [OI_STATIC=1] is picked up by the root [dune]'s discover rule and
   appended to the release-profile link flags as [-cclib -static].
   Building inside Alpine/musl with that flag yields a fully static
   binary that runs on any Linux distro. *)
let oi_builder_stage ~src_context =
  let apk_build_pkgs =
    "build-base m4 perl pkgconf git curl bash patch gmp-dev sqlite-dev \
     sqlite-static openssl-dev openssl-libs-static zlib-dev zlib-static"
  in
  DF.comment "=== Stage: oi-builder (static musl binary on alpine) ==="
  @@ DF.from ~alias:"oi-builder" ~tag:"alpine-3.22-ocaml-5.4" "ocaml/opam"
  @@ DF.user "root"
  @@ DF.run "apk add --no-cache %s" apk_build_pkgs
  @@ DF.user "opam"
  @@ DF.workdir "/home/opam/src"
  @@ DF.copy ~chown:"opam:opam" ~src:[ src_context ] ~dst:"/home/opam/src" ()
  @@ DF.env
       [
         ("OPAMYES", "true");
         ("OPAMCONFIRMLEVEL", "unsafe-yes");
         ("OI_STATIC", "1");
         ("CI", "true");
       ]
  (* The ocaml/opam image's [default] repo points at a baked-in local
     clone (git+file:///home/opam/opam-repository), so `opam update
     default` alone just re-reads that frozen snapshot. Pull the clone
     from upstream first so recent packages (e.g. dune >= 3.21 required
     by tomlt) are visible to the solver. *)
  @@ DF.run
       "cd ~/opam-repository && git pull --quiet origin master && opam-2.5 \
        update default"
  @@ DF.run "opam-2.5 install ."
  @@ DF.run "opam-2.5 exec -- dune build --profile=release bin/main.exe"
  @@ DF.user "root"
  @@ DF.run
       "mkdir -p /out && cp _build/default/bin/main.exe /out/oi && chmod 755 \
        /out/oi && strip /out/oi"

(* -- Per-distro build stage --------------------------------------------- *)

(* GitHub repo whose releases page hosts the static [oi] binaries.
   Change this if you maintain a fork; every per-distro image pulls
   [oi-linux-<arch>] from [<release_repo>/releases/latest/download/]. *)
let release_repo = "avsm/oi"

(* A short, filesystem-safe stage alias for a resolved distro. *)
let stage_alias (d : Distro.distro) =
  let tag = Distro.tag_of_distro (d :> Distro.t) in
  "build-" ^ tag

(* Each distro stage installs the generic build toolchain and the union
   of overlay depexts, then fetches the latest statically linked
   [oi-linux-<arch>] and [oix-linux-<arch>] from the [oi] GitHub
   releases page rather than building from source. This keeps image
   builds fast and makes the release workflow the single source of
   truth for both binaries. The stage doesn't run the build/export
   itself: the compose file supplies a [command:] that invokes
   [oi build --all --export /out]. *)
let distro_stage ?(overlay_depexts = []) d =
  let resolved = Distro.resolve_alias d in
  let img, tag = Distro.base_distro_tag (resolved :> Distro.t) in
  let mgr = Distro.package_manager (resolved :> Distro.t) in
  let alias = stage_alias resolved in
  let human = Distro.human_readable_string_of_distro (resolved :> Distro.t) in
  let fetch_oi =
    Fmt.str
      "arch=$(uname -m) && curl -fsSL -o /usr/local/bin/oi \
       https://github.com/%s/releases/latest/download/oi-linux-$arch && curl \
       -fsSL -o /usr/local/bin/oix \
       https://github.com/%s/releases/latest/download/oix-linux-$arch && chmod \
       0755 /usr/local/bin/oi /usr/local/bin/oix"
      release_repo release_repo
  in
  let base = build_depexts mgr in
  (* Dedup overlay depexts against the bootstrap toolchain so the
     install command doesn't list e.g. [git] twice. *)
  let base_words =
    String.split_on_char ' ' base |> List.filter (fun s -> s <> "")
  in
  let overlay_extras =
    overlay_depexts
    |> List.filter (fun p -> not (List.mem p base_words))
    |> List.sort_uniq String.compare
  in
  let combined =
    match overlay_extras with
    | [] -> base
    | xs -> base ^ " " ^ String.concat " " xs
  in
  DF.comment "=== Stage: %s ===" human
  @@ DF.from ~alias ~tag img
  @@ DF.env [ ("CI", "true") ]
  @@ DF.run "%s" (install_cmd mgr combined)
  @@ DF.run "%s" fetch_oi @@ DF.workdir "/work"

(* -- Top-level Dockerfiles ---------------------------------------------- *)

let dockerfile_oi ~src_context =
  DF.comment
    "Generated by `oi docker --all` -- static musl build of oi on alpine."
  @@ DF.comment
       "Build: docker buildx build -f Dockerfile.oi --output \
        type=local,dest=./oi-bin ."
  @@ oi_builder_stage ~src_context
  @@ DF.comment "=== Stage: final (just the static binary) ==="
  @@ DF.from "scratch"
  @@ DF.copy ~from:"oi-builder" ~src:[ "/out/oi" ] ~dst:"/oi" ()

(* Project Dockerfile: bake oi + build depexts into a distro stage,
   then [COPY . /src] and run [cmd]. Used by [oi build --docker]
   ([cmd = "oi build"]) and [oi test --docker] ([cmd = "oi test"]). *)
let dockerfile_project ?(overlay_depexts = []) ?(cmd = "oi build")
    ?(generator = "oi build --docker") d =
  let resolved = Distro.resolve_alias d in
  let tag = Distro.tag_of_distro (resolved :> Distro.t) in
  DF.comment "Generated by `%s` -- %s project image (cmd: %s)." generator tag
    cmd
  @@ DF.comment "Build: docker build -t my-project ."
  @@ distro_stage ~overlay_depexts d
  @@ DF.workdir "/src"
  @@ DF.copy ~src:[ "." ] ~dst:"/src" ()
  @@ DF.run "%s" cmd

(* Build-time secret IDs that consumers must supply (via BuildKit's
   [docker build --secret] or compose's [secrets:] block; obuilder uses
   the same IDs in its [(secrets …)] block). *)
let s3_access_key_secret = "s3-access-key"
let s3_secret_key_secret = "s3-secret-key"

(* Static s3cmd configuration baked into the generated [/tmp/oi-s3cfg]
   file at build time. Only [access_key] / [secret_key] live in secrets;
   the endpoint / bucket-shape fields below are visible in the spec and
   in the image's history (in the printf format string that builds the
   config). Defaults mirror the oi maintainers' own [~/.s3cfg] (Cloudflare
   R2 in front of [s3://oiu/]); override via [oi docker]'s [--s3-*]
   flags. *)
type s3_config = {
  bucket : string;
  host_base : string;
  host_bucket : string;
  bucket_location : string;
  enable_multipart : bool;
}

let default_s3_config =
  {
    bucket = "s3://oiu/";
    host_base = "94ecc99023b2863bae01c23c6757341f.r2.cloudflarestorage.com";
    host_bucket = "94ecc99023b2863bae01c23c6757341f.r2.cloudflarestorage.com";
    bucket_location = "auto";
    enable_multipart = false;
  }

(* Reject characters that would break out of the shell single-quoted
   printf format we embed these values into. Single quote terminates
   the surrounding [' … '] block; newlines / carriage returns would
   inject extra config lines. None of these belong in a legitimate
   bucket / host / location string anyway, so refuse them outright
   rather than try to escape. *)
let validate_s3_field ~field s =
  String.iter
    (function
      | '\'' | '\n' | '\r' ->
          Oi.Error.fail_config_error
            "oi docker: %s value %S may not contain quotes or newlines" field s
      | _ -> ())
    s

(* The single shell command that drives [oi docker --all]'s real work
   inside one distro container: synthesise an [s3cmd] config from
   mounted secrets, run the full registry build into [/registry], and
   push the tree to S3. Used identically by the Dockerfile output
   (mounted via BuildKit secrets) and the obuilder spec (mounted via
   [(secrets …)]).

   Secrets are read fresh from their [target=…] mount path and never
   land on the image filesystem: the config is written under [/tmp],
   the build + sync both reference it via [-c PATH], and the final
   step is an unconditional [rm -f] of that path. The cleanup runs
   even when the build or sync fails — we collect each step's exit
   code into [RC] and only [exit $RC] after the [rm], so a failed
   sync still purges the config from this layer.

   The config goes to a fixed [/tmp] path (not [$HOME/.s3cfg])
   because obuilder typically runs the build as a non-root user
   (e.g. [opam]) — relying on HOME would write under one user and
   have s3cmd look under another. [-c PATH] is HOME-agnostic.

   Setup uses [set -e]: if either secret file is missing or empty
   (typical cause: forgot to pass [--secret s3-access-key=<file>]
   to [obuilder build] / [docker build]), bail out immediately with
   a clear message instead of writing a config with empty keys and
   letting s3cmd fail later with the opaque [Configuration file not
   available] error. [set -e] is disabled before [oi build] / [s3cmd]
   so we can capture their exit codes and still run [rm]. *)
let build_sync_shell ?(s3 = default_s3_config) () =
  validate_s3_field ~field:"--s3-bucket" s3.bucket;
  validate_s3_field ~field:"--s3-host-base" s3.host_base;
  validate_s3_field ~field:"--s3-host-bucket" s3.host_bucket;
  validate_s3_field ~field:"--s3-bucket-location" s3.bucket_location;
  let enable_multipart = if s3.enable_multipart then "True" else "False" in
  (* Upload-only: [s3cmd sync] is bidirectional once the destination
     already has some objects — newer mtimes on the remote pull files
     *down* into the container's [/registry], which we don't want. The
     container's filesystem is the authoritative producer for this build,
     so use [put --recursive] + [--skip-existing] to write new objects
     only and never read existing ones back. *)
  Fmt.str
    "set -eu; S3CFG=/tmp/oi-s3cfg; ACCESS=$(cat /run/secrets/%s); SECRET=$(cat \
     /run/secrets/%s); [ -n \"$ACCESS\" ] && [ -n \"$SECRET\" ] || { echo 'oi \
     docker: %s and %s secrets must be supplied and non-empty (pass via \
     --secret to obuilder build / docker build)' >&2; exit 78; }; printf \
     '[default]\\naccess_key = %%s\\nsecret_key = %%s\\nbucket_location = \
     %s\\nhost_base = %s\\nhost_bucket = %s\\nenable_multipart = %s\\n' \
     \"$ACCESS\" \"$SECRET\" > \"$S3CFG\"; chmod 600 \"$S3CFG\"; set +e; \
     OI_BUILD_PARALLELISM=$(nproc) oi build --refresh --all \
     --use-registry=archives --export /registry; RC=$?; [ $RC -eq 0 ] && { \
     s3cmd -c \"$S3CFG\" put --recursive --skip-existing /registry/ %s; \
     RC=$?; }; rm -f \"$S3CFG\"; exit $RC"
    s3_access_key_secret s3_secret_key_secret s3_access_key_secret
    s3_secret_key_secret s3.bucket_location s3.host_base s3.host_bucket
    enable_multipart s3.bucket

(* Single-distro Dockerfile: distro stage with oi + s3cmd, then one
   final RUN that uses BuildKit secret + cache mounts to bake the
   registry tree into [/registry] and upload it to S3 with
   [s3cmd put --recursive --skip-existing]. The secrets are only
   readable inside this RUN — they never persist in the image. *)
let dockerfile_one_distro ?(s3 = default_s3_config) ?(overlay_depexts = []) d =
  let resolved = Distro.resolve_alias d in
  let tag = Distro.tag_of_distro (resolved :> Distro.t) in
  let cmd = build_sync_shell ~s3 () in
  DF.comment "Generated by `oi docker --all` -- %s registry build image." tag
  @@ DF.comment
       "Usage: S3_ACCESS_KEY=… S3_SECRET_KEY=… docker compose build (BuildKit \
        secrets carry the keys; the build runs oi build --all \
        --export=/registry and then s3cmd put --recursive --skip-existing to \
        %s)."
       s3.bucket
  @@ distro_stage ~overlay_depexts d
  @@ DF.run
       ~mounts:
         [
           DF.mount_cache ~id:"oi-d10ir-archives"
             ~target:"/cache/d10ir/archives" ();
           DF.mount_cache ~id:"oi-layers" ~target:"/cache/layers" ();
           DF.mount_secret ~id:s3_access_key_secret
             ~target:("/run/secrets/" ^ s3_access_key_secret)
             ();
           DF.mount_secret ~id:s3_secret_key_secret
             ~target:("/run/secrets/" ^ s3_secret_key_secret)
             ();
         ]
       ~network:`Host "%s" cmd

(* Filename (without directory) for the per-distro Dockerfile. *)
let one_distro_filename d =
  let resolved = Distro.resolve_alias d in
  "Dockerfile." ^ Distro.tag_of_distro (resolved :> Distro.t)

(* Short name used for the docker-compose service and for the image tag. *)
let service_name d =
  let resolved = Distro.resolve_alias d in
  Distro.tag_of_distro (resolved :> Distro.t)

(* -- docker-compose.yml emitter ----------------------------------------- *)

(* Compose v2 is the current syntax; the top-level [version] key is obsolete
   but we keep a comment so old tooling doesn't choke. *)

(* docker-compose.yml that drives [oi docker --all]. Each per-distro
   service's build does the entire flow inline — oi build, then s3cmd
   sync — using BuildKit secret mounts supplied via the top-level
   [secrets:] block (which reads the S3 keys from the host env at
   compose-build time, so nothing is baked into the image).

   [OI_BUILD_PARALLELISM=$(nproc)] (set inside the build RUN) overrides
   the [min cpu_count 8] cap that {!D10ir.Config.default} applies for
   macOS fd-limit safety. Inside a Linux container with a high [nofile]
   ulimit (set on the service below) we want to use every core on the
   host. *)
let docker_compose_yaml ?(s3 = default_s3_config) ~distros () =
  let buf = Buffer.create 1024 in
  let out s = Buffer.add_string buf s in
  out "# Generated by `oi docker --all`.\n";
  out "# Run with: S3_ACCESS_KEY=… S3_SECRET_KEY=… docker compose build\n";
  out "#\n";
  Fmt.kstr out
    "# Each per-distro service's image build runs `oi build --refresh --all \
     --export /registry` to populate a per-os_key registry tree, then `s3cmd \
     sync --skip-existing /registry/ %s` to publish it.\n"
    s3.bucket;
  out
    "# Secrets ride the build via BuildKit `--mount=type=secret` and are\n\
     # only readable inside the sync RUN — they never persist in the image.\n\
     #\n\
     # Override the reporepo URL via `OI_REPOREPO_URL` in the service\n\
     # environment if you need something other than oi's built-in default.\n";
  out "services:\n";
  List.iter
    (fun d ->
      let name = service_name d in
      let file = one_distro_filename d in
      Fmt.kstr out "  %s:\n" name;
      Fmt.kstr out "    build:\n";
      Fmt.kstr out "      context: .\n";
      Fmt.kstr out "      dockerfile: %s\n" file;
      Fmt.kstr out "      secrets:\n";
      Fmt.kstr out "        - %s\n" s3_access_key_secret;
      Fmt.kstr out "        - %s\n" s3_secret_key_secret;
      Fmt.kstr out "    image: oi-registry-%s\n" name;
      (* Raise the per-container fd limit so high parallelism doesn't
         hit the kernel's default 1024 nofile cap. Each in-flight oi
         build holds capture pipes, compiler subprocess pipes, and
         transient fetch / patch handles — at [nproc] concurrent
         builds on a many-core host that easily climbs into the
         thousands. 65536 leaves plenty of headroom. *)
      Fmt.kstr out "    ulimits:\n";
      Fmt.kstr out "      nofile:\n";
      Fmt.kstr out "        soft: 65536\n";
      Fmt.kstr out "        hard: 65536\n")
    distros;
  out "secrets:\n";
  Fmt.kstr out "  %s:\n    environment: S3_ACCESS_KEY\n" s3_access_key_secret;
  Fmt.kstr out "  %s:\n    environment: S3_SECRET_KEY\n" s3_secret_key_secret;
  Buffer.contents buf

(* Escape a shell payload for embedding inside a [(shell ...)] sexp
   string. Sexp only needs backslash and double-quote escapes;
   everything else (including literal backslash-n) is taken verbatim
   — and we want [\n] verbatim so it survives into the shell
   command. *)
let sexp_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

(* -- Obuilder spec emitter (parallel to the per-distro Dockerfile) ------ *)

(* Per-distro obuilder spec: same multi-stage shape — install depexts,
   fetch the static [oi] binary, then a single [(run …)] that mounts
   the S3 secrets and the d10ir caches, writes the s3cmd config to
   [/tmp/oi-s3cfg], runs the registry build, and pushes the tree to S3
   via [s3cmd -c /tmp/oi-s3cfg].

   [(secrets …)] mounts the two named keys at [/run/secrets/<id>]; the
   build shell reads them via [cat] (matching the Dockerfile path) and
   the keys are only available during this RUN. *)
let obuilder_spec_one_distro ?(s3 = default_s3_config) ?(overlay_depexts = []) d
    =
  let resolved = Distro.resolve_alias d in
  let img, tag = Distro.base_distro_tag (resolved :> Distro.t) in
  let distro_label = Distro.tag_of_distro (resolved :> Distro.t) in
  let mgr = Distro.package_manager (resolved :> Distro.t) in
  let mgr =
    match mgr with
    | (`Apk | `Apt | `Yum) as m -> m
    | _ ->
        Oi.Error.fail_config_error
          "oi docker --obuilder: distro %s uses an unsupported package manager"
          distro_label
  in
  let base = build_depexts mgr in
  let base_words =
    String.split_on_char ' ' base |> List.filter (fun s -> s <> "")
  in
  let overlay_extras =
    overlay_depexts
    |> List.filter (fun p -> not (List.mem p base_words))
    |> List.sort_uniq String.compare
  in
  let combined =
    match overlay_extras with
    | [] -> base
    | xs -> base ^ " " ^ String.concat " " xs
  in
  let install = install_cmd mgr combined in
  let fetch_oi =
    Fmt.str
      "arch=$(uname -m) && curl -fsSL -o /usr/local/bin/oi \
       https://github.com/%s/releases/latest/download/oi-linux-$arch && curl \
       -fsSL -o /usr/local/bin/oix \
       https://github.com/%s/releases/latest/download/oix-linux-$arch && chmod \
       0755 /usr/local/bin/oi /usr/local/bin/oix"
      release_repo release_repo
  in
  (* Obuilder defaults the [(run (shell ...))] interpreter to
     [/bin/bash -c]. Alpine doesn't ship bash, so pin the spec's
     shell to [/bin/sh -c] before any run op. apt/dnf distros already
     have bash so we leave them on the default. *)
  let shell_op = match mgr with `Apk -> " (shell /bin/sh -c)\n" | _ -> "" in
  Fmt.str
    {|; Generated by `oi docker --all` -- %s registry build (obuilder spec).
;
; Usage: obuilder build --secret %s=<file> --secret %s=<file> \
;          -f %s.spec --store=zfs:tank .
;
; Runs `oi build --refresh --all --export /registry`, then
; `s3cmd put --recursive --skip-existing /registry/ %s` using the mounted secrets.

((from "%s:%s")
 (env CI "true")
 (env OI_CACHE_DIR "/cache")
 (env OI_DATA_DIR "/state")
%s (run (network host) (shell "%s"))
 (run (network host) (shell "%s"))
 (run (cache (oi-d10ir-archives (target /cache/d10ir/archives))
             (oi-layers (target /cache/layers)))
      (secrets (%s (target /run/secrets/%s))
               (%s (target /run/secrets/%s)))
      (network host)
      (shell "%s")))
|}
    distro_label s3_access_key_secret s3_secret_key_secret distro_label
    s3.bucket img tag shell_op (sexp_escape install) (sexp_escape fetch_oi)
    s3_access_key_secret s3_access_key_secret s3_secret_key_secret
    s3_secret_key_secret
    (sexp_escape (build_sync_shell ~s3 ()))

(* Filename (without directory) for the per-distro obuilder spec. *)
let one_distro_spec_filename d =
  let resolved = Distro.resolve_alias d in
  Distro.tag_of_distro (resolved :> Distro.t) ^ ".spec"

(* -- Writers ------------------------------------------------------------ *)

let write_dockerfile path df =
  let oc = open_out path in
  let buf = Buffer.create 4096 in
  let fmt = Format.formatter_of_buffer buf in
  DF.pp fmt df;
  Format.pp_print_flush fmt ();
  output_string oc (Buffer.contents buf);
  close_out oc

let write_file path contents =
  let oc = open_out path in
  output_string oc contents;
  close_out oc
