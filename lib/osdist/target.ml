type family =
  | Deb
  | Rpm
  | Static

type t = {
  tag : string;
  family : family;
  distro : Dockerfile_opam.Distro.t option;
  base_image : string;
  codename : string option;
  debrev : string option;
  rpmrel : string option;
  arch : string;
}

let string_of_family = function
  | Deb -> "deb"
  | Rpm -> "rpm"
  | Static -> "static"

let ubuntu_24_04 =
  {
    tag = "ubuntu-24.04";
    family = Deb;
    distro = Some (`Ubuntu `V24_04);
    base_image = "ubuntu:24.04";
    codename = Some "noble";
    debrev = Some "1~noble1";
    rpmrel = None;
    arch = "x86_64";
  }

let ubuntu_26_04 =
  {
    tag = "ubuntu-26.04";
    family = Deb;
    distro = Some (`Ubuntu `V26_04);
    base_image = "ubuntu:26.04";
    codename = Some "resolute";
    debrev = Some "1~resolute1";
    rpmrel = None;
    arch = "x86_64";
  }

let debian_13 =
  {
    tag = "debian-13";
    family = Deb;
    distro = Some (`Debian `V13);
    base_image = "debian:13";
    codename = Some "trixie";
    debrev = Some "1~deb13";
    rpmrel = None;
    arch = "x86_64";
  }

let fedora_44 =
  {
    tag = "fedora-44";
    family = Rpm;
    distro = Some (`Fedora `V44);
    base_image = "fedora:44";
    codename = None;
    debrev = None;
    rpmrel = Some "1";
    arch = "x86_64";
  }

let alpine_static =
  {
    tag = "alpine-static";
    family = Static;
    distro = None;
    (* Matches the [Registry_docker.oi_builder_stage] base — keep them in
       lock-step or the static binaries drift across the two entry points. *)
    base_image = "ocaml/opam:alpine-3.22-ocaml-5.4";
    codename = None;
    debrev = None;
    rpmrel = None;
    arch = "x86_64";
  }

(* Order matters for the generated [build.sh]: glibc distros first
   (highest likelihood of producing artefacts), then [alpine-static]
   last because musl is the strictest target and tends to surface
   upstream porting issues that don't affect glibc builds. *)
let default_targets =
  [ ubuntu_24_04; ubuntu_26_04; debian_13; fedora_44; alpine_static ]

let of_tag s = List.find_opt (fun t -> t.tag = s) default_targets

let parse_list s =
  let toks =
    String.split_on_char ',' s
    |> List.map String.trim
    |> List.filter (fun x -> x <> "")
  in
  let resolved = List.filter_map of_tag toks in
  if resolved = [] then
    Fmt.failwith "osdist: no known targets in %S (known: %s)" s
      (String.concat ", " (List.map (fun t -> t.tag) default_targets));
  resolved

let pp ppf t =
  Fmt.pf ppf "%s [%s, %s]" t.tag (string_of_family t.family) t.base_image
