(** Per-target packaging metadata.

    A {!t} pairs a {!Dockerfile_opam.Distro.t} with the bits the upstream distro
    module does not carry (debian revision, rpm Release, codename string, the
    base docker image we want to build in). The {!Static} family is the
    odd-one-out: it produces a plain tarball of binaries built on Alpine with
    [OI_STATIC=1], not a system package. *)

type family =
  | Deb
  | Rpm
  | Static

type t = {
  tag : string;
      (** Short, filesystem-safe identifier, e.g. ["ubuntu-26.04"],
          ["fedora-44"], ["alpine-static"]. Doubles as the [out/<tag>/]
          subdirectory name and the dnf repo path. *)
  family : family;
  distro : Dockerfile_opam.Distro.t option;
      (** [None] for {!Static} (the alpine image is hard-coded). *)
  base_image : string;  (** Docker image used by this target's build stage. *)
  codename : string option;
      (** Debian/Ubuntu series ([noble], [resolute], [trixie]); [None] for
          rpm/static. *)
  debrev : string option;
      (** Debian revision suffix ([1~resolute1]); [None] for non-deb. *)
  rpmrel : string option;  (** rpm [Release:] prefix; [None] for non-rpm. *)
  arch : string;  (** ["x86_64"] currently. *)
}

val ubuntu_24_04 : t
(** [ubuntu_24_04] is the Ubuntu 24.04 (noble) [.deb] target on the
    [ubuntu:24.04] base image. *)

val ubuntu_26_04 : t
(** [ubuntu_26_04] is the Ubuntu 26.04 (resolute) [.deb] target on the
    [ubuntu:26.04] base image. *)

val debian_13 : t
(** [debian_13] is the Debian 13 (trixie) [.deb] target on the
    [debian:13] base image. *)

val fedora_44 : t
(** [fedora_44] is the Fedora 44 [.rpm] target on the [fedora:44] base
    image. *)

val alpine_static : t
(** [alpine_static] is the statically-linked musl binary target, built on
    [ocaml/opam:alpine-3.22-ocaml-5.4] with [OI_STATIC=1] in the env. *)

val default_targets : t list
(** [default_targets] is the default per-target build matrix:
    [ubuntu-24.04, ubuntu-26.04, debian-13, fedora-44, alpine-static] — all
    [x86_64]. Glibc distros come first so the more fragile [alpine-static]
    failure mode doesn't poison their resilient-build summary. *)

val of_tag : string -> t option
(** [of_tag s] looks up a default target by its [tag] field. Case-sensitive. *)

val parse_list : string -> t list
(** [parse_list s] parses a comma-separated tag list (e.g.
    ["ubuntu-26.04,fedora-44"]) into targets, dropping unknown tags. Raises
    [Failure] if every entry is unknown. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] renders [t] as [<tag> [<family>, <base_image>]]. *)

val string_of_family : family -> string
(** [string_of_family f] is a short lowercase tag (["deb"], ["rpm"],
    ["static"]) for [f]. *)
