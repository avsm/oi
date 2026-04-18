(** Opam repository management.

    Manages opam repository remotes using opam's {!OpamRepository} backend which
    handles git, HTTP ([index.tar.gz]), and other transports. The relocatable
    overlay repo takes priority over the default opam-repository.

    Repository packages are accessed as a list of directories (one per repo)
    rather than merged into a single directory — the solver uses
    {!Opam_ctx.switch_state} with {!Opam_0install.Switch_context} to resolve
    packages across all repos.

    Clones are refreshed lazily: a clone whose directory mtime is older than
    {!refresh_max_age} is re-pulled on next use. Pass [~refresh:true] to force a
    refresh regardless of age. *)

type remote = { name : string; url : string }
type config = { remotes : remote list; default : string }

val config : config
(** Default repo configuration: relocatable overlay (priority) + default. *)

val refresh_max_age : float
(** Seconds. Default [86_400.0] (24h). A repo clone older than this is pulled
    again on next use. *)

val ensure : data_dir:string -> ?refresh:bool -> unit -> unit
(** [ensure ~data_dir ()] clones any missing built-in remotes and pulls any
    whose clone directory is older than {!refresh_max_age}. When
    [~refresh:true], every remote is pulled regardless of age. *)

val repo_dir : data_dir:string -> string -> string
(** [repo_dir ~data_dir name] is the local clone path for repo [name]. *)

val packages_dirs : data_dir:string -> string list
(** [packages_dirs ~data_dir] returns the [packages/] directories for all
    configured repos, in priority order (relocatable first). *)

val ensure_extra :
  data_dir:string -> ?refresh:bool -> Project.extra_repo list -> string list
(** [ensure_extra ~data_dir extras] clones/updates each entry using the same
    age/force semantics as {!ensure}. Each entry is cloned into
    [data_dir/repos/<name>] — two entries with the same name collide by design
    (callers should deduplicate). Returns one [packages/] directory per entry in
    input order. *)

val pp_config : config Fmt.t
