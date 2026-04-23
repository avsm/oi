[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

type entry = { pkg : OpamPackage.t; sys_pkgs : OpamSysPkg.Set.t }

let compute_from_env ~env ~packages_dirs solved =
  List.filter_map
    (fun pkg ->
      match Solve.load_opam packages_dirs pkg with
      | None -> None
      | Some opam ->
          let depexts = OpamFile.OPAM.depexts opam in
          let active =
            List.fold_left
              (fun acc (pkgs, filter) ->
                if OpamFilter.eval_to_bool ~default:false env filter then
                  OpamSysPkg.Set.union acc pkgs
                else acc)
              OpamSysPkg.Set.empty depexts
          in
          if OpamSysPkg.Set.is_empty active then None
          else Some { pkg; sys_pkgs = active })
    solved

let compute ctx ~packages_dirs solved =
  compute_from_env ~env:(Opam_ctx.platform_env ctx) ~packages_dirs solved

let compute_for_conf ~conf ~packages_dirs solved =
  compute_from_env ~env:(Solve.filter_env conf) ~packages_dirs solved

type status = {
  installed : OpamSysPkg.Set.t;
  missing : OpamSysPkg.Set.t;
  not_found : OpamSysPkg.Set.t;
}

let status pkgs =
  if OpamSysPkg.Set.is_empty pkgs then
    {
      installed = OpamSysPkg.Set.empty;
      missing = OpamSysPkg.Set.empty;
      not_found = OpamSysPkg.Set.empty;
    }
  else
    (* [packages_status] needs an [OpamFile.Config.t]. An empty config
       is enough for the query: opam uses it only to pick up a few
       flags (Windows Cygwin/MSYS2, yum-cron hints). Any caller that
       has already initialised an [Opam_ctx.t] has run [init_opam]
       which sets the root dir; we don't need that here. *)
    let config = OpamFile.Config.empty in
    match
      try
        Some (OpamSysInteract.packages_status config pkgs)
      with _ -> None
    with
    | None ->
        (* Package manager unreachable (Windows without Cygwin,
           unsupported distro). Everything counts as missing so the
           user still sees a useful list. *)
        {
          installed = OpamSysPkg.Set.empty;
          missing = pkgs;
          not_found = OpamSysPkg.Set.empty;
        }
    | Some { OpamSysPkg.s_available; s_not_found } ->
        let installed =
          OpamSysPkg.Set.diff pkgs
            (OpamSysPkg.Set.union s_available s_not_found)
        in
        { installed; missing = s_available; not_found = s_not_found }
