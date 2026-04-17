[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]

type entry = { pkg : OpamPackage.t; sys_pkgs : OpamSysPkg.Set.t }

let compute ctx ~packages_dirs solved =
  let env = Opam_ctx.platform_env ctx in
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
