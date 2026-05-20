let overlay = "x-oi-overlay"
let ref = "x-oi-ref"
let toolchain = "x-oi-toolchain"
let toolchain_name = "x-oi-toolchain-name"
let toolchain_compiler = "x-oi-toolchain-compiler"
let toolchain_roots = "x-oi-toolchain-roots"
let toolchain_tools = "x-oi-toolchain-tools"
let relocatable = "x-oi-relocatable"
let default_toolchain = "x-oi-default-toolchain"
let root_packages = "x-root-packages"
let repos = "x-repos"
let reporepo_hash = "x-reporepo-hash"
let d10_archive = "x-d10-archive"

let read_string_ext key opam =
  let exts = OpamFile.OPAM.extensions opam in
  match OpamStd.String.Map.find_opt key exts with
  | Some v -> (
      match v.OpamParserTypes.FullPos.pelem with
      | OpamParserTypes.FullPos.String s -> Some s
      | _ -> None)
  | None -> None
