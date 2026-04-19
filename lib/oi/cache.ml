[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

let ( / ) = Filename.concat

type t = { fs : Eio.Fs.dir_ty Eio.Path.t; root : string }

let create ~root fs =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / root);
  { fs; root }

let root t = Eio.Path.(t.fs / t.root)
let root_s t = t.root
let dune_root t = t.root / "dune"
let fs t = t.fs

(* -- Script run cache ---------------------------------------------------- *)

let run_dir t ~hash =
  let dir = t.root / "runs" / hash in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(t.fs / dir);
  Eio.Path.(t.fs / dir)

(* -- Pin-depends cache --------------------------------------------------- *)

let pins_dir t = t.root / "pins"

(* -- Cleanup ------------------------------------------------------------- *)

type item = {
  label : string;
  path : Eio.Fs.dir_ty Eio.Path.t;
  description : string;
}

let cleanable_items t ~data_dir =
  let p sub = Eio.Path.(t.fs / t.root / sub) in
  let d sub = Eio.Path.(t.fs / data_dir / sub) in
  [
    {
      label = "sources";
      path = p "sources";
      description = "Downloaded source tarballs";
    };
    {
      label = "layers";
      path = p "layers";
      description = "Binary layer cache (day10 format)";
    };
    { label = "runs"; path = p "runs"; description = "Cached script builds" };
    {
      label = "prefixes";
      path = p "prefixes";
      description = "Assembled prefix cache (hardlinks)";
    };
    { label = "dune"; path = p "dune"; description = "Dune shared build cache" };
    { label = "repos"; path = d "repos"; description = "Cloned repositories" };
    {
      label = "pins";
      path = p "pins";
      description = "Pin-depends sources and synthesized packages trees";
    };
  ]

let size ~sys path =
  let path_s = Eio.Path.native_exn path in
  try
    let s = String.trim (D10.Sysops.Cmd.run_out sys [ "du"; "-sk"; path_s ]) in
    let kb =
      Int64.of_string_opt (List.hd (String.split_on_char '\t' s))
      |> Stdlib.Option.value ~default:0L
    in
    Int64.mul kb 1024L
  with _ -> 0L

let pp_size fmt sz =
  if sz > 1_000_000_000L then Fmt.pf fmt "%.1fGB" (Int64.to_float sz /. 1e9)
  else if sz > 1_000_000L then Fmt.pf fmt "%.1fMB" (Int64.to_float sz /. 1e6)
  else if sz > 1_000L then Fmt.pf fmt "%.1fKB" (Int64.to_float sz /. 1e3)
  else Fmt.pf fmt "%LdB" sz
