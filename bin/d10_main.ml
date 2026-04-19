[@@@ai_disclosure "ai-assisted"]
[@@@ai_model "claude-opus-4-6"]
[@@@ai_provider "Anthropic"]

open Cmdliner

let ( / ) = Filename.concat

let xdg_cache_home () =
  match Sys.getenv_opt "XDG_CACHE_HOME" with
  | Some d -> d
  | None -> Sys.getenv "HOME" / ".cache"

let default_cache_dir () = xdg_cache_home () / "oi"

let cache_dir_term =
  Arg.(
    value
    & opt string (default_cache_dir ())
    & info ~docv:"DIR" ~doc:"Cache directory containing layers"
        ~env:(Cmd.Env.info "OI_CACHE_DIR")
        [ "cache-dir"; "C" ])

(* -- helpers -------------------------------------------------------------- *)

let with_d10 cache_dir env f =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let fs = Eio.Stdenv.fs env in
  let clock = Eio.Stdenv.clock env in
  let sys =
    D10.Sysops.create
      ~stdout:(Eio.Stdenv.stdout env)
      ~stderr:(Eio.Stdenv.stderr env)
      ~proc_mgr ~fs ()
  in
  let platform = Osrel.detect ~proc_mgr ~fs in
  let os_key = D10.Os_key.(to_string (of_platform platform)) in
  let d10 : D10.Config.t =
    {
      sys;
      fs;
      clock :> D10.Config.clk;
      root = Eio.Path.(fs / cache_dir);
      os_key;
    }
  in
  f d10

let short_hash h = if String.length h > 12 then String.sub h 0 12 else h

(* -- list ----------------------------------------------------------------- *)

let list_cmd =
  let run () cache_dir =
    Eio_posix.run @@ fun env ->
    with_d10 cache_dir env @@ fun d10 ->
    let layers_dir = Eio.Path.native_exn d10.root / "layers" / d10.os_key in
    Fmt.pr "@[<v>%a %s@,@," Fmt.(styled `Bold string) "Layers" d10.os_key;
    if not (Sys.file_exists layers_dir) then Fmt.pr "  (empty)@,"
    else begin
      let entries =
        Sys.readdir layers_dir |> Array.to_list |> List.sort String.compare
      in
      let total = ref 0 in
      List.iter
        (fun hash ->
          let json =
            Eio.Path.(d10.root / "layers" / d10.os_key / hash / "layer.json")
          in
          match D10.Layer.load_meta json with
          | Some m ->
              let status =
                if m.exit_status = 0 then
                  Fmt.str "%a" Fmt.(styled `Green string) "ok"
                else
                  Fmt.str "%a %d" Fmt.(styled `Red string) "fail" m.exit_status
              in
              Fmt.pr "  %a  %s  %s@,"
                Fmt.(styled `Faint string)
                (short_hash hash) status m.package;
              incr total
          | None ->
              Fmt.pr "  %a  %a@,"
                Fmt.(styled `Faint string)
                (short_hash hash)
                Fmt.(styled `Yellow string)
                "(no metadata)")
        entries;
      Fmt.pr "@,%a %d layers@," Fmt.(styled `Bold string) "Total:" !total
    end;
    Fmt.pr "@]@."
  in
  let info = Cmd.info "list" ~doc:"List all layers for the current platform" in
  Cmd.v info Term.(const run $ const () $ cache_dir_term)

(* -- show ----------------------------------------------------------------- *)

let show_cmd =
  let run () cache_dir package =
    Eio_posix.run @@ fun env ->
    with_d10 cache_dir env @@ fun d10 ->
    let layers_dir = Eio.Path.native_exn d10.root / "layers" / d10.os_key in
    if not (Sys.file_exists layers_dir) then Fmt.pr "No layers found.@."
    else begin
      let entries = Sys.readdir layers_dir |> Array.to_list in
      let found = ref false in
      List.iter
        (fun hash ->
          let json =
            Eio.Path.(d10.root / "layers" / d10.os_key / hash / "layer.json")
          in
          match D10.Layer.load_meta json with
          | Some m
            when String.length m.package >= String.length package
                 && String.sub m.package 0 (String.length package) = package ->
              found := true;
              Fmt.pr "@[<v>%a %s@," Fmt.(styled `Bold string) "Layer" hash;
              Fmt.pr "  package:  %s@," m.package;
              Fmt.pr "  status:   %s@,"
                (if m.exit_status = 0 then "ok"
                 else Fmt.str "failed (exit %d)" m.exit_status);
              Fmt.pr "  created:  %s@,"
                (let t = Unix.gmtime m.created in
                 Fmt.str "%04d-%02d-%02d %02d:%02d UTC" (t.tm_year + 1900)
                   (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min);
              Fmt.pr "  deps:     %s@,"
                (if m.deps = [] then "(none)" else String.concat ", " m.deps);
              Fmt.pr "  hashes:   %s@,"
                (if m.hashes = [] then "(none)"
                 else String.concat ", " (List.map short_hash m.hashes));
              (* Count files *)
              let fs_dir = layers_dir / hash / "fs" in
              if Sys.file_exists fs_dir then begin
                let count = ref 0 in
                let rec scan dir =
                  Array.iter
                    (fun name ->
                      let path = dir / name in
                      if Sys.is_directory path then scan path else incr count)
                    (Sys.readdir dir)
                in
                scan fs_dir;
                Fmt.pr "  files:    %d@," !count
              end;
              Fmt.pr "@]@."
          | _ -> ())
        entries;
      if not !found then Fmt.pr "No layers found matching %S@." package
    end
  in
  let package =
    Arg.(
      required
      & pos 0 (some string) None
      & info ~docv:"PACKAGE" ~doc:"Package name (prefix match)" [])
  in
  let info = Cmd.info "show" ~doc:"Show details for a package's layers" in
  Cmd.v info Term.(const run $ const () $ cache_dir_term $ package)

(* -- binaries ------------------------------------------------------------- *)

let binaries_cmd =
  let run () cache_dir =
    Eio_posix.run @@ fun env ->
    with_d10 cache_dir env @@ fun d10 ->
    let index_path = Eio.Path.native_exn d10.root / "layers" / "index.db" in
    if not (Sys.file_exists index_path) then
      Fmt.pr "No index found. Run 'd10 index' first.@."
    else begin
      let db = D10.Index.open_ ~path:index_path in
      let bins = D10.Index.all_binaries db ~os_key:d10.os_key in
      Fmt.pr "@[<v>%a@,@," Fmt.(styled `Bold string) "Available binaries";
      List.iter
        (fun (binary, pkg_name, pkg_ver) ->
          Fmt.pr "  %-30s  %s.%s@," binary pkg_name pkg_ver)
        bins;
      Fmt.pr "@,%a %d binaries@,@]@."
        Fmt.(styled `Bold string)
        "Total:" (List.length bins);
      D10.Index.close db
    end
  in
  let info = Cmd.info "binaries" ~doc:"List all binaries in the layer index" in
  Cmd.v info Term.(const run $ const () $ cache_dir_term)

(* -- index ---------------------------------------------------------------- *)

let index_cmd =
  let run () cache_dir =
    Eio_posix.run @@ fun env ->
    with_d10 cache_dir env @@ fun d10 ->
    let layers_root = Eio.Path.native_exn d10.root / "layers" in
    let index_path = layers_root / "index.db" in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(d10.fs / layers_root);
    let db = D10.Index.open_ ~path:index_path in
    let total_layers = ref 0 in
    let total_bins = ref 0 in
    let total_files = ref 0 in
    if Sys.file_exists layers_root then
      Array.iter
        (fun entry ->
          let dir = layers_root / entry in
          if
            Sys.is_directory dir && entry <> "." && entry <> ".."
            && (not (Filename.check_suffix entry ".db"))
            && (not (Filename.check_suffix entry ".db-shm"))
            && not (Filename.check_suffix entry ".db-wal")
          then begin
            let c : D10.Config.t =
              {
                sys = d10.sys;
                fs = d10.fs;
                clock = d10.clock;
                root = d10.root;
                os_key = entry;
              }
            in
            D10.Index.rebuild c db;
            let nl, nb, nf = D10.Index.stats db ~os_key:entry in
            Fmt.pr "  %s: %d layers, %d binaries, %d files@." entry nl nb nf;
            total_layers := !total_layers + nl;
            total_bins := !total_bins + nb;
            total_files := !total_files + nf
          end)
        (Sys.readdir layers_root);
    D10.Index.close db;
    Fmt.pr "Total: %d layers, %d binaries, %d files@." !total_layers !total_bins
      !total_files;
    Fmt.pr "Index: %s@." index_path
  in
  let info = Cmd.info "index" ~doc:"Rebuild the SQLite layer index" in
  Cmd.v info Term.(const run $ const () $ cache_dir_term)

(* -- stats ---------------------------------------------------------------- *)

let stats_cmd =
  let run () cache_dir =
    Eio_posix.run @@ fun env ->
    with_d10 cache_dir env @@ fun d10 ->
    let index_path = Eio.Path.native_exn d10.root / "layers" / "index.db" in
    if not (Sys.file_exists index_path) then
      Fmt.pr "No index found. Run 'd10 index' first.@."
    else begin
      let db = D10.Index.open_ ~path:index_path in
      let nl, nb, nf = D10.Index.stats db ~os_key:d10.os_key in
      Fmt.pr "@[<v>%a %s@,@," Fmt.(styled `Bold string) "Stats" d10.os_key;
      Fmt.pr "  layers:   %d@," nl;
      Fmt.pr "  binaries: %d@," nb;
      Fmt.pr "  files:    %d@," nf;
      Fmt.pr "@]@.";
      D10.Index.close db
    end
  in
  let info = Cmd.info "stats" ~doc:"Show layer cache statistics" in
  Cmd.v info Term.(const run $ const () $ cache_dir_term)

(* -- main ----------------------------------------------------------------- *)

let () =
  let info =
    Cmd.info "d10" ~version:"0.1.0"
      ~doc:"Inspect a d10 content-addressed layer cache"
  in
  let cmd =
    Cmd.group info [ list_cmd; show_cmd; binaries_cmd; index_cmd; stats_cmd ]
  in
  exit (Cmd.eval cmd)
