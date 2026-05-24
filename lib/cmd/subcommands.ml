(* Canonical list of every top-level [oi] subcommand. Owned here (rather
   than in [bin/main.ml]) so the [oix] wrapper can derive subcommand
   names via {!Cmdliner.Cmd.name} instead of hardcoding them. *)

let all : unit Cmdliner.Cmd.t list =
  [
    Run.cmd;
    Build.cmd;
    Build.test_cmd;
    Install.cmd;
    Ir.cmd;
    Dist_cmd.cmd;
    Add.cmd;
    Exec.cmd;
    Search.cmd;
    Show.cmd;
    Env.cmd;
    Config.cmd;
    Repo.cmd;
    Clean.cmd;
    Cache.cmd;
    Self.cmd;
  ]

let names () = List.map Cmdliner.Cmd.name all
