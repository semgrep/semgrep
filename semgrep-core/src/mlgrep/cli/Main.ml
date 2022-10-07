(* Entry point for mlgrep standalone program. *)

let () = Semgrep_CLI_lib.main Sys.argv |> exit
