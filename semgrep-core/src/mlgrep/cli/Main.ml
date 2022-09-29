(* Entry point for mlgrep stand-alone program. *)

let () = Semgrep_CLI_lib.main Sys.argv |> exit
