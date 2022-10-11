(* Entry point for osemgrep standalone program. *)

let () =
  let exit_code = Semgrep_CLI_lib.main Sys.argv in
  exit (Exit_code.to_int exit_code)
