(* Entry point for osemgrep standalone program. *)

let () =
  let exit_code = CLI.main Sys.argv in
  exit (Exit_code.to_int exit_code)
