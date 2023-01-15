(* Entry point for the osemgrep standalone program.

   Translated from __main__.py
*)

let () =
  let exit_code = CLI.main Sys.argv in
  exit (Exit_code.to_int exit_code)
