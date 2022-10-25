(* no parameters for now *)
type logout_cli_conf = unit

(*
   Parse a semgrep-logout command, execute it and exit.

   Usage: main [| "semgrep-logut"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
   Exceptions are caught and turned into an appropriate exit code.
*)
val main : string array -> Exit_code.t

(* internal *)
val run : logout_cli_conf -> Exit_code.t
