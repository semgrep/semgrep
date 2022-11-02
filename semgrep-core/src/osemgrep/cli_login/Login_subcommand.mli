(* no parameters for now *)
type login_cli_conf = unit

(*
   Parse a semgrep-login command, execute it and exit.

   Usage: main [| "semgrep-login"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
   Exceptions are caught and turned into an appropriate exit code.
*)
val main : string array -> Exit_code.t

(* internal *)
val run : login_cli_conf -> Exit_code.t
