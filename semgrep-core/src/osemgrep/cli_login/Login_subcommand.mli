(*
   Parse a semgrep-login command, execute it and exit.

   Usage: main [| "semgrep-login"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : string array -> Exit_code.t

(* no parameters for now *)
type login_cli_conf = unit

(* internal *)
val run : login_cli_conf -> Exit_code.t
