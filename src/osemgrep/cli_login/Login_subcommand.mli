(*
   Parse a semgrep-login command, execute it and exit.

   Usage: main [| "semgrep-login"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : string array -> Exit_code.t

(* internal *)
val run : Login_CLI.conf -> Exit_code.t
