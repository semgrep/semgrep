(*
   Parse a semgrep-ci command, execute it and exit.

   Usage: main [| "semgrep-ci"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
   Exceptions are caught and turned into an appropriate exit code.
*)
val main : string array -> Exit_code.t

(* internal *)
val run : Ci_CLI.conf -> Exit_code.t
