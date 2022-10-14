(*
   Parse a semgrep-scan command, execute it and exit.

   Usage: main [| "semgrep-scan"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
   Exceptions are caught and turned into an appropriate exit code.
*)
val main : string array -> Exit_code.t

(* internal *)
val run : Scan_CLI.conf -> Exit_code.t
