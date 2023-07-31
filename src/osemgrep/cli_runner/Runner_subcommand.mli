(*
   Parse a semgrep-runner command, execute it and exit.

   Usage: main [| "semgrep-runner"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : string array -> Exit_code.t

(* internal *)
val run : Runner_CLI.conf -> Exit_code.t
