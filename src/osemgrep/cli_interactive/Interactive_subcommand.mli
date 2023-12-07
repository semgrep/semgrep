(*
   Parse a semgrep-interactive command, execute it and exit.

   Usage: main [| "semgrep-interactive"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : string array -> Exit_code.t

(* internal *)
val run_conf : Interactive_CLI.conf -> Exit_code.t
