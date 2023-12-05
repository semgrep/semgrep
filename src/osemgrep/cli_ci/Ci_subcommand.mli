(*
   Parse a semgrep-ci command, execute it and exit.

   Usage: main [| "semgrep-ci"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : Cap.all_caps -> string array -> Exit_code.t

(* internal *)
val run_conf : Cap.all_caps -> Ci_CLI.conf -> Exit_code.t
