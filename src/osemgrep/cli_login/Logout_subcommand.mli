(*
   Parse a semgrep-logout command, execute it and exit.

   Usage: main caps [| "semgrep-logout"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : < Cap.stdout > -> string array -> Exit_code.t

(* internal *)
val run_conf : < Cap.stdout > -> Logout_CLI.conf -> Exit_code.t
