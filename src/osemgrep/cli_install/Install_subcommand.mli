(*
   Install semgrep in CI for a given repository.
*)

(*
   Parse a semgrep-install-ci command, execute it and exit.

   Usage: main [| "semgrep-install-ci"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : < Cap.random ; Cap.chdir > -> string array -> Exit_code.t

(* internal *)
val run_conf : < Cap.random ; Cap.chdir > -> Install_CLI.conf -> Exit_code.t
