(*
   Install semgrep in CI for a given repository.
*)

(* we need Cap.exec for calling 'git', 'gh', 'command' *)
type caps = < Cap.random ; Cap.chdir ; Cap.tmp ; Cap.exec >

(*
   Parse a semgrep-install-ci command, execute it and exit.

   Usage: main [| "semgrep-install-ci"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : < caps ; .. > -> string array -> Exit_code.t

(* internal *)
val run_conf : < caps ; .. > -> Install_ci_CLI.conf -> Exit_code.t
