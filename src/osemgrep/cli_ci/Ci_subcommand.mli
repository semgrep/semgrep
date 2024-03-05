module OutJ = Semgrep_output_v1_j

(*
   Parse a semgrep-ci command, execute it and exit.

   Usage: main [| "semgrep-ci"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main :
  < Cap.network ; Cap.stdout ; Cap.exec ; Cap.tmp ; Cap.chdir > ->
  string array ->
  Exit_code.t

(* internal *)
val run_conf :
  < Cap.network ; Cap.stdout ; Cap.exec ; Cap.tmp ; Cap.chdir > ->
  Ci_CLI.conf ->
  Exit_code.t

val rule_is_blocking : JSON.t -> bool
val finding_is_blocking : OutJ.cli_match -> bool
