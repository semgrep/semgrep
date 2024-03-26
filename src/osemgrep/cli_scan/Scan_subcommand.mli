(*
   Parse a semgrep-scan command, execute it and exit.

   Usage: main [| "semgrep-scan"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)

type caps = < Cap.stdout ; Cap.network ; Cap.tmp ; Cap.chdir >

val main : caps -> string array -> Exit_code.t

(* internal *)
val run_conf : caps -> Scan_CLI.conf -> Exit_code.t
val run_scan_conf : caps -> Scan_CLI.conf -> Exit_code.t

(* internal: scan all the files - also used in CI *)
val run_scan_files :
  < Cap.stdout ; Cap.chdir ; Cap.tmp > ->
  Scan_CLI.conf ->
  Profiler.t ->
  Rule_fetching.rules_and_origin list ->
  Fpath.t list * Semgrep_output_v1_t.skipped_target list ->
  ( Rule.rule list * Core_runner.result * Semgrep_output_v1_t.cli_output,
    Exit_code.t )
  result
