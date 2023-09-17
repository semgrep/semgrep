(*
   Parse a semgrep-scan command, execute it and exit.

   Usage: main [| "semgrep-scan"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : string array -> Exit_code.t

(* internal *)
val run_conf : Scan_CLI.conf -> Exit_code.t
val run_scan_conf : Scan_CLI.conf -> Exit_code.t

(* Semgrep Pro hook *)
(* TODO it might be better to pass this through and avoid the hook,
   but it was fairly annoying to *)
val invoke_semgrep_core_proprietary :
  (Fpath.t list ->
  ?diff_config:Differential_scan_config.t ->
  Engine_type.t ->
  Core_runner.core_scan_func_for_osemgrep)
  option
  ref

(* internal: scan all the files - also used in CI *)
val run_scan_files :
  Scan_CLI.conf ->
  Profiler.t ->
  Rule_fetching.rules_and_origin list ->
  Fpath.t list * Semgrep_output_v1_t.skipped_target list ->
  ( Rule.rule list * Core_runner.result * Semgrep_output_v1_t.cli_output,
    Exit_code.t )
  result
