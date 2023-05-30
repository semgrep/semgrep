(* entry point *)
val cli_output_of_core_results :
  logging_level:Logs.level option ->
  Core_runner.result ->
  Semgrep_output_v1_j.cli_output

(* internal function used for the 'lines:' field in the JSON output
 * but also now in Output.ml for the Emacs output.
 *)
val lines_of_file :
  Semgrep_output_v1_j.position * Semgrep_output_v1_j.position ->
  Fpath.t ->
  string list

(* for now used only to interpolate metavars in the 'message:' field *)
val contents_of_file :
  Semgrep_output_v1_j.position * Semgrep_output_v1_j.position ->
  Common.filename ->
  string

(* internals used in Scan_subcommant.ml *)
val exit_code_of_error_type : Semgrep_output_v1_j.core_error_kind -> Exit_code.t

(* internals used also for incremental display of matches *)
type env = { hrules : Rule.hrules }

val cli_match_of_core_match :
  env -> Semgrep_output_v1_t.core_match -> Semgrep_output_v1_t.cli_match

val dedup_and_sort :
  Semgrep_output_v1_t.cli_match list -> Semgrep_output_v1_t.cli_match list
