(* entry point *)
val cli_output_of_core_results :
  Scan_CLI.conf -> Core_runner.result -> Semgrep_output_v1_j.cli_output

(* internal function used for the 'lines:' field in the JSON output
 * but also now in Output.ml for the Emacs output.
 *)
val lines_of_file :
  Semgrep_output_v1_j.position * Semgrep_output_v1_j.position ->
  Common.filename ->
  string list

(* for now used only to interpolate metavars in the 'message:' field *)
val contents_of_file :
  Semgrep_output_v1_j.position * Semgrep_output_v1_j.position ->
  Common.filename ->
  string list
