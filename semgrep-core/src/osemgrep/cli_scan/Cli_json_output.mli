val cli_output_of_core_results :
  Scan_CLI.conf -> Core_runner.result -> Semgrep_output_v1_j.cli_output

val lines_of_file :
  Semgrep_output_v1_j.position * Semgrep_output_v1_j.position ->
  Common.filename ->
  string list
