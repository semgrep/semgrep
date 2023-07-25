val pp_summary :
  Format.formatter ->
  bool
  * CLI_common.maturity option
  * int
  * Semgrep_output_v1_t.skipped_target list
  * Semgrep_output_v1_t.skipped_target list
  * Semgrep_output_v1_t.skipped_target list
  * Semgrep_output_v1_t.skipped_target list
  * Semgrep_output_v1_t.skipped_target list
  * Semgrep_output_v1_t.skipped_target list ->
  unit
