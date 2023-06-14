(* TODO: this is an ugly signature. Should define a record to hold the data *)
val pp_skipped :
  Format.formatter ->
  bool
  * bool
  * int
  * Semgrep_output_v1_t.skipped_target list
  * Semgrep_output_v1_t.skipped_target list
  * Semgrep_output_v1_t.skipped_target list
  * Semgrep_output_v1_t.skipped_target list
  * Semgrep_output_v1_t.skipped_target list
  * Semgrep_output_v1_t.skipped_target list ->
  unit
