(* this type is used both for Skipped_report.ml and Summary_report.ml *)
type skipped_targets_grouped = {
  (* targets skipped because of file targeting semantic *)
  ignored : Semgrep_output_v1_t.skipped_target list;
  size : Semgrep_output_v1_t.skipped_target list;
  include_ : Semgrep_output_v1_t.skipped_target list;
  exclude : Semgrep_output_v1_t.skipped_target list;
  other : Semgrep_output_v1_t.skipped_target list;
  (* targets skipped because there was parsing/matching
   * errors while running the engine on it
   * (see errors_skipped())
   *)
  errors : Semgrep_output_v1_t.skipped_target list;
}

val errors_to_skipped :
  Semgrep_output_v1_t.core_error list -> Semgrep_output_v1_t.skipped_target list

val group_skipped :
  errors_skipped:Semgrep_output_v1_t.skipped_target list ->
  Semgrep_output_v1_t.skipped_target list ->
  skipped_targets_grouped

val pp_skipped :
  Format.formatter -> bool * Maturity.t * int * skipped_targets_grouped -> unit
