type t = {
  (* targets skipped because of file targeting semantic *)
  ignored : Semgrep_output_v1_t.skipped_target list;
  size : Semgrep_output_v1_t.skipped_target list;
  include_ : Semgrep_output_v1_t.skipped_target list;
  exclude : Semgrep_output_v1_t.skipped_target list;
  always : Semgrep_output_v1_t.skipped_target list;
  other : Semgrep_output_v1_t.skipped_target list;
  (* targets skipped because there was parsing/matching
   * errors while running the engine on it (see errors_to_skipped())
   *)
  errors : Semgrep_output_v1_t.skipped_target list;
}

val group : Semgrep_output_v1_t.skipped_target list -> t

(* internal also used in Scan_subcommand.ml *)
val errors_to_skipped :
  Semgrep_output_v1_t.core_error list -> Semgrep_output_v1_t.skipped_target list
