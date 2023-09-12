(* entry point *)
val cli_output_of_core_results :
  logging_level:Logs.level option ->
  (* essentially Core_runner.result *)
  Semgrep_output_v1_t.core_output ->
  Rule.hrules ->
  Fpath.t Set_.t ->
  Semgrep_output_v1_j.cli_output

(* internals used in Scan_subcommant.ml *)
val exit_code_of_error_type : Semgrep_output_v1_t.core_error_kind -> Exit_code.t

(* internals used also for incremental display of matches *)
val cli_match_of_core_match :
  Rule.hrules -> Semgrep_output_v1_t.core_match -> Semgrep_output_v1_t.cli_match

val index_match_based_ids :
  Semgrep_output_v1_t.cli_match list -> Semgrep_output_v1_t.cli_match list
(** [index_match_based_ids matches] will append an index to the match based id
  * where the index is what # finding of the same rule kind in the same file
  * it is. This is needed for the App to do deduplication
  *)

val dedup_and_sort :
  Semgrep_output_v1_t.cli_match list -> Semgrep_output_v1_t.cli_match list
