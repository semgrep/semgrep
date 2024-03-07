(* entry point *)
val cli_output_of_core_results :
  dryrun:bool ->
  logging_level:Logs.level option ->
  (* essentially Core_runner.result *)
  Semgrep_output_v1_t.core_output ->
  Rule.hrules ->
  Fpath.t Set_.t ->
  Semgrep_output_v1_j.cli_output

(* internals used in Scan_subcommant.ml *)
val exit_code_of_error_type : Semgrep_output_v1_t.error_type -> Exit_code.t

(* internals used also for incremental display of matches
   The [applied_fixes] hash table is both in and out parameter, it is used for
   deciding whether a fixed_lines elements is included in the cli_match. This
   depends on whether an overlapping fix was already included in an earlier
   cli_match in the same list of matches.
*)
val cli_match_of_core_match :
  dryrun:bool ->
  ?applied_fixes:(string, (int * int) list) Hashtbl.t ->
  Rule.hrules ->
  Semgrep_output_v1_t.core_match ->
  Semgrep_output_v1_t.cli_match

val index_match_based_ids :
  Semgrep_output_v1_t.cli_match list -> Semgrep_output_v1_t.cli_match list
(** [index_match_based_ids matches] will append an index to the match based id
  * where the index is what # finding of the same rule kind in the same file
  * it is. This is needed for the App to do deduplication
  *)
