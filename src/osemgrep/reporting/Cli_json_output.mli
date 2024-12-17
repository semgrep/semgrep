module Out = Semgrep_output_v1_j

(* Entry point *)
val json_output : Out.format_context -> Out.cli_output -> string

(* Helper used to build a cli_output from a Core_runner result used
 * by other Xxx_output.ml files.
 * When fixed_lines is true, the JSON output contains the fixed lines
 * (and in Scan_subcommand.ml we do not apply the autofix on the files).
 *)
val cli_output_of_runner_result :
  fixed_lines:bool ->
  (* those 3 parameters are essentially Core_runner.result *)
  Out.core_output ->
  Rule.hrules ->
  Fpath.t Set_.t ->
  Out.cli_output

(* internals used in Scan_subcommand.ml *)
val exit_code_of_error_type : Out.error_type -> Exit_code.t

(* internals used also for incremental display of matches. The Fixed_lines.env is
 * used for deciding whether a fixed_lines elements is included in the
 * cli_match. This depends on whether an overlapping fix was already included in
 * an earlier cli_match in the same list of matches. *)
val cli_match_of_core_match :
  fixed_lines:bool ->
  Fixed_lines.env ->
  Rule.hrules ->
  Out.core_match ->
  Out.cli_match

val index_match_based_ids : Out.cli_match list -> Out.cli_match list
(** [index_match_based_ids matches] will append an index to the match based id
  * where the index is what # finding of the same rule kind in the same file
  * it is. This is needed for the App to do deduplication
  *)
