val semgrep_with_raw_results_and_exn_handler :
  Runner_common.config ->
  Common.filename list ->
  exn option * Report.rule_result * Common.filename list
(** [semgrep_with_raw_results_and_exn_handler config roots] runs the semgrep
    engine starting from a list of [roots] and returns
    (success, result, targets).
    The targets are all the files that were considered valid targets for the
    semgrep scan. This excludes files that were filtered out on purpose
    due to being in the wrong language, too big, etc.
    It includes targets that couldn't be scanned, for instance due to
    a parsing error.
*)

val semgrep_with_formatted_output :
  Runner_common.config -> Common.filename list -> unit
(** [semgrep_with_formatted_output config roots] calls
    [semgrep_with_raw_results_and_exn_handler] and
    format the results on stdout either in a JSON or Textual format
    (depending on the value in config.output_format)
*)

val semgrep_with_one_pattern :
  Runner_common.config -> Common.filename list -> unit
(** this is the function used when running semgrep with -e or -f *)

(* internal functions used in tests or semgrep-core variants *)

val replace_named_pipe_by_regular_file : Common.filename -> Common.filename
(**
   Copy named pipes created with <(echo 'foo') on the command line
   into a regular file to avoid illegal seeks when reporting match results
   or parsing errors.
   Any file coming from the command line should go through this so as to
   allows easy manual testing.
*)

val print_match :
  ?str:string ->
  Matching_report.match_format ->
  Metavariable.mvar list ->
  Metavariable.bindings ->
  (Metavariable.mvalue -> Parse_info.t list) ->
  Parse_info.t list ->
  unit

val exn_to_error : Common.filename -> exn -> Semgrep_error_code.error
(**
  Small wrapper over Semgrep_error_code.exn_to_error to handle also
  semgrep-specific exns that have a position.
  See also JSON_report.json_of_exn for non-target related exn handling.
*)

val xlang_files_of_dirs_or_files :
  Xlang.t ->
  Common.filename list ->
  Common.filename list * Semgrep_core_response_j.skipped_target list
(**
  Small wrapper over Find_target.files_of_dirs_or_files to handle
  also Xlang.t (LRegex and LGeneric)
 *)

val filter_files_with_too_many_matches_and_transform_as_timeout :
  int ->
  Pattern_match.t list ->
  Pattern_match.t list
  * Semgrep_error_code.error list
  * Semgrep_core_response_j.skipped_target list

val timeout_function : Common.filename -> float -> (unit -> 'a) -> 'a
