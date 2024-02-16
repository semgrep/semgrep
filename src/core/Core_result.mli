type processed_match = {
  pm : Pattern_match.t;
  (* semgrep-core is now responsible for the nosemgrep and autofix *)
  is_ignored : bool;
  autofix_edit : Textedit.t option;
}
[@@deriving show]

(* Final match result for all the files and all the rules *)
type t = {
  processed_matches : processed_match list;
  errors : Core_error.t list;
  (* The targets are all the files that were considered valid targets for the
   * semgrep scan. This excludes files that were filtered out on purpose
   * due to being in the wrong language, too big, etc.
   * It includes targets that couldn't be scanned, for instance due to
   * a parsing error.
   * TODO: are we actually doing that? this was a comment
   * for semgrep_with_raw_results_and_exn_handler (now scan_with_exn_handler)
   * but I'm not sure we're doing it.
   *)
  scanned : Fpath.t list;
  (* extra information useful to also give to the user (in JSON or
   * in textual reports) or for tools (e.g., the playground).
   *)
  skipped_targets : Semgrep_output_v1_t.skipped_target list;
  skipped_rules : Rule.invalid_rule_error list;
  rules_with_targets : Rule.rule list;
  extra : Core_profiling.t Core_profiling.debug_info;
  explanations : Matching_explanation.t list option;
  rules_by_engine : (Rule_ID.t * Engine_kind.t) list;
  interfile_languages_used : Xlang.t list;
}
[@@deriving show]

type result_or_exn = (t, Exception.t * Core_error.t option) result

val mk_processed_match : Pattern_match.t -> processed_match

(* Intermediate match result.
 * The 'a below can be substituted with different profiling types
 * in Core_profiling.ml.
 * This usually represents the match results for one target file
 * (possibly matches coming from more than one rule).
 *)

type 'a match_result = {
  matches : Pattern_match.t list;
  errors : Core_error.ErrorSet.t;
  extra : 'a Core_profiling.debug_info;
  explanations : Matching_explanation.t list;
}
[@@deriving show]

(* shortcut *)
type matches_single_file = Core_profiling.partial_profiling match_result
[@@deriving show]

(* take the match results for each file, all the rules, all the targets,
 * and build the final result
 *)
val make_final_result :
  Core_profiling.file_profiling match_result list ->
  (Rule.rule * Engine_kind.t) list ->
  Rule.invalid_rule_error list ->
  Fpath.t list ->
  Xlang.t list ->
  rules_parse_time:float ->
  t

(* This is useful when an exn was raised during a scan but we
 * still need to print a core_result on stdout in JSON. In that
 * case we usually transform the exn into a Core_error that gets
 * added in the errors field.
 * This is also used for semgrep-core metachecker (-check_rules)
 *)
val mk_final_result_with_just_errors : Core_error.t list -> t
val empty_match_result : Core_profiling.times match_result

val make_match_result :
  Pattern_match.t list -> Core_error.ErrorSet.t -> 'a -> 'a match_result

(* match results profiling adjustment helpers *)
val modify_match_result_profiling :
  'a match_result -> ('a -> 'b) -> 'b match_result

val add_run_time :
  float ->
  Core_profiling.partial_profiling match_result ->
  Core_profiling.file_profiling match_result

val add_rule :
  Rule.rule ->
  Core_profiling.times match_result ->
  Core_profiling.rule_profiling match_result

(* aggregate results *)
val collate_pattern_results :
  Core_profiling.times match_result list -> Core_profiling.times match_result

val collate_rule_results :
  Fpath.t ->
  Core_profiling.rule_profiling match_result list ->
  Core_profiling.partial_profiling match_result
