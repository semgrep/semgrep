(* The 'a below can be substituted with different profiling types
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

(* Final match result for all the files and all the rules *)

type t = {
  matches : Pattern_match.t list;
  errors : Core_error.t list;
  skipped_rules : Rule.invalid_rule_error list;
  (* may contain skipped_target info *)
  extra : Core_profiling.t Core_profiling.debug_info;
  explanations : Matching_explanation.t list;
  rules_by_engine : (Rule_ID.t * Pattern_match.engine_kind) list;
  (* The targets are all the files that were considered valid targets for the
   * semgrep scan. This excludes files that were filtered out on purpose
   * due to being in the wrong language, too big, etc.
   * It includes targets that couldn't be scanned, for instance due to
   * a parsing error.
   * TODO: are we actually doing that? this was a comment
   * for semgrep_with_raw_results_and_exn_handler but I'm not sure
   * we're doing it here.
   *)
  scanned : Fpath.t list;
}
[@@deriving show]

val empty_match_result : Core_profiling.times match_result
val empty_final_result : t

val make_match_result :
  Pattern_match.t list -> Core_error.ErrorSet.t -> 'a -> 'a match_result

(* take the match results for each file, all the rules, all the targets,
 * and build the final result
 *)
val make_final_result :
  Core_profiling.file_profiling match_result list ->
  (Rule.rule * Pattern_match.engine_kind) list ->
  Rule.invalid_rule_error list ->
  Fpath.t list ->
  rules_parse_time:float ->
  t

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
