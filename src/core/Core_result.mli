(* The 'a below can be substituted with different profiling types
 * in Core_profiling.ml.
 * This usually represent the match results for one target file
 * (possibly using more than one rule).
 *)

type 'a match_result = {
  matches : Pattern_match.t list;
  errors : Core_error.ErrorSet.t;
  extra : 'a Core_profiling.debug_info;
  explanations : Matching_explanation.t list;
}
[@@deriving show]

(* Result object for all the files, all the rules *)

type t = {
  matches : Pattern_match.t list;
  errors : Core_error.t list;
  skipped_rules : Rule.invalid_rule_error list;
  extra : Core_profiling.t Core_profiling.debug_info;
  explanations : Matching_explanation.t list;
  rules_by_engine : rule_id_and_engine_kind list;
}

and rule_id_and_engine_kind = Rule_ID.t * Pattern_match.engine_kind
[@@deriving show]

val empty_match_result : Core_profiling.times match_result
val empty_final_result : t

val make_match_result :
  Pattern_match.t list -> Core_error.ErrorSet.t -> 'a -> 'a match_result

(* take the match results for each file, all the rules, and build the final
 * result *)
val make_final_result :
  Core_profiling.file_profiling match_result list ->
  (Rule.rule * Pattern_match.engine_kind) list ->
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

val collate_pattern_results :
  Core_profiling.times match_result list -> Core_profiling.times match_result

val collate_rule_results :
  Fpath.t ->
  Core_profiling.rule_profiling match_result list ->
  Core_profiling.partial_profiling match_result
