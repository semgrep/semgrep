(* Save time information as we run each rule *)

type times = { parse_time : float; match_time : float }

type rule_profiling = {
  rule_id : Rule.rule_id;
  parse_time : float;
  match_time : float;
}

(* Save time information as we run each file *)

type partial_profiling = {
  file : Common.filename;
  rule_times : rule_profiling list;
}

type file_profiling = {
  file : Common.filename;
  rule_times : rule_profiling list;
  run_time : float;
}

(* Substitute in the profiling type we have *)

type 'a match_result = {
  matches : Pattern_match.t list;
  errors : Semgrep_error_code.error list;
  skipped : Output_from_core_t.skipped_target list;
  profiling : 'a;
}

(* Result object for the entire rule *)

type final_profiling = {
  rules : Rule.rule list;
  rules_parse_time : float;
  file_times : file_profiling list;
}

type final_result = {
  matches : Pattern_match.t list;
  errors : Semgrep_error_code.error list;
  skipped : Output_from_core_t.skipped_target list;
  final_profiling : final_profiling option;
}

val empty_partial_profiling : Common.filename -> partial_profiling

val empty_semgrep_result : times match_result

val add_run_time :
  float -> partial_profiling match_result -> file_profiling match_result

val add_rule : Rule.rule -> times match_result -> rule_profiling match_result

val collate_pattern_results : times match_result list -> times match_result

val make_final_result :
  file_profiling match_result list ->
  Rule.rule list ->
  report_time:bool ->
  rules_parse_time:float ->
  final_result

val collate_rule_results :
  string -> rule_profiling match_result list -> partial_profiling match_result
