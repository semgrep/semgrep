(* Full result information *)

type profiling = {
  file: Common.filename;
  parse_time: float;
  match_time: float;
  run_time: float;
}

(* Partial result information *)
(* To store match/parse information before total run_time for the *)
(* rule-file pair is computed *)

type partial_profiling = {
  file: Common.filename;
  parse_time: float;
  match_time: float;
}

(* To store only time information, as semgrep.check reports *)
type times = {
  parse_time: float;
  match_time: float
}

(* Substitute in the profiling type we have *)

type 'a match_result = {
  matches: Pattern_match.t list;
  errors: Error_code.error list;
  profiling: 'a;
}

(* Result object for the entire rule *)

type rule_profiling = {
  rule_parse_time: float;
  file_times: profiling list
}

type rule_result = {
  matches: Pattern_match.t list;
  errors: Error_code.error list;
  rule_profiling: rule_profiling option;
}

val empty_partial_profiling : Common.filename -> partial_profiling

val empty_semgrep_result : times match_result

val add_run_time : float -> partial_profiling match_result -> profiling match_result

val add_file : Common.filename -> times match_result -> partial_profiling match_result

val collate_semgrep_results : times match_result list -> times match_result

val make_rule_result : profiling match_result list -> report_time : bool -> rule_parse_time : float -> rule_result
