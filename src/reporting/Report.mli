(* Options for what extra debugging information to output*)

type debug_mode = MDebug | MTime | MNo_info [@@deriving show]

type 'a debug_info =
  | Debug of {
      skipped_targets : Output_from_core_t.skipped_target list;
      profiling : 'a;
    }
  | Time of { profiling : 'a }
  | No_info
[@@deriving show]

val debug_info_to_option : 'a debug_info -> 'a option
(** [debug_info_to_option debug] returns [Some profiling] if we collected
    metrics. Otherwise, it returns [None]. *)

(* Global to set the debug mode. Should be set
   exactly once after the arguments are read *)

val mode : debug_mode ref

(* Save time information as we run each rule *)

type times = { parse_time : float; match_time : float }

type rule_profiling = {
  rule_id : Rule.rule_id;
  parse_time : float;
  match_time : float;
}
[@@deriving show]

(* Save time information as we run each file *)

type partial_profiling = {
  file : Common.filename;
  rule_times : rule_profiling list;
}
[@@deriving show]

type file_profiling = {
  file : Common.filename;
  rule_times : rule_profiling list;
  run_time : float;
}
[@@deriving show]

type rule_id_and_engine_kind = Rule_ID.t * Pattern_match.engine_kind
[@@deriving show]

(* Substitute in the profiling type we have *)

module ErrorSet : Set.S with type elt = Semgrep_error_code.error

type 'a match_result = {
  matches : Pattern_match.t list;
  errors : ErrorSet.t;
  extra : 'a debug_info;
  explanations : Matching_explanation.t list;
}
[@@deriving show]

(* Result object for the entire rule *)

type final_profiling = {
  rules : Rule.rule list;
  rules_parse_time : float;
  file_times : file_profiling list;
  max_memory_bytes : int;
}
[@@deriving show]

type final_result = {
  matches : Pattern_match.t list;
  errors : Semgrep_error_code.error list;
  skipped_rules : Rule.invalid_rule_error list;
  extra : final_profiling debug_info;
  explanations : Matching_explanation.t list;
  rules_by_engine : rule_id_and_engine_kind list;
}
[@@deriving show]

val empty_extra : 'a -> 'a debug_info
val empty_partial_profiling : Common.filename -> partial_profiling
val empty_rule_profiling : Rule.t -> rule_profiling
val empty_semgrep_result : times match_result
val empty_final_result : final_result

val make_match_result :
  Pattern_match.t list -> ErrorSet.t -> 'a -> 'a match_result

val add_run_time :
  float -> partial_profiling match_result -> file_profiling match_result

val modify_match_result_profiling :
  'a match_result -> ('a -> 'b) -> 'b match_result

val add_rule : Rule.rule -> times match_result -> rule_profiling match_result
val collate_pattern_results : times match_result list -> times match_result

val make_final_result :
  file_profiling match_result list ->
  (Rule.rule * Pattern_match.engine_kind) list ->
  rules_parse_time:float ->
  final_result

val collate_rule_results :
  string -> rule_profiling match_result list -> partial_profiling match_result
