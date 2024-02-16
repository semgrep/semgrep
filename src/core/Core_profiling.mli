(* Should be set exactly once after the CLI arguments are read *)
val profiling : bool ref

type times = { parse_time : float; match_time : float }

(* Save time information as we run each rule *)
type rule_profiling = {
  rule_id : Rule_ID.t;
  rule_parse_time : float;
  rule_match_time : float;
}
[@@deriving show]

(* Save time information as we run each file *)
type file_profiling = {
  file : Fpath.t;
  rule_times : rule_profiling list;
  run_time : float;
}
[@@deriving show]

type partial_profiling = {
  p_file : Fpath.t;
  p_rule_times : rule_profiling list;
}
[@@deriving show]

type t = {
  rules : Rule.rule list;
  rules_parse_time : float;
  file_times : file_profiling list;
  max_memory_bytes : int;
}
[@@deriving show]

val merge : t -> t -> t
val add_times : times -> times -> times
val empty_partial_profiling : Fpath.t -> partial_profiling
val empty_rule_profiling : Rule.t -> rule_profiling

(* return Some prof if profiling above was set to true, otherwise
 * discard the profiling information.
 *)
val profiling_opt : 'a -> 'a option
