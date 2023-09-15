(* Options for what extra debugging information to output*)

type debug_mode = MDebug | MTime | MNo_info [@@deriving show]

type 'a debug_info =
  | Debug of {
      skipped_targets : Semgrep_output_v1_t.skipped_target list;
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
  rule_id : Rule_ID.t;
  parse_time : float;
  match_time : float;
}
[@@deriving show]

(* Save time information as we run each file *)

type partial_profiling = { file : Fpath.t; rule_times : rule_profiling list }
[@@deriving show]

type file_profiling = {
  file : Fpath.t;
  rule_times : rule_profiling list;
  run_time : float;
}
[@@deriving show]

type t = {
  rules : Rule.rule list;
  rules_parse_time : float;
  file_times : file_profiling list;
  max_memory_bytes : int;
}
[@@deriving show]

val empty_extra : 'a -> 'a debug_info
val empty_partial_profiling : Fpath.t -> partial_profiling
val empty_rule_profiling : Rule.t -> rule_profiling
