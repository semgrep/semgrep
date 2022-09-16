open Common

(*
   Type definitions, mostly.
*)

(* in JSON mode, we might need to display intermediate '.' in the
 * output for semgrep to track progress as well as extra targets
 * found by extract rules.
 *)
type output_format = Text | Json of bool (* dots *) [@@deriving show]

type t = {
  (* Debugging/profiling/logging flags *)
  log_config_file : filename;
  log_to_file : filename option;
  test : bool;
  debug : bool;
  profile : bool;
  report_time : bool;
  error_recovery : bool;
  profile_start : float;
  matching_explanations : bool;
  (* Main flags *)
  pattern_string : string;
  pattern_file : filename;
  rules_file : filename;
  equivalences_file : string;
  lang : Xlang.t option;
  roots : Common.path list;
  output_format : output_format;
  match_format : Matching_report.match_format;
  mvars : Metavariable.mvar list;
  lsp : bool;
  (* Limits *)
  (* maximum time to spend running a rule on a single file *)
  timeout : float;
  (* maximum number of rules that can timeout on a file *)
  timeout_threshold : int;
  max_memory_mb : int;
  max_match_per_file : int;
  ncores : int;
  parsing_cache_dir : Common.dirname; (* "" means no cache *)
  (* Flag used by the semgrep-python wrapper *)
  target_file : string;
  (* Common.ml action for the -dump_xxx *)
  action : string;
  (* Other *)
  version : string;
}
