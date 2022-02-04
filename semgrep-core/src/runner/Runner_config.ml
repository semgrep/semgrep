open Common

(*
   Type definitions, mostly.
*)

type output_format = Text | Json [@@deriving show]

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
  timeout : float;
  (* per rule on a single file *)
  (* TODO: timeout_threshold *)
  max_memory_mb : int;
  max_match_per_file : int;
  ncores : int;
  (* Flags used by the semgrep-python wrapper *)
  use_parsing_cache : string;
  target_file : string;
  action : string;
  (* Other *)
  version : string;
}
