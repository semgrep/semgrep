type output_format = Text | Json

type config = {
  (* Debugging/profiling/logging flags *)
  log_config_file : string;
  test : bool;
  debug : bool;
  profile : bool;
  report_time : bool;
  error_recovery : bool;
  fail_fast : bool;
  profile_start : float;
  (* Main flags *)
  pattern_string : string;
  (* -e *)
  pattern_file : string;
  (* -f *)
  rules_file : string;
  (* -rules_file *)
  config_file : string;
  (* -config *)
  equivalences_file : string;
  lang : string;
  output_format : output_format;
  match_format : Matching_report.match_format;
  mvars : Metavariable.mvar list;
  lsp : bool;
  (* Limits *)
  timeout : float;
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

exception Main_timeout of string

val semgrep_with_patterns_file : config -> string list -> unit

val semgrep_with_rules_file : config -> string list -> unit

val semgrep_with_one_pattern : config -> string list -> unit
