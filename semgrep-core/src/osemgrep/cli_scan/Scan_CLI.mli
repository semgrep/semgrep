(*
   'semgrep scan' (and also 'semgrep ci') command-line parsing.
*)

(*
   The result of parsing a 'semgrep scan' command.
*)
type conf = {
  autofix : bool;
  baseline_commit : string option;
  config : string;
  dryrun : bool;
  exclude : string list;
  exclude_rule_ids : string list;
  include_ : string list;
  lang : string option;
  logging_level : Logs.level option;
  max_memory_mb : int;
  max_target_bytes : int;
  metrics : Metrics.State.t;
  num_jobs : int;
  optimizations : bool;
  output_format : Output_format.t;
  pattern : string option;
  respect_git_ignore : bool;
  rewrite_rule_ids : bool;
  scan_unknown_extensions : bool;
  severity : Severity.rule_severity list;
  show_supported_languages : bool;
  strict : bool;
  target_roots : string list;
  time_flag : bool;
  timeout : float;
  timeout_threshold : int;
  version : bool;
  version_check : bool;
}

(* Command-line defaults. *)
val default : conf

(*
   Usage: parse_argv [| "semgrep-scan"; <args> |]

   This function returns an exit code to be passed to the 'exit' function
   if there was an error parsing argv (Exit_code.fatal) or when
   using semgrep scan --help (Exit_code.ok), and the conf otherwise if everything
   went fine.
*)
val parse_argv : string array -> (conf, Exit_code.t) result

(* exported because used by Ci_CLI.ml too *)
val cmdline_term : conf Cmdliner.Term.t
