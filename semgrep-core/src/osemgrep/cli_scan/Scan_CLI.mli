(*
   'semgrep scan' (and also 'semgrep ci') command-line parsing.
*)

(*
   The result of parsing a 'semgrep scan' command.
*)
type conf = {
  autofix : bool;
  baseline_commit : string option;
  dryrun : bool;
  error : bool;
  exclude : string list;
  exclude_rule_ids : Rule.rule_id list;
  force_color : bool;
  include_ : string list;
  logging_level : Logs.level option;
  max_memory_mb : int;
  max_target_bytes : int;
  metrics : Metrics.State.t;
  num_jobs : int;
  optimizations : bool;
  output_format : Output_format.t;
  profile : bool;
  respect_git_ignore : bool;
  rewrite_rule_ids : bool;
  rules_source : rules_source;
  scan_unknown_extensions : bool;
  severity : Severity.rule_severity list;
  show_supported_languages : bool;
  strict : bool;
  target_roots : string list;
  test : bool;
  test_ignore_todo : bool;
  time_flag : bool;
  timeout : float;
  timeout_threshold : int;
  validate : bool;
  version : bool;
  version_check : bool;
}

and rules_source =
  (* -e/-l/--replacement *)
  | Pattern of string * Xlang.t * string option (* replacement *)
  (* --config *)
  | Configs of string list
[@@deriving show]

(* Command-line defaults. *)
val default : conf

(*
   Usage: parse_argv [| "semgrep-scan"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.
*)
val parse_argv : string array -> conf

(* exported because used by Ci_CLI.ml too *)
val cmdline_term : conf Cmdliner.Term.t
