(*
   'semgrep scan' (and also 'semgrep ci') command-line parsing.
*)

(*
   The result of parsing a 'semgrep scan' command.
*)
type conf = {
  (* Main configuration options *)
  (* mix of --pattern/--lang/--replacement, --config *)
  rules_source : rules_source;
  (* can be a list of files or directories *)
  target_roots : string list;
  (* Configuration refinements *)
  exclude_rule_ids : Rule.rule_id list;
  severity : Severity.rule_severity list;
  autofix : bool;
  dryrun : bool;
  error_on_findings : bool;
  strict : bool;
  (* Path options *)
  exclude : string list;
  include_ : string list;
  max_target_bytes : int;
  respect_git_ignore : bool;
  (* TODO? better parsing of the string? a Git.version type? *)
  baseline_commit : string option;
  scan_unknown_extensions : bool;
  (* Performance options *)
  num_jobs : int;
  optimizations : bool;
  max_memory_mb : int;
  timeout : float;
  timeout_threshold : int;
  (* Display options *)
  (* mix of --json, --emacs, --vim, etc. *)
  output_format : Output_format.t;
  (* mix of --debug, --quiet, --verbose *)
  logging_level : Logs.level option;
  rewrite_rule_ids : bool;
  force_color : bool;
  time_flag : bool;
  profile : bool;
  (* Networking options *)
  metrics : Metrics.State.t;
  version_check : bool;
  (* Ugly: should be in separate subcommands *)
  dump_ast : dump_ast option;
  show_supported_languages : bool;
  test : bool;
  test_ignore_todo : bool;
  validate : bool;
  version : bool;
}

and rules_source =
  (* -e/-l/--replacement *)
  | Pattern of string * Xlang.t * string option (* replacement *)
  (* --config *)
  | Configs of string list

and dump_ast =
  | DumpPattern of string * Xlang.t
  | DumpTarget of Common.filename * Xlang.t
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
