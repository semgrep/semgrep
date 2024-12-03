(*
   'semgrep scan' (and also 'semgrep ci') command-line parsing.
*)

(*
   The result of parsing a 'semgrep scan' command.
*)
type conf = {
  (* Main configuration options *)
  (* mix of --pattern/--lang/--replacement, --config *)
  rules_source : Rules_source.t;
  (* the roots can be files, directories, or even symlinks *)
  target_roots : Scanning_root.t list;
  (* Rules/targets refinements *)
  rule_filtering_conf : Rule_filtering.conf;
  targeting_conf : Find_targets.conf;
  (* Other configuration options *)
  error_on_findings : bool;
  rewrite_rule_ids : bool;
  engine_type : Engine_type.t;
  autofix : bool;
  (* Performance options *)
  core_runner_conf : Core_runner.conf;
  (* file or URL (None means output to stdout) *)
  output : string option;
  output_conf : Output.conf;
  (* osemgrep-only: *)
  incremental_output : bool;
  (* Networking options *)
  metrics : Metrics_.config;
  version_check : bool;
  (* Debugging/logging/profiling options *)
  common : CLI_common.conf;
  trace : bool;
  (* TODO: use Uri.t *)
  trace_endpoint : string option;
  (* Ugly: should be in separate subcommands *)
  version : bool;
  show : Show_CLI.conf option;
  validate : Validate_CLI.conf option;
  test : Test_CLI.conf option;
  allow_local_builds : bool;
  (* --x-* options are experimental forever! (= subject to change or removal
     without notice) *)
  (* --x-ls: *)
  ls : bool;
  (* --x-ls-long: *)
  ls_format : Ls_subcommand.format;
}
[@@deriving show]

(* Command-line defaults. *)
val default : conf

(*
   Usage: parse_argv [| "semgrep-scan"; <args> |]

   Turn argv into a conf structure.

   This function may raise an exn in case of an error parsing argv
   but this should be caught by CLI.safe_run.

   ugly: need to pass Cap.tmp to support named pipes
   TODO: would be better to do that in Scan_subcommand.ml instead, after
   parsing the arguments.
*)
val parse_argv : < Cap.tmp > -> string array -> conf

(* exported because used by Interactive_CLI.ml too (and some by Ci_CLI.ml) *)
val o_lang : string option Cmdliner.Term.t
val o_target_roots : string list Cmdliner.Term.t
val o_include : string list Cmdliner.Term.t
val o_exclude : string list Cmdliner.Term.t
val o_secrets : bool Cmdliner.Term.t

(* exported because used by Ci_CLI.ml *)
val o_allow_local_builds : bool Cmdliner.Term.t
val o_allow_untrusted_validators : bool Cmdliner.Term.t
val o_autofix : bool Cmdliner.Term.t
val o_baseline_commit : string option Cmdliner.Term.t
val o_dataflow_traces : bool Cmdliner.Term.t
val o_diff_depth : int Cmdliner.Term.t
val o_dryrun : bool Cmdliner.Term.t
val o_dump_command_for_core : bool Cmdliner.Term.t
val o_emacs : bool Cmdliner.Term.t
val o_emacs_outputs : string list Cmdliner.Term.t
val o_exclude_minified_files : bool Cmdliner.Term.t
val o_exclude_rule_ids : string list Cmdliner.Term.t
val o_files_with_matches : bool Cmdliner.Term.t
val o_force_color : bool Cmdliner.Term.t
val o_gitlab_sast : bool Cmdliner.Term.t
val o_gitlab_sast_outputs : string list Cmdliner.Term.t
val o_gitlab_secrets : bool Cmdliner.Term.t
val o_gitlab_secrets_outputs : string list Cmdliner.Term.t
val o_historical_secrets : bool Cmdliner.Term.t
val o_ignore_semgrepignore_files : bool Cmdliner.Term.t
val o_incremental_output : bool Cmdliner.Term.t
val o_json : bool Cmdliner.Term.t
val o_json_outputs : string list Cmdliner.Term.t
val o_junit_xml : bool Cmdliner.Term.t
val o_junit_xml_outputs : string list Cmdliner.Term.t
val o_matching_explanations : bool Cmdliner.Term.t
val o_max_chars_per_line : int Cmdliner.Term.t
val o_max_lines_per_finding : int Cmdliner.Term.t
val o_max_log_list_entries : int Cmdliner.Term.t
val o_max_memory_mb : int Cmdliner.Term.t
val o_max_target_bytes : int Cmdliner.Term.t
val o_metrics : Metrics_.config Cmdliner.Term.t
val o_num_jobs : int Cmdliner.Term.t
val o_no_secrets_validation : bool Cmdliner.Term.t
val o_nosem : bool Cmdliner.Term.t
val o_optimizations : bool Cmdliner.Term.t
val o_oss : bool Cmdliner.Term.t
val o_output : string option Cmdliner.Term.t
val o_pro : bool Cmdliner.Term.t
val o_pro_intrafile : bool Cmdliner.Term.t
val o_pro_languages : bool Cmdliner.Term.t
val o_pro_path_sensitive : bool Cmdliner.Term.t
val o_rewrite_rule_ids : bool Cmdliner.Term.t
val o_sarif : bool Cmdliner.Term.t
val o_sarif_outputs : string list Cmdliner.Term.t
val o_scan_unknown_extensions : bool Cmdliner.Term.t
val o_test : bool Cmdliner.Term.t
val o_text : bool Cmdliner.Term.t
val o_text_outputs : string list Cmdliner.Term.t
val o_time : bool Cmdliner.Term.t
val o_timeout : float Cmdliner.Term.t
val o_timeout_interfile : int Cmdliner.Term.t
val o_timeout_threshold : int Cmdliner.Term.t
val o_trace : bool Cmdliner.Term.t
val o_trace_endpoint : string option Cmdliner.Term.t
val o_use_git : bool Cmdliner.Term.t
val o_version_check : bool Cmdliner.Term.t
val o_vim : bool Cmdliner.Term.t
val o_vim_outputs : string list Cmdliner.Term.t

val engine_type_conf :
  oss:bool ->
  pro_lang:bool ->
  pro_intrafile:bool ->
  pro:bool ->
  secrets:bool ->
  no_secrets_validation:bool ->
  allow_untrusted_validators:bool ->
  pro_path_sensitive:bool ->
  Engine_type.t

val output_format_conf :
  text:bool ->
  files_with_matches:bool ->
  json:bool ->
  emacs:bool ->
  vim:bool ->
  sarif:bool ->
  gitlab_sast:bool ->
  gitlab_secrets:bool ->
  junit_xml:bool ->
  Output_format.t

val outputs_conf :
  text_outputs:'a list ->
  json_outputs:'a list ->
  emacs_outputs:'a list ->
  vim_outputs:'a list ->
  sarif_outputs:'a list ->
  gitlab_sast_outputs:'a list ->
  gitlab_secrets_outputs:'a list ->
  junit_xml_outputs:'a list ->
  ('a option, Output_format.t) Map_.t
