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
  (* can be a list of files or directories *)
  target_roots : Fpath.t list;
  (* Rules/targets refinements *)
  rule_filtering_conf : Rule_filtering.conf;
  targeting_conf : Find_targets.conf;
  (* Other configuration options *)
  error_on_findings : bool;
  rewrite_rule_ids : bool;
  engine_type : Engine_type.t;
  (* Performance options *)
  core_runner_conf : Core_runner.conf;
  (* file or URL (None means output to stdout) *)
  output : string option;
  output_conf : Output.conf;
  (* osemgrep-only: *)
  incremental_output : bool;
  (* text output config (TODO: make a separate type gathering all of them)
   * or add them under Output_format.Text
   *)
  (* Networking options *)
  metrics : Metrics_.config;
  version_check : bool;
  common : CLI_common.conf;
  (* Ugly: should be in separate subcommands *)
  version : bool;
  show : Show_CLI.conf option;
  validate : Validate_subcommand.conf option;
  test : Test_CLI.conf option;
  trace : bool;
  ls : bool;
}
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
val cmdline_term : allow_empty_config:bool -> conf Cmdliner.Term.t

(* exported because used by Interactive_CLI.ml too *)
val o_lang : string option Cmdliner.Term.t
val o_target_roots : string list Cmdliner.Term.t
val o_include : string list Cmdliner.Term.t
val o_exclude : string list Cmdliner.Term.t
