(* input *)
type core_runner_conf = {
  (* opti and limits *)
  num_jobs : int;
  optimizations : bool;
  max_memory_mb : int;
  timeout : float;
  timeout_threshold : int; (* output flags *)
  nosem : bool;
  strict : bool;
  time_flag : bool;
  matching_explanations : bool;
  dataflow_traces : bool;
}
[@@deriving show]

(* output *)
type core_runner_result = {
  core : Semgrep_output_v1_t.core_output;
  hrules : Rule.hrules;
  scanned : Fpath.t Set_.t;
}

val create_core_result :
  Rule.rule list -> Core_result.result_or_exn -> core_runner_result

type output_conf = {
  nosem : bool;
  autofix : bool;
  dryrun : bool;
  strict : bool;
  (* maybe should define an Output_option.t, or add a record to
   * Output_format.Text *)
  force_color : bool;
  logging_level : Logs.level option;
  (* Display options *)
  (* mix of --json, --emacs, --vim, etc. *)
  output_format : Output_format.t;
  max_chars_per_line : int;
  max_lines_per_finding : int;
}
[@@deriving show]

val default_output_conf : output_conf

val preprocess_core_runner_result :
  output_conf -> core_runner_result -> Semgrep_output_v1_t.cli_output
(** [preprocess_result conf result] preprocesses the result of a scan
  * according to the configuration [conf]. This handles
  * nosemgrep, interpolating messages, and more.
  *)
