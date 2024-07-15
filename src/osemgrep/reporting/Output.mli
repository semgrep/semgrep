module OutJ = Semgrep_output_v1_j

type conf = {
  (* Display options *)
  (* mix of --json, --emacs, --vim, etc. *)
  output_format : Output_format.t;
  max_chars_per_line : int;
  max_lines_per_finding : int;
  (* maybe should define an Output_option.t, or add a record to
   * Output_format.Text *)
  force_color : bool;
  (* For text and SARIF *)
  show_dataflow_traces : bool;
  (* TODO: why strict part of an output conf? *)
  strict : bool;
  dryrun : bool;
  logging_level : Logs.level option;
}
[@@deriving show]

(* Some parameters that are determined at runtime can also affect
 * the output. For example, if a user is not logged in, then in
 * the SARIF output format, we include a message to nudge the user
 * to log in and try Pro.
 *)
type runtime_params = { is_logged_in : bool; is_using_registry : bool }

val default : conf

val preprocess_result : conf -> Core_runner.result -> OutJ.cli_output
(** This handles nosemgrep, interpolating messages, and more. *)

(* Output the core results on stdout depending on flags in
 * the configuration.
 *
 * ugly: this also apply autofixes depending on the configuration.
 *)
val output_result :
  < Cap.stdout > ->
  conf ->
  runtime_params ->
  Profiler.t ->
  Core_runner.result ->
  OutJ.cli_output
