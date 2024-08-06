module Out = Semgrep_output_v1_j

(* Display options *)
type conf = {
  (* mix of --json, --emacs, --vim, etc. *)
  output_format : Output_format.t;
  (* for Text *)
  max_chars_per_line : int;
  max_lines_per_finding : int;
  force_color : bool;
  (* for text and SARIF *)
  show_dataflow_traces : bool;
  (* misc *)
  strict : bool;
  (* a.k.a. dryrun in Scan_CLI.conf *)
  fixed_lines : bool;
  (* true when using --verbose or --debug in Scan_CLI.ml *)
  skipped_files : bool;
}
[@@deriving show]

val default : conf

(* Some parameters that are determined at runtime can also affect
 * the output. For example, if a user is not logged in, then in
 * the SARIF output format, we include a message to nudge the user
 * to log in and try Pro.
 *)
type runtime_params = { is_logged_in : bool; is_using_registry : bool }

(* Output the core results on stdout depending on flags in conf *)
val output_result :
  < Cap.stdout > ->
  conf ->
  runtime_params ->
  Profiler.t ->
  Core_runner.result ->
  Out.cli_output

(* helper used in output_result() and other callsites.
 * This handles nosemgrep, interpolating messages, and more.
 *)
val preprocess_result : fixed_lines:bool -> Core_runner.result -> Out.cli_output

(* used by RPC_return.ml for the vim and emacs formatter for now *)
val format : Output_format.t -> Out.cli_output -> string list
