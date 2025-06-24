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
  (* Used when displaying rule ids or skipped files. If above the limit,
   * the entries will not be displayed and replaced by a <SKIPPED DATA>
   * in the log output.
   *)
  max_log_list_entries : int;
}
[@@deriving show]

val default : conf

(* used with max_log_list_entries *)
val too_much_data : string

(* Output the Semgrep result (matches, errors, etc.) on stdout depending on
 * flags in conf (and also return an Out.cli_output for further processing in
 * the caller).
 *
 * The format_context contains fields that are determined at runtime and
 * which can also affect the output. For example, if a user is not logged in
 * then in the SARIF output format we include a message to nudge the user
 * to log in and try Pro.
 *)
val output_result :
  < Cap.stdout > ->
  (* derived from CLI flags *)
  conf ->
  (* derived from runtime info *)
  Out.format_context ->
  Profiler.t ->
  Core_runner_result.t ->
  Out.cli_output

(* helper used in output_result() and other callsites.
 * This handles nosemgrep, interpolating messages, and more.
 *)
val preprocess_result :
  fixed_lines:bool -> Core_runner_result.t -> Out.cli_output

(* Called by pysemgrep via RPC for Vim/Emacs/Junit_xml/Gitlab_xxx formats *)
val format :
  Output_format.t -> Out.format_context -> Out.cli_output -> string list
