module OutJ = Semgrep_output_v1_j

type conf = {
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

val default : conf

val preprocess_result : conf -> Core_runner.result -> OutJ.cli_output
(** [preprocess_result conf result] preprocesses the result of a scan
  * according to the configuration [conf]. This handles
  * nosemgrep, interolating messages, and more. It returns a function that
  * will output the result, so it can be timed.
  *)

(* Output the core results on stdout depending on flags in
 * the configuration:
 *  - Json
 -  - Vim
 *  - Emacs
 *  - TODO Text
 *  - TODO Sarif
 *  - TODO ...
 *
 * ugly: this also apply autofixes depending on the configuration.
 *)
val output_result : conf -> Profiler.t -> Core_runner.result -> OutJ.cli_output
