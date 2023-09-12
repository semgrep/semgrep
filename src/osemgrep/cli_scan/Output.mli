module Out = Semgrep_output_v1_j

val preprocess_result :
  Scan_CLI.conf -> Core_runner.result -> unit -> Out.cli_output
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
val output_result :
  Scan_CLI.conf -> Profiler.t -> Core_runner.result -> Out.cli_output
