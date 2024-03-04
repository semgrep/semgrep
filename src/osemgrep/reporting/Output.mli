module OutJ = Semgrep_output_v1_j

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
  Core_to_cli.output_conf ->
  Profiler.t ->
  is_logged_in:bool ->
  Core_to_cli.core_runner_result ->
  OutJ.cli_output
