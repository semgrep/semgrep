(* LATER: This module should disappear once osemgrep is done *)

(* Main entry point to the semgrep-core scan. This is called from
 * Core_CLI.ml, called itself from Main.ml
 *)
val run_conf : < Cap.stdout ; Cap.tmp ; Cap.exit > -> Core_scan_config.t -> unit

val output_core_results :
  < Cap.stdout ; Cap.exit > ->
  Core_result.result_or_exn ->
  Core_scan_config.t ->
  unit
(** [output_core_results] takes the results of a core scan and
    format the results on stdout either in a JSON or Textual format
    (depending on the value in config.output_format)
*)
