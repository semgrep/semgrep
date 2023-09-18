(* LATER: This module should disappear once osemgrep is done *)

(* Main entry point to the semgrep-core scan. This is called from
 * Core_CLI.ml, called itself from Main.ml
 *)
val semgrep_core_dispatch : Core_scan_config.t -> unit

val semgrep_core_with_one_pattern : Core_scan_config.t -> unit
(** this is the function used when running semgrep-core with -e or -f *)

val semgrep_core_with_rules_and_formatted_output : Core_scan_config.t -> unit
(** [semgrep_core_with_rules_and_formatted_output config] calls
    [scan_with_exn_handler] and then [output_core_results] on the results
    This is the function used when running semgrep-core with -rules.
*)

val output_core_results :
  Core_result.result_and_exn -> Core_scan_config.t -> unit
(** [output_core_results] takes the results of a core scan and
    format the results on stdout either in a JSON or Textual format
    (depending on the value in config.output_format)
*)
