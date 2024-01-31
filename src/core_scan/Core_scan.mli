(* The type of the semgrep "core" scan. We define it here so that
   semgrep and semgrep-proprietary use the same definition *)
type core_scan_func = Core_scan_config.t -> Core_result.result_or_exn

(* Entry point. This is used in Core_command.ml for semgrep-core,
 * in tests, in semgrep-pro, and finally in osemgrep.
 *
 * [scan_with_exn_handler config] runs a core scan with a starting list
 * of targets and capture any exception.
 * This internally calls Match_rules.check on every files, in
 * parallel, with some memory limits, and aggregate the results.
 *
 * This has the type core_scan_func defined above.
 *
 * Note that this function will run the pre/post scan hook defined
 * in Pre_post_core_scan.hook_processor.
 *)

val scan_with_exn_handler : Core_scan_config.t -> Core_result.result_or_exn

(* As opposed to [scan_with_exn_handler()], [scan() ...] below may throw
 * an exception (for example in case of a fatal error).
 *
 * This is called by scan_with_exn_handler(). This also uses
 * a few hooks that can be defined in semgrep-pro:
 *  - Match_tainting_mode.hook_setup_hook_function_taint_signature
 *  - Dataflow_tainting.hook_function_taint_signature
 *
 * Functions from matching/ and engine/ called internally uses even
 * more hooks to enhance the behavior of a "core scan".
 *
 * The match_hook parameter is a deprecated way to print matches. If not
 * provided, it defaults to a function that internally calls
 * print_match() below.
 *)
val scan :
  ?match_hook:(string -> Pattern_match.t -> unit) ->
  Core_scan_config.t ->
  (Rule.t list * Rule.invalid_rule_error list) * float ->
  Core_result.t

(* Old hook to support incremental display of matches for semgrep-core
 * in text-mode. Deprecated. Use Core_scan_config.file_match_results_hook
 * instead now with osemgrep.
 *)
val print_match :
  ?str:string ->
  Core_scan_config.t ->
  Pattern_match.t ->
  (Metavariable.mvalue -> Tok.t list) ->
  unit

(*****************************************************************************)
(* Utilities functions used in tests or semgrep-pro *)
(*****************************************************************************)

val rules_from_rule_source :
  Core_scan_config.t -> Rule.rules * Rule.invalid_rule_error list
(** Get the rules *)

val targets_of_config :
  Core_scan_config.t ->
  Input_to_core_t.targets * Semgrep_output_v1_t.skipped_target list
(**
  Compute the set of targets, either by reading what was passed
  in -target, or by using Find_target.files_of_dirs_or_files.
  The rule ids argument is useful only when you don't use -target.
 *)

val extracted_targets_of_config :
  Core_scan_config.t ->
  Rule.extract_rule list ->
  Input_to_core_t.code_target list ->
  Input_to_core_t.target list * Extract.adjusters
(**
   Generate a list of new targets, which are extracted with extract rules
   from original targets. This returns also "adjusters" which are functions
   to map back matches on extracted targets to matches on the original target.
*)

(* This is also used by semgrep-proprietary. It filters the rules that
   apply to a given target file for a given analyzer.
   It takes into account the analyzer (specified by 'languages' field)
   and the per-rule include/exclude patterns; possibly more in the future.
*)
val select_applicable_rules_for_target :
  analyzer:Xlang.t ->
  products:Semgrep_output_v1_t.product list ->
  path:Fpath.t ->
  respect_rule_paths:bool ->
  Rule.t list ->
  Rule.t list

(* This is used only by semgrep-proprietary.
   Compare to select_applicable_rules_for_target which additionally can
   honor per-rule include/exclude patterns based on the target path.
*)
val select_applicable_rules_for_analyzer :
  analyzer:Xlang.t -> Rule.t list -> Rule.t list

(* This function prints the number of additional targets, which is consumed by
   pysemgrep to update the progress bar. See `core_runner.py`
*)
val print_cli_additional_targets : Core_scan_config.t -> int -> unit

(* This function prints a dot, which is consumed by pysemgrep to update
   the progress bar. See `core_runner.py`
*)
val print_cli_progress : Core_scan_config.t -> unit

(* used internally but also called by osemgrep *)
val errors_of_invalid_rule_errors :
  Rule.invalid_rule_error list -> Core_error.t list

val replace_named_pipe_by_regular_file : Fpath.t -> Fpath.t
(* Small wrapper around File.replace_named_pipe_by_regular_file_if_needed.
   Any file coming from the command line should go through this so as to
   allows easy manual testing.
*)

val filter_files_with_too_many_matches_and_transform_as_timeout :
  int ->
  Core_result.processed_match list ->
  Core_result.processed_match list
  * Core_error.t list
  * Semgrep_output_v1_j.skipped_target list

val xtarget_of_file :
  parsing_cache_dir:Fpath.t option -> Xlang.t -> Fpath.t -> Xtarget.t

(*
   Sort targets by decreasing size. This is meant for optimizing
   CPU usage when processing targets in parallel on a fixed number of cores.
*)
val sort_targets_by_decreasing_size :
  Input_to_core_t.target list -> Input_to_core_t.target list

val parse_equivalences : Fpath.t option -> Equivalence.equivalences
