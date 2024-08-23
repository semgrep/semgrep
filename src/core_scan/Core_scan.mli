(* The type of the semgrep "core" scan. We define it here so that
   semgrep and semgrep-proprietary use the same definition *)
type func = Core_scan_config.t -> Core_result.result_or_exn

(* Entry point. This is used in Core_command.ml for semgrep-core,
 * in tests, in semgrep-pro, and finally in osemgrep.
 *
 * [scan caps config] runs a core scan with a fixed list of targets
 * and rules and capture any exception.
 * This internally calls Match_rules.check() on every files, in
 * parallel, with some memory limits, and aggregate the results.
 *
 * It can print things on stdout depending on Core_scan_config.output_format:
 *  - incremental dots when used from pysemgrep in Json true mode
 *  - incremental matches when used from semgrep-core in Text mode
 *  - nothing when called from osemgrep (or the playground), unless
 *    file_match_hook is also set in which case it can display incremental
 *    matches too
 * The rest of the output is done in the caller of scan() such as
 * Core_command.run_conf() for semgrep-core.
 *
 * alt: we should require Cap.stdout below, but this is false when using the
 * NoOutput output_format so for now we internally use Cap.stdout_caps_UNSAFE()
 * or UConsole. In theory, scan() can be completely pure.
 *
 * TODO: we should require Cap.fork (for Parmap), Cap.alarm (for timeout
 * in Check_rules()), and more.
 *
 * The scan function has the type [func] defined above.
 *
 * Note that this function will run the pre/post scan hook defined
 * in Pre_post_core_scan.hook_processor.
 *)
val scan :
  < (* no caps needed for now *) > ->
  Core_scan_config.t ->
  Core_result.result_or_exn

(*****************************************************************************)
(* Utilities functions used in tests or semgrep-pro *)
(*****************************************************************************)

val rules_from_rule_source :
  Core_scan_config.rule_source -> Rule_error.rules_and_invalid
(** Get the rules *)

val targets_of_config :
  Core_scan_config.t -> Target.t list * Semgrep_output_v1_t.skipped_target list
(**
  Compute the set of targets, either by reading what was passed
  in -target, or by using Find_target.files_of_dirs_or_files.
  TODO: replace by just 'Get the targets'
 *)

(* This is also used by semgrep-proprietary. It filters the rules that
   apply to a given target file for a given analyzer.
   It takes into account the analyzer (specified by 'languages' field)
   and the per-rule include/exclude patterns; possibly more in the future.
*)
val rules_for_target :
  analyzer:Xlang.t ->
  products:Semgrep_output_v1_t.product list ->
  origin:Origin.t ->
  respect_rule_paths:bool ->
  Rule.t list ->
  Rule.t list

(* This is used only by semgrep-proprietary.
   Compare to select_applicable_rules_for_target which additionally can
   honor per-rule include/exclude patterns based on the target path.
*)
val rules_for_analyzer : analyzer:Xlang.t -> Rule.t list -> Rule.t list

(* This function prints a dot, which is consumed by pysemgrep to update
   the progress bar if the output_format is Json true.
   See also `core_runner.py`
*)
val print_cli_progress : Core_scan_config.t -> unit

(* This function prints the number of additional targets, which is consumed by
   pysemgrep to update the progress bar, if the output_format is Json true.
   This was used by extract-mode (TODO still useful?).
   See `core_runner.py`
*)
val print_cli_additional_targets : Core_scan_config.t -> int -> unit

(* This function print matches incrementally when the output_format is Text *)
val print_incremental_matches_when_text_mode :
  Core_scan_config.t -> Pattern_match.t -> unit

val filter_files_with_too_many_matches_and_transform_as_timeout :
  int ->
  Core_result.processed_match list ->
  Core_result.processed_match list
  * Core_error.t list
  * Semgrep_output_v1_j.skipped_target list

val parse_equivalences : Fpath.t option -> Equivalence.equivalences

(* small wrapper around Parse_target.parse_and_resolve_name *)
val parse_and_resolve_name :
  Lang.t -> Fpath.t -> AST_generic.program * Tok.location list

val log_scan_inputs :
  Core_scan_config.t ->
  targets:'a list ->
  skipped:'b list ->
  valid_rules:'c list ->
  invalid_rules:'d list ->
  unit

val log_scan_results :
  Core_scan_config.t -> Core_result.t -> skipped_targets:'a list -> unit

(* DO NOT USE *)
val targets_of_config_DEPRECATED :
  Core_scan_config.t -> Target.t list * Semgrep_output_v1_t.skipped_target list
