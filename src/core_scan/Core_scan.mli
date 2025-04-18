(* The type of the semgrep "core" scan. We define it here so that
   semgrep and semgrep-proprietary use the same definition *)
type func = Core_scan_config.t -> Core_result.result_or_exn

(* See the comment for scan() below explaining the need for those capabilities *)
type caps = < Cap.fork ; Cap.time_limit ; Cap.memory_limit ; Cap.readdir >

(* Entry point. This is used in Core_CLI.ml for semgrep-core, in tests,
 * and finally in osemgrep.
 *
 * [scan caps config] runs a core scan with a fixed list of targets
 * and rules and capture any exception.
 * This internally calls Match_rules.check() on every files, in
 * parallel, with some time and memory limits, and aggregate the results
 * (hence the need for Cap.fork, Cap.time_limit, Cap.memory_limit above).
 *
 * It can print things on stdout depending on Core_scan_config.output_format:
 *  - incremental dots when used from pysemgrep in Json true mode
 *  - incremental matches when used from semgrep-core in Text mode
 *  - nothing when called from osemgrep (or the playground), unless
 *    file_match_hook is also set in which case it can display incremental
 *    matches too
 * The rest of the output is done in the caller of scan() such as
 * Core_CLI.main_exn() for semgrep-core with Core_CLI.output_core_results().
 *
 * alt: we should require Cap.stdout above, but this is false when using the
 * NoOutput output_format so for now we internally use Cap.stdout_caps_UNSAFE()
 * or UConsole. In theory, scan() can be completely pure.
 *
 * We require Cap.fork for Parmap.
 * We require Cap.time_limit for timeout in Check_rules().
 * We require Cap.readdir for ??
 *
 * The scan function has the type [func] defined above.
 *)
val scan : < caps ; .. > -> Core_scan_config.t -> Core_result.result_or_exn

(*****************************************************************************)
(* Utilities functions used in tests or semgrep-pro *)
(*****************************************************************************)

(* Compute the set of targets, either by reading what was passed
   in -target, or passed explicitly in Core_scan_config.Targets.
   The rules are required to associate analyzers (language specified
   in the rule) with target paths as reflected by the Target.t type.
*)
val targets_of_config :
  Core_scan_config.t ->
  Rule.t list ->
  Target.t list * Core_error.t list * Semgrep_output_v1_t.skipped_target list

(* pysemgrep/semgrep-core RPC *)
val get_targets_for_pysemgrep :
  Semgrep_output_v1_t.scanning_roots ->
  Semgrep_output_v1_t.target_discovery_result

(* Get the rules *)
val rules_of_config : Core_scan_config.t -> Rule_error.rules_and_invalid

(* Get the rules, using targeting info in config to filter irrelevant rules *)
val applicable_rules_of_config :
  Core_scan_config.t -> Rule_error.rules_and_invalid

(* This is also used by semgrep-proprietary. It filters the rules that
   apply to a given target file for a given analyzer.
   It takes into account the analyzer (specified by 'languages' field)
   and the per-rule include/exclude patterns; possibly more in the future.
*)
val rules_for_target :
  combine_js_with_ts:bool ->
  respect_rule_paths:bool ->
  Target.t ->
  Rule.t list ->
  Rule.t list

(* This is used only by semgrep-proprietary.
   Compare to select_applicable_rules_for_target which additionally can
   honor per-rule include/exclude patterns based on the target path.
*)
val rules_for_analyzer :
  combine_js_with_ts:bool -> Analyzer.t -> Rule.t list -> Rule.t list

(* exposed for SCA_scan *)
val origin_satisfy_paths_filter : Origin.t -> Rule.paths -> bool

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

type target_handler = Target.t -> Core_result.matches_single_file * bool

val iter_targets_and_get_matches_and_exn_to_errors :
  < Cap.fork ; Cap.memory_limit > ->
  Core_scan_config.t ->
  target_handler ->
  Target.t list ->
  Core_result.matches_single_file_with_time list * Target.t list

val filter_files_with_too_many_matches_and_transform_as_timeout :
  int ->
  Core_result.processed_match list ->
  Core_result.processed_match list
  * Core_error.t list
  * Semgrep_output_v1_j.skipped_target list

type post_processor =
  Core_result.processed_match -> Core_result.processed_match * Core_error.t list

val post_process_matches : post_processor -> Core_result.t -> Core_result.t
val post_autofix : post_processor
val post_nosemgrep : strict:bool -> post_processor

(* small wrapper around Parse_target.parse_and_resolve_name *)
val parse_and_resolve_name :
  Lang.t -> Fpath.t -> AST_generic.program * Tok.location list

(* small wrapper around Match_rules.check *)
val match_rules :
  < Cap.time_limit ; .. > ->
  matches_hook:(Core_match.t list -> Core_match.t list) ->
  Core_scan_config.t ->
  Match_env.prefilter_config ->
  Rule.t list ->
  Xtarget.t ->
  Core_result.matches_single_file

val log_scan_inputs :
  Core_scan_config.t ->
  targets:_ list ->
  errors:_ list ->
  skipped:_ list ->
  valid_rules:_ list ->
  invalid_rules:_ list ->
  unit

val log_scan_results :
  Core_scan_config.t ->
  Core_result.t ->
  scanned_targets:'a list ->
  skipped_targets:'b list ->
  unit
