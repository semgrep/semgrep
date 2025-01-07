(* The order of the functions in this file is mostly the order in which
 * the information is displayed to the user in the terminal and most functions
 * are called from Scan_subcommand.ml:
 *  - logo (Logs.app)
 *  - product selection (Logs.app)
 *  - rules source (Logs.app)
 *  - running rules (Logs.info)
 *  - roots, skipped part1, and selected targets (Log_targeting.Log.debug)
 *  - Code/SCA/Secret rules, language/origin, targets (Logs.app)
 *  - scan status (Logs.app) from Core_runner.ml this time
 *  # (Findings in Text_output.ml/Sarif_output.ml/...)
 *  - file skipped part2 (Logs.info)
 *  - scan summary (Logs.app)
 *)

val logo : string

val product_selection :
  includes_token:bool -> Rules_source.t -> Engine_type.t -> string

val rules_source : Rules_source.t -> string
val rules : too_many_entries:int -> Rules_source.t -> Rule.t list -> string

val targets :
  Scanning_root.t list ->
  Semgrep_output_v1_t.skipped_target list ->
  Fpath.t list ->
  string

val scan_status :
  num_rules:int ->
  num_targets:int ->
  respect_gitignore:bool ->
  Lang_job.t list ->
  string

(* findings in Text_output.ml/Sarif_output.ml/... *)

val skipped :
  too_many_entries:int ->
  respect_git_ignore:bool ->
  max_target_bytes:int ->
  Maturity.t ->
  Skipped_groups.t ->
  string

val scan_summary :
  respect_gitignore:bool ->
  max_target_bytes:int ->
  num_valid_rules:int ->
  Maturity.t ->
  Semgrep_output_v1_t.cli_output ->
  Skipped_groups.t ->
  string
