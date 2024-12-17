(* The order of the functions in this file is mostly the order in which
 * the information is displayed to the user in the terminal:
 *  - running rules (Logs.info)
 *  - roots, skipped, and selected targets (Log_targeting.Log.debug)
 *  - SEMI Code/SCA/Secret rules, language/origin, targets (Logs.app)
 *  # (Findings in Text_output.ml/Sarif_output.ml/...)
 *  - file skipped (Logs.info)
 *  - scan summary (Logs.app)
 *)

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
