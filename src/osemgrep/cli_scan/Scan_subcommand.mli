(*
   Parse a semgrep-scan command, execute it and exit.

   Usage: main [| "semgrep-scan"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : string array -> Exit_code.t

(* internal *)
val run_conf : Scan_CLI.conf -> Exit_code.t
val run_scan_conf : Scan_CLI.conf -> Exit_code.t

(* This option is designed for the differential scan within the Pro
   engine. It involves two distinct sets of input files: one for the
   entire input set and the other for the differential input set,
   which is defined using the `diff_targets` parameter. In this
   context, the deep preprocessors, including constant propagation and
   taint signature extraction, exclusively process the files found in
   the differential input set and their related dependencies. This
   targeted approach substantially cuts down the overall analysis time
   while upholding a high level of accuracy. The dependencies are
   identified through a file-level dependency graph with a maximum
   distance of k edges, as determined by the `diff_depth`
   parameter. Note that, when the `--baseline-commit` option is given,
   `diff_targets` are automatically calculated based on the output
   from the `git diff` command between the baseline commit and the
   head commit. *)
type diff_config = { diff_targets : Fpath.t list; diff_depth : int option }

val default_diff_config : diff_config

(* Semgrep Pro hook *)
(* TODO it might be better to pass this through and avoid the hook,
   but it was fairly annoying to *)
val invoke_semgrep_core_proprietary :
  (Fpath.t list ->
  ?diff_config:diff_config ->
  Engine_type.t ->
  Core_runner.semgrep_core_runner)
  option
  ref

(* internal: scan all the files - also used in CI *)
val run_scan_files :
  Scan_CLI.conf ->
  Profiler.t ->
  Rule_fetching.rules_and_origin list ->
  Fpath.t list * Semgrep_output_v1_t.skipped_target list ->
  ( Rule.rule list * Core_runner.result * Semgrep_output_v1_t.cli_output,
    Exit_code.t )
  result
