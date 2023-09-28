(* input *)
type conf = {
  num_jobs : int;
  optimizations : bool;
  max_memory_mb : int;
  timeout : float;
  timeout_threshold : int;
  (* osemgrep-only: *)
  ast_caching : bool;
}
[@@deriving show]

(* output *)
type result = {
  core : Semgrep_output_v1_t.core_output;
  hrules : Rule.hrules;
  scanned : Fpath.t Set_.t;
}

val create_core_result : Rule.rule list -> Core_result.result_or_exn -> result

(* similar to Core_scan.core_scan_func *)
type scan_func_for_osemgrep =
  ?respect_git_ignore:bool ->
  ?file_match_results_hook:
    (Fpath.t ->
    Core_profiling.partial_profiling Core_result.match_result ->
    unit)
    option ->
  conf ->
  (* LATER? use Config_resolve.rules_and_origin instead? *)
  Rule.rules ->
  Rule.invalid_rule_error list ->
  Fpath.t list ->
  Core_result.result_or_exn

(* Core_scan_func adapter to be used in osemgrep.

   This will eventually call a core scan like pysemgrep but without
   creating a subprocess.

   The first argument is usually Core_scan.scan_with_exn_handler,
   but it can also be Run.deep_with_raw_results_and_exn_handler
   when running in Pro Interfile mode and when called from
   the Steps_runner in Semgrep Pro.

   LATER: This function should go away.
*)
val mk_scan_func_for_osemgrep :
  Core_scan.core_scan_func -> scan_func_for_osemgrep

(* Helper used in Semgrep_scan.ml to setup logging *)
val core_scan_config_of_conf : conf -> Core_scan_config.t
