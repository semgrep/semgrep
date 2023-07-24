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
  core : Semgrep_output_v1_t.core_match_results;
  hrules : Rule.hrules;
  scanned : Fpath.t Set_.t;
}

type semgrep_core_runner =
  ?respect_git_ignore:bool ->
  ?file_match_results_hook:
    (Fpath.t -> Report.partial_profiling Report.match_result -> unit) option ->
  conf ->
  (* LATER? use Config_resolve.rules_and_origin instead? *)
  Rule.rules ->
  Rule.invalid_rule_error list ->
  Fpath.t list ->
  result

(*
   This calls the semgrep-core command like the Python implementation used
   to, but without creating a subprocess.

   LATER: This function should go away, eventually, with some parts being
   integrated into what's currently semgrep-core.
*)
val invoke_semgrep_core :
  ?engine:Runner_config.semgrep_engine -> semgrep_core_runner

(* Helper used in Semgrep_scan.ml to setup logging *)
val runner_config_of_conf : conf -> Runner_config.t
