(*
   This calls the semgrep-core command like the Python implementation used
   to, but without creating a subprocess.

   LATER: This module should go away, eventually, with some parts being
   integrated into what's currently semgrep-core.
*)

type path = string

type result = {
  core : Semgrep_output_v1_t.core_match_results;
  hrules : Rule.hrules;
  scanned : path Set_.t;
}

val invoke_semgrep_core : Scan_CLI.conf -> Rule.rules -> path list -> result

(* Helper used in Semgrep_scan.ml *)
val runner_config_of_conf : Scan_CLI.conf -> Runner_config.t
