(*
   This calls the semgrep-core command like the Python implementation used
   to, but without creating a subprocess unless necessary.

   This module should go away, eventually, with some parts being integrated
   into what's currently semgrep-core.
*)

type path = string

type result = {
  findings_by_rule : (Rule.t, Rule_match.t list) Map_.t;
  errors : Error.t list;
  all_targets : path Set_.t;
  parsing_data : Parsing_data.t;
  explanations : Semgrep_output_v0_t.matching_explanation list option;
}

val invoke_semgrep : Scan_CLI.conf -> result
