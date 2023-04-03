type t = Semgrep_output_v1_t.core_match * Rule.rule

val postprocess_results :
  Report.final_result -> Rule.hrules -> string list -> t list * string list
