val check_rule :
  Rule.t ->
  (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t * Equivalence.equivalences ->
  Rule.pformula ->
  Xtarget.t ->
  Report.rule_profiling Report.match_result

val matches_of_formula :
  Config_semgrep_t.t * Equivalence.equivalences ->
  Rule.rule ->
  Xtarget.t ->
  Rule.formula ->
  Range_with_metavars.t option ->
  Report.rule_profiling Report.match_result * Range_with_metavars.ranges
