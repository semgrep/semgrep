(* main entry point *)
val check_rule :
  Rule.search_rule ->
  (string ->
  Metavariable.bindings ->
  Parse_info.t list Lazy.t ->
  _ option ->
  unit) ->
  Config_semgrep.t * Equivalence.equivalences ->
  Xtarget.t ->
  Report.rule_profiling Report.match_result

(* called from check_rule above and from Match_tainting_mode *)
val matches_of_formula :
  Config_semgrep_t.t * Equivalence.equivalences ->
  Rule.rule ->
  Xtarget.t ->
  Rule.formula ->
  Range_with_metavars.t option ->
  Report.rule_profiling Report.match_result * Range_with_metavars.ranges
