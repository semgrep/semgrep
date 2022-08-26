(* main entry point *)
val check_rule :
  Rule.search_rule ->
  (string -> Pattern_match.t -> unit) ->
  Match_env.xconfig ->
  Xtarget.t ->
  Report.rule_profiling Report.match_result
  * Match_tainting_mode.taint_info list

(* called from check_rule above and from Match_tainting_mode *)
val matches_of_formula :
  Match_env.xconfig ->
  Rule.rule ->
  Xtarget.t ->
  Rule.formula ->
  Range_with_metavars.t option ->
  Report.rule_profiling Report.match_result
  * Range_with_metavars.ranges
  * Match_tainting_mode.taint_info list
