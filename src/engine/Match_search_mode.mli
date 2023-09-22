(* main entry point *)
val check_rule :
  Rule.search_rule ->
  (string -> Pattern_match.t -> unit) ->
  Match_env.xconfig ->
  Xtarget.t ->
  Core_profiling.rule_profiling Core_result.match_result

val hook_pro_entropy_analysis : (string -> bool) option ref

(* called from check_rule above and from Match_tainting_mode *)
val matches_of_formula :
  Match_env.xconfig ->
  Rule.rule ->
  Xtarget.t ->
  Rule.formula ->
  Range_with_metavars.t option ->
  Core_profiling.rule_profiling Core_result.match_result
  * Range_with_metavars.ranges
