(* main entry point *)
val check_rule :
  Rule.search_rule ->
  (Pattern_match.t list -> Pattern_match.t list) ->
  Match_env.xconfig ->
  Xtarget.t ->
  Core_profiling.rule_profiling Core_result.match_result

val hook_pro_entropy_analysis : (string -> bool) option ref

val hook_pro_metavariable_name :
  (Match_env.env -> AST_generic.expr -> Rule.metavar_name_kind -> bool) option
  ref
(** Determine whether a expression is a name of the given kind. *)

(* called from check_rule above and from Match_tainting_mode *)
val matches_of_formula :
  Match_env.xconfig ->
  Rule.rule ->
  Xtarget.t ->
  Rule.formula ->
  Range_with_metavars.t option ->
  Core_profiling.rule_profiling Core_result.match_result
  * Range_with_metavars.ranges
