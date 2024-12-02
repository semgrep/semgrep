(* main entry point *)
val check_rule :
  Rule.search_rule ->
  (Core_match.t list -> Core_match.t list) ->
  Match_env.xconfig ->
  Xtarget.t ->
  Core_profiling.rule_profiling Core_result.match_result

val hook_pro_entropy_analysis :
  (mode:Rule.entropy_analysis_mode -> string -> bool) option ref

val hook_pro_metavariable_name :
  (AST_generic.expr -> Rule.metavar_cond_name -> bool) option ref
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
