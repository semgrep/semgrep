(* The first argument is the function
 * Match_search_mode.nested_formula_has_matches which is passed to break
 * mutual recursivity between Metavariable_pattern and Match_search_mode
 * as handling metavariable-pattern: requires nested search.
 *)
val satisfies_metavar_pattern_condition :
  (Match_env.env -> Rule.formula -> Range_with_metavars.t option -> bool) ->
  Match_env.env ->
  Range_with_metavars.t ->
  (* The arguments in CondNestedFormula *)
  Metavariable.mvar ->
  Xlang.t option ->
  Rule.formula ->
  bool
