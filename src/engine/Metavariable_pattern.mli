(* The first argument is the function
 * Match_search_mode.nested_formula_has_matches which is passed to break
 * mutual recursivity between Metavariable_pattern and Match_search_mode
 * as handling metavariable-pattern: requires nested search.
 *
 * This function returns a list of all the nonempty new bindings introduced
 * by  the `metavariable-pattern`, for each instance of the match.
 *)
val get_nested_metavar_pattern_bindings :
  (Match_env.env ->
  Formula_internal.formula ->
  Range_with_metavars.t ->
  Range_with_metavars.t list) ->
  Match_env.env ->
  Range_with_metavars.t ->
  (* The arguments in CondNestedFormula *)
  Metavariable.mvar ->
  Xlang.t option ->
  Formula_internal.formula ->
  Metavariable.bindings list
