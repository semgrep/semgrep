(* The first argument is the function
 * Match_search_mode.nested_formula_has_matches which is passed to break
 * mutual recursivity between Metavariable_pattern and Match_search_mode
 * as handling metavariable-pattern: requires nested search.
 *
 * This function returns a list of all the nonempty new bindings introduced
 * by  the `metavariable-pattern`, for each instance of the match.
 *)
val get_metavar_regex_capture_bindings :
  Match_env.env ->
  file:string ->
  Range_with_metavars.t ->
  (* The arguments in CondNestedFormula *)
  (* mvar, regex string, const prop *)
  Metavariable.mvar * string * bool ->
  Metavariable.bindings list option
