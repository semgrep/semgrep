(*
   Copyright (c) 2022-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
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
  Rule.formula ->
  Range_with_metavars.t ->
  Range_with_metavars.t list) ->
  Match_env.env ->
  Range_with_metavars.t ->
  (* The arguments in CondNestedFormula *)
  Metavariable.mvar ->
  (* Why is this analyzer optional? *)
  Analyzer.t option ->
  Rule.formula ->
  Metavariable.bindings list
