(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* main entry point *)
val check_rule :
  matches_hook:(Core_match.t list -> Core_match.t list) ->
  Rule.search_rule ->
  Match_env.xconfig ->
  Xtarget.t ->
  Core_profiling.rule_profiling Core_result.match_result

val hook_pro_entropy_analysis :
  (mode:Rule.entropy_analysis_mode -> string -> bool) option Hook.t

val hook_pro_metavariable_name :
  (Lang.t -> AST_generic.expr -> Rule.metavar_cond_name -> bool) option ref
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
