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
val check_fundef :
  Taint_rule_inst.t ->
  IL.name option (** entity being analyzed *) ->
  AST_to_IL.ctx ->
  ?glob_env:Taint_lval_env.t ->
  AST_generic.function_definition ->
  Fun_CFG.t * Shape_and_sig.Effects.t * Dataflow_tainting.mapping
(** Check a function definition using a [Dataflow_tainting.config] (which can
  * be obtained with [taint_config_of_rule]). Findings are passed on-the-fly
  * to the [handle_findings] callback in the dataflow config.
  *
  * This is a low-level function exposed for debugging purposes (-dfg_tainting).
  *)

val check_rules :
  matches_hook:(Core_match.t list -> Core_match.t list) ->
  per_rule_boilerplate_fn:
    (Rule.rule ->
    (unit -> Core_profiling.rule_profiling Core_result.match_result) ->
    Core_profiling.rule_profiling Core_result.match_result) ->
  Rule.taint_rule list ->
  Match_env.xconfig ->
  Xtarget.t ->
  (* timeout function *)
  Core_profiling.rule_profiling Core_result.match_result list
  * Core_error.ErrorSet.t
(** Runs the engine on a group of taint rules, which should be for the
  * same language. Running on multiple rules at once enables inter-rule
  * optimizations.
  *)
