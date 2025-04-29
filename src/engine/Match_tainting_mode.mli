val check_fundef :
  Taint_rule_inst.t ->
  IL.name option (** entity being analyzed *) ->
  AST_to_IL.ctx ->
  ?glob_env:Taint_lval_env.t ->
  AST_generic.function_definition ->
  IL.fun_cfg * Shape_and_sig.Effects.t * Dataflow_tainting.mapping
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
(** Runs the engine on a group of taint rules, which should be for the
  * same language. Running on multiple rules at once enables inter-rule
  * optimizations.
  *)

val matches_of_effects :
  Rule_options.t -> Shape_and_sig.Effects.t -> Core_match.t list
(** Generate matches from ToSink effects. (Exported for Pro) *)

val dedup_matches : Core_match.t list -> Core_match.t list
(** Deduplicate taint matches. (Exported for Pro) *)

(* pro-scan hook *)
val hook_mk_hook_function_taint_signature :
  (Rule.taint_rule ->
  Taint_rule_inst.t ->
  Xtarget.t ->
  Dataflow_tainting.hook_function_taint_signature)
  option
  Hook.t
