val hook_setup_hook_function_taint_signature :
  (Rule.taint_rule -> Taint_rule_inst.t -> Xtarget.t -> unit) option ref
(** This is used for intra-file inter-procedural taint-tracking, and the idea is
  * that this hook will do top-sorting and infer the signature of each function
  * in the file, and while doing this it will also setup
  * 'Dataflow_tainting.hook_function_taint_signature'.
  *
  * Doing it here (vs what DeepSemgrep does) has the advantage that we can re-use
  * the same 'Dataflow_tainting.config' without having to do any caching on disk.
  *
  * FIXME: Once we have the taint signature of a function we do not need to run
  *   taint tracking on it anymore... but we still do it hence duplicating work.
  *   We only need to analyze anonymous functions which do not get taint signatures
  *   (or we could infer a signature for them too...).
  *)

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
  match_hook:(Core_match.t list -> Core_match.t list) ->
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
