type debug_taint = {
  sources : (Range_with_metavars.t * Rule.taint_source) list;
      (** Ranges matched by `pattern-sources:` *)
  sanitizers : Range_with_metavars.ranges;
      (** Ranges matched by `pattern-sanitizers:` *)
  sinks : (Range_with_metavars.t * Rule.taint_sink) list;
      (** Ranges matched by `pattern-sinks:` *)
}
(** To facilitate debugging of taint rules. *)

val hook_setup_hook_function_taint_signature :
  (Match_env.xconfig ->
  Rule.taint_rule ->
  Dataflow_tainting.config ->
  Xtarget.t ->
  unit)
  option
  ref
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
  *   We only need to analyze anonymous functions which do not get taint sigantures
  *   (or we could infer a signature for them too...).
  *)

(* It could be a private function, but it is also used by Deep Semgrep. *)
val taint_config_of_rule :
  Match_env.xconfig ->
  Common.filename ->
  AST_generic.program * Parse_info.token_location list ->
  Rule.taint_rule ->
  (AST_generic.unique_name option ->
  Taint.finding list ->
  Taint_lval_env.t ->
  unit) ->
  Dataflow_tainting.config * debug_taint * Matching_explanation.t list

val check_fundef :
  Lang.t ->
  Config_semgrep_t.t (** rule options *) ->
  Dataflow_tainting.config ->
  AST_generic.entity option (** entity being analyzed *) ->
  AST_generic.function_definition ->
  IL.cfg * Dataflow_tainting.mapping
(** Check a function definition using a [Dataflow_tainting.config] (which can
  * be obtained with [taint_config_of_rule]). Findings are passed on-the-fly
  * to the [handle_findings] callback in the dataflow config.
  *
  * This is a low-level function exposed for debugging purposes (-dfg_tainting).
  *)

val check_rule :
  Rule.taint_rule ->
  (string -> Pattern_match.t -> unit) ->
  Match_env.xconfig ->
  Xtarget.t ->
  Report.rule_profiling Report.match_result * debug_taint
