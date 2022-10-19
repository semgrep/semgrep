type debug_taint = {
  sources : (Range_with_metavars.t * Rule.taint_source) list;
      (** Ranges matched by `pattern-sources:` *)
  sanitizers : Range_with_metavars.ranges;
      (** Ranges matched by `pattern-sanitizers:` *)
  sinks : (Range_with_metavars.t * Rule.taint_sink) list;
      (** Ranges matched by `pattern-sinks:` *)
}
(** To facilitate debugging of taint rules. *)

(* It could be a private function, but it is also used by Deep Semgrep. *)
val taint_config_of_rule :
  Match_env.xconfig ->
  Common.filename ->
  AST_generic.program * Parse_info.token_location list ->
  Rule.taint_rule ->
  (Dataflow_tainting.var option ->
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
