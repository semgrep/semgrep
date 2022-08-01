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
  Config_semgrep_t.t ->
  Equivalence.equivalences ->
  Common.filename ->
  AST_generic.program * Parse_info.token_location list ->
  Rule.taint_rule ->
  (Dataflow_tainting.var option ->
  Taint.finding list ->
  Taint.taints Dataflow_core.env ->
  unit) ->
  Dataflow_tainting.config * debug_taint

val check_rule :
  Rule.taint_rule ->
  (string -> Pattern_match.t -> unit) ->
  Config_semgrep.t * Equivalence.equivalences ->
  Xtarget.t ->
  Report.rule_profiling Report.match_result * debug_taint
