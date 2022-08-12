type debug_taint = {
  sources : (Range_with_metavars.t * Specialize_formula.taint_source) list;
      (** Ranges matched by `pattern-sources:` *)
  sanitizers : Range_with_metavars.ranges;
      (** Ranges matched by `pattern-sanitizers:` *)
  sinks : (Range_with_metavars.t * Specialize_formula.taint_sink) list;
      (** Ranges matched by `pattern-sinks:` *)
}
(** To facilitate debugging of taint rules. *)

(* It could be a private function, but it is also used by Deep Semgrep. *)
val get_taint_config :
  Match_env.env ->
  (Specialize_formula.sformula -> Range_with_metavars.t list) ->
  Specialize_formula.taint_spec ->
  (Dataflow_tainting.var option ->
  Taint.finding list ->
  Taint.taints Dataflow_core.env ->
  unit) ->
  Dataflow_tainting.config * debug_taint

val get_matches_raw :
  Match_env.env ->
  Specialize_formula.taint_spec ->
  (Specialize_formula.sformula -> Range_with_metavars.t list) ->
  Range_with_metavars.t list
