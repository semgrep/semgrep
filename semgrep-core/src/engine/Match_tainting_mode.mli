type debug_taint = {
  sources : (Range_with_metavars.t * Rule.taint_source) list;
      (** Ranges matched by `pattern-sources:` *)
  sanitizers : Range_with_metavars.ranges;
      (** Ranges matched by `pattern-sanitizers:` *)
  sinks : (Range_with_metavars.t * Rule.taint_sink) list;
      (** Ranges matched by `pattern-sinks:` *)
}

type taint_info = {
  config : Dataflow_tainting.config;
  debug_taint : debug_taint;
  spec : Rule.taint_spec;
}
(** To facilitate debugging of taint rules. *)

(* It could be a private function, but it is also used by Deep Semgrep. *)

val get_taint_config :
  Match_env.env ->
  (Rule.formula -> Range_with_metavars.t list * Matching_explanation.t option) ->
  Rule.taint_spec ->
  (Dataflow_tainting.var option ->
  Taint.finding list ->
  Taint.taints Dataflow_var_env.t ->
  unit) ->
  Matching_explanation.t list * taint_info

val evaluate_taint :
  Match_env.env ->
  Rule.taint_spec ->
  (Rule.formula -> Range_with_metavars.t list * Matching_explanation.t option) ->
  Range_with_metavars.t list * Matching_explanation.t list * taint_info
