type propagator_match = {
  id : Taint_spec_preds.var;
      (** An unique identifier for the propagator match. This is used as an
   * auxiliary variable to store the taints flowing from `from` to `to`. *)
  rwm : Range_with_metavars.t;
  from : Range.t;  (** The range matched by the `from` metavariable. *)
  to_ : Range.t;  (** The range matched by the `to` metavariable. *)
  spec : Rule.taint_propagator;
}
(** Taint will flow from `from` to `to_` through the axiliary variable `id`. *)

type raw_spec_matches = {
  raw_sources : (Range_with_metavars.t * Rule.taint_source) list;
      (** Ranges matched by `pattern-sources:` *)
  raw_propagators : propagator_match list;
      (** Ranges matched by `pattern-propagators:` *)
  raw_sanitizers : (Range_with_metavars.t * Rule.taint_sanitizer) list;
      (** Ranges matched by `pattern-sanitizers:` *)
  raw_sinks : (Range_with_metavars.t * Rule.taint_sink) list;
      (** Ranges matched by `pattern-sinks:` *)
}
(** The "raw" matches of each pattern formula that makes the taint rule. *)

type spec_matches = {
  sources : (Range_with_metavars.t * Rule.taint_source) list;
  propagation_points :
    Taint_spec_preds.propagation_point Taint_spec_match.t list;
  sanitizers : (Range_with_metavars.t * Rule.taint_sanitizer) list;
  sinks : (Range_with_metavars.t * Taint_spec_preds.sink) list;
}
(** A refined version of 'raw_spec_matches' with added
  'Taint_spec_preds.propagation_point's. *)

val hook_mk_taint_spec_match_preds :
  (Rule.rule -> spec_matches -> Taint_spec_preds.t) option ref

(* The [per_file_formula_cache] argument is exposed here because this function is
  also a subroutine, but the cache itself should be created outside of the any
  main loop which runs over rules. This cache is only safe to share with if
  [taint_config_of_rule] is used on the same file!

  NOTE: It could be a private function, but it is also used by Deep Semgrep.
 *)
val taint_config_of_rule :
  per_file_formula_cache:Formula_cache.t ->
  file:Taint_rule_inst.file ->
  Match_env.xconfig ->
  AST_generic.program * Tok.location list ->
  Rule.taint_rule ->
  Taint_rule_inst.t * raw_spec_matches * Matching_explanation.t list

(* Exposed for Pro *)

val range_of_any : AST_generic.any -> Range.t option
val overlap_with : match_range:Range.t -> Range.t -> float
