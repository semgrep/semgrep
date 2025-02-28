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

type spec_matches = {
  sources : (Range_with_metavars.t * Rule.taint_source) list;
      (** Ranges matched by `pattern-sources:` *)
  propagators : propagator_match list;
      (** Ranges matched by `pattern-propagators:` *)
  sanitizers : (Range_with_metavars.t * Rule.taint_sanitizer) list;
      (** Ranges matched by `pattern-sanitizers:` *)
  sinks : (Range_with_metavars.t * Rule.taint_sink) list;
      (** Ranges matched by `pattern-sinks:` *)
}

val hook_mk_taint_spec_match_preds :
  (Rule.rule -> spec_matches -> Taint_spec_preds.t) option ref

(* It could be a private function, but it is also used by Deep Semgrep. *)
(* This [formula_cache] argument is exposed here because this function is also
   a subroutine but the cache itself should be created outside of the any main
   loop which runs over rules. This cache is only safe to share with if
   [taint_config_of_rule] is used on the same file!
*)
val taint_config_of_rule :
  per_file_formula_cache:Formula_cache.t ->
  file:Taint_rule_inst.file ->
  Match_env.xconfig ->
  AST_generic.program * Tok.location list ->
  Rule.taint_rule ->
  Taint_rule_inst.t * spec_matches * Matching_explanation.t list

(* Exposed for Pro *)

val range_of_any : AST_generic.any -> Range.t option
val overlap_with : match_range:Range.t -> Range.t -> float

val mk_propagator_match :
  Rule.rule ->
  propagator_match ->
  string ->
  [ `From | `To ] ->
  Range.t ->
  Taint_spec_preds.a_propagator Taint_spec_match.t
