type propagator_match = {
  id : Taint_rule_inst.var;
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
  (Rule.rule -> spec_matches -> Taint_rule_inst.spec_predicates) option ref

(* It could be a private function, but it is also used by Deep Semgrep. *)
(* This [formula_cache] argument is exposed here because this function is also
   a subroutine but the cache itself should be created outside of the any main
   loop which runs over rules. This cache is only safe to share with if
   [taint_config_of_rule] is used on the same file!
*)
val taint_config_of_rule :
  per_file_formula_cache:Formula_cache.t ->
  ?handle_effects:Taint_rule_inst.effects_handler
    (** Use 'handle_effects' to e.g. apply hash-consing (see 'Deep_tainting'), or
        to do some side-effect if needed.

        old: In the past one had to use 'handle_effects' to record taint effects
          by side-effect (no pun intended), however this is not needed now because
          'Dataflow_tainting.fixpoint' already returns the set of taint effects. *) ->
  Match_env.xconfig ->
  Lang.t ->
  Fpath.t ->
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
  Taint_rule_inst.a_propagator Taint_spec_match.t
