type propagator_match = {
  id : Dataflow_tainting.var;
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

(* It could be a private function, but it is also used by Deep Semgrep. *)
(* This [formula_cache] argument is exposed here because this function is also
   a subroutine but the cache itself should be created outside of the any main
   loop which runs over rules. This cache is only safe to share with if
   [taint_config_of_rule] is used on the same file!
*)
val taint_config_of_rule :
  per_file_formula_cache:Formula_cache.t ->
  Match_env.xconfig ->
  string (* filename *) ->
  AST_generic.program * Tok.location list ->
  Rule.taint_rule ->
  Dataflow_tainting.effects_handler ->
  Dataflow_tainting.config * spec_matches * Matching_explanation.t list
