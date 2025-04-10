type mapping = Taint_lval_env.t Dataflow_core.mapping
(** Mapping from variables to taint sources (if the variable is tainted).
  * If a variable is not in the map, then it's not tainted. *)

type java_props_cache
(** When we encounter getters/setters without a definition, we need to resolve them
  * to their corresponding property, we cache the results here. *)

type func = {
  fname : IL.name option;
  best_matches : Taint_spec_match.Best_matches.t;
      (** Best matches for the taint sources/etc, see 'Taint_spec_match'. *)
  used_lambdas : IL.NameSet.t;
      (** Set of lambda names that are *used* within the function. If a lambda
        is used, we analyze it at use-site, otherwise we analyze it at def site. *)
}

val mk_empty_java_props_cache : unit -> java_props_cache

type hook_function_taint_signature =
  Taint_rule_inst.t ->
  AST_generic.expr ->
  (Shape_and_sig.Signature.t * [ `Fun | `Var ]) option

(* deep-scan (and pro-scan) hook *)
val hook_function_taint_signature : hook_function_taint_signature option Hook.t

val hook_infer_sig_for_lambda :
  (Taint_rule_inst.t ->
  func ->
  in_env:Taint_lval_env.t ->
  IL.name ->
  IL.function_definition ->
  IL.fun_cfg ->
  Shape_and_sig.Signature.t)
  option
  Hook.t

val fixpoint_aux :
  Taint_rule_inst.t ->
  func ->
  ?needed_vars:IL.NameSet.t ->
  enter_lval_env:Taint_lval_env.t ->
  in_lambda:IL.name option ->
  IL.fun_cfg ->
  Shape_and_sig.Effects.t * mapping
(** Pro: inter-proc lambdas *)

val fixpoint :
  Taint_rule_inst.t ->
  ?in_env:Taint_lval_env.t ->
  ?name:IL.name ->
  IL.fun_cfg ->
  Shape_and_sig.Effects.t * mapping
(** Main entry point, [fixpoint taint_inst cfg] returns a mapping (effectively a set)
  * containing all the tainted variables in [cfg]. Besides, if it infers any taint
  * 'findings', it will invoke [config.handle_findings] which can perform any
  * side-effectful action.
  *
  * @param in_env are the assumptions made on the function's parameters.
  * @param name is the name of the function being analyzed, if it has a name.
  * *)

val must_drop_taints_if_bool_or_number : Rule_options.t -> 'a Type.t -> bool
(** 'must_drop_taints_if_bool_or_number options typ' is 'true' iff given the
  `taint_assume_safe_*` options we need to sanitize expressions of type 'typ'.

  For example, if `taint_assume_safe_numbers` is set and 'typ' is an integer
  type, then 'must_drop_taints_if_bool_or_number' will evaluate to 'true'.

  THINK: Move to module 'Taint' or somewhere else? *)

val sinks_of_matches :
  Taint_lval_env.t ->
  Taint_spec_preds.sink Taint_spec_match.t list ->
  Shape_and_sig.Effect.sink list * Taint_lval_env.t
(** Gets and pre-evaluates the actual 'requires' preconditions for the sinks,
  it already filters out sink matches that trivially fail their 'requires'. *)

val effects_of_tainted_sink :
  Rule_options.t ->
  Shape_and_sig.Effect.taint_to_sink_item list ->
  Shape_and_sig.Effect.sink ->
  Shape_and_sig.Effect.poly list
(** Exposed for Pro *)
