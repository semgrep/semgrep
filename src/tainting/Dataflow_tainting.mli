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

val hook_function_taint_signature :
  (Taint_rule_inst.t ->
  AST_generic.expr ->
  (Shape_and_sig.Signature.t * [ `Fun | `Var ]) option)
  option
  ref
(** Pro inter-file (aka deep) *)

val hook_find_attribute_in_class :
  (AST_generic.name -> string -> AST_generic.name option) option ref
(** Pro inter-file (aka deep) *)

val hook_infer_sig_for_lambda :
  (Taint_rule_inst.t ->
  func ->
  in_env:Taint_lval_env.t ->
  IL.name ->
  IL.function_definition ->
  IL.fun_cfg ->
  Shape_and_sig.Signature.t)
  option
  ref

val hook_check_tainted_at_exit_sinks :
  (Taint_rule_inst.t ->
  Taint_lval_env.t ->
  IL.node ->
  (Taint.taints * Shape_and_sig.Effect.sink list) option)
  option
  ref
(** Pro: support for `at-exit: true` sinks *)

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

(* TODO: Move to module 'Taint' maybe. *)
val drop_taints_if_bool_or_number :
  Rule_options.t -> Taint.Taint_set.t -> 'a Type.t -> Taint.Taint_set.t
