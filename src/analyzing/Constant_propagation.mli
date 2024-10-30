val constant_propagation_and_evaluate_literal :
  ?lang:Lang.t -> AST_generic.expr -> AST_generic.svalue option
(** Partially evaluate a Generic expression. *)

type propagate_basic_visitor_funcs = {
  visit_definition :
    Eval_generic_partial.env * Iter_with_context.context ->
    AST_generic.definition ->
    unit;
}

val add_constant_env :
  AST_generic.ident ->
  AST_generic.sid * AST_generic.svalue ->
  Eval_generic_partial.env ->
  unit

(* used by pro engine *)
val hook_propagate_basic_visitor : propagate_basic_visitor_funcs option ref

(* Works by side effect on the generic AST by modifying its refs.
 * We pass the lang because some constant propagation algorithm may be
 * specific to a language.
 *)
(* !Note that this assumes Naming_AST.resolve has been called before! *)
val propagate_basic : Lang.t -> AST_generic.program -> unit

(* This is exposed for DeepSemgrep, so it can control the order
 * in which functions are analyzed. Generally will perform better
 * if propagate_basic is called first *)
val propagate_dataflow_one_function :
  Lang.t -> IL.fun_cfg (* function CFG *) -> unit

val propagate_dataflow : Lang.t -> AST_generic.program -> unit
