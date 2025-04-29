(* Iago Abal
 *
 * Copyright (C) 2025 Semgrep Inc., All rights reserved
 *)
open Shape_and_sig

type t = {
  instantiate_function_signature :
    Rule_options.t ->
    Taint_lval_env.t ->
    Signature.t ->
    callee:IL.exp ->
    args:IL.exp IL.argument list option (** actual arguments *) ->
    (Taint.Taint_set.t * Shape.shape) IL.argument list ->
    (Taint.Taint_set.t
    * Shape.shape
    * Taint_lval_env.t
    * Shape_and_sig.Effects.t)
    option;
      (** pro-scan hook *)
  infer_update_effects_at_exit :
    in_lambda:bool ->
    enter_env:Taint_lval_env.t ->
    Taint_lval_env.t ->
    Effect.poly list;
      (** pro-scan hook *)
  find_attribute_in_class :
    AST_generic.name -> string -> AST_generic.name option;
      (** deep-scan hook *)
  check_tainted_at_exit_sinks :
    Taint_spec_preds.t ->
    Taint_lval_env.t ->
    IL.node ->
    (Taint.taints * Shape_and_sig.Effect.sink list) option * Taint_lval_env.t;
      (** pro-scan hook *)
  run_pending_propagators :
    Taint.taints Dataflow_var_env.VarMap.t ->
    Taint_lval_env.t ->
    Shape_and_sig.Effects.t ->
    Taint_lval_env.t * Shape_and_sig.Effects.t;
      (** pro-scan hook *)
}

let hook_taint_pro_hooks : t option Hook.t = Hook.create None
