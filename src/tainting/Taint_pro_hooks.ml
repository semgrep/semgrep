(* Iago Abal
 *
 * Copyright (C) 2025 Semgrep Inc., All rights reserved
 *)
open Shape_and_sig

type t = {
  instantiate_function_signature :
    Taint_lval_env.t ->
    Signature.t ->
    callee:IL.exp ->
    args:IL.exp IL.argument list option (** actual arguments *) ->
    (Taint.Taint_set.t * Shape.shape) IL.argument list ->
    Instantiated_signature.t option;
      (** Support for inter-procedural analysis. *)
  check_tainted_at_exit_sinks :
    Taint_spec_preds.t ->
    Taint_lval_env.t ->
    IL.node ->
    (Taint.taints * Shape_and_sig.Effect.sink list) option;
      (** Support for `at-exit: true` sinks *)
}

let hook_taint_pro_hooks : t option Hook.t = Hook.create None
