open Shape_and_sig

type t = {
  instantiate_function_signature :
    Taint_lval_env.t ->
    Signature.t ->
    callee:IL.exp ->
    args:IL.exp IL.argument list option (** actual arguments *) ->
    (Taint.Taint_set.t * Shape.shape) IL.argument list ->
    Instantiated_signature.t option;
}

let hook_taint_pro_hooks : t option Hook.t = Hook.create None
