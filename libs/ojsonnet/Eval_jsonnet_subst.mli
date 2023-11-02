(* may raise Eval_jsonnet_common.Error *)
val eval_expr : Core_jsonnet.expr -> Value_jsonnet.value_
val manifest_value : Value_jsonnet.value_ -> JSON.t
