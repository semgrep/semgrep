(* may raise Eval_jsonnet_common.Error *)
val eval_program : Core_jsonnet.program -> Value_jsonnet.value_
val manifest_value : Value_jsonnet.value_ -> JSON.t
