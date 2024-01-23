(* entry point; may raise Eval_jsonnet_common.Error *)
val eval_program : Core_jsonnet.program -> Value_jsonnet.t
val manifest_value : Value_jsonnet.t -> JSON.t
