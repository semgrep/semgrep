(* may also raise an exception? *)
val eval_program : Core_jsonnet.program -> Value_jsonnet.value_

(* Ideally it should be a in a separate Manifest_jsonnet module, but
 * the evaluator and manifester are mutually recursive
 *)
val manifest_value : Value_jsonnet.value_ -> JSON.t
