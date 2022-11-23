(* See https://jsonnet.org/ref/spec.html#manifestation *)

let manifest (v : Value_jsonnet.value_) : JSON.t = Eval_jsonnet.manifest_value v
