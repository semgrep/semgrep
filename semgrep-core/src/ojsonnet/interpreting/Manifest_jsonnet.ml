(* See https://jsonnet.org/ref/spec.html#manifestation *)

let manifest_value (v : Value_jsonnet.value_) : JSON.t =
  Eval_jsonnet.manifest_value v
