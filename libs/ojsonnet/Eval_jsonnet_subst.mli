exception Error of string * Tok.t

val eval_expr : Core_jsonnet.expr -> Value_jsonnet.value_
val manifest_value : Value_jsonnet.value_ -> JSON.t
