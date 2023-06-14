exception Error of string * Tok.t

(* may raise Error *)
val eval_program : Core_jsonnet.program -> Value_jsonnet.value_
