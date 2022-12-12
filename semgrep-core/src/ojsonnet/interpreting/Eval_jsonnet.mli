exception Error of string * Parse_info.t

(* may raise Error *)
val eval_program : Core_jsonnet.program -> Value_jsonnet.value_
