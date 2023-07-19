exception Error of string * Tok.t
(* TODO: at some point we probably would prefer an AST_generic.program
 * which could track the origin of tokens through import, eval, and
 * manifestation and that we could pass to osemgrep to evaluate the rules.
 *)

(* may raise Error *)
val eval_program_with_env :
  Value_jsonnet.env -> Core_jsonnet.program -> Value_jsonnet.value_

val eval_program : Core_jsonnet.program -> Value_jsonnet.value_
val manifest_value : Value_jsonnet.value_ -> JSON.t
