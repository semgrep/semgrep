exception Error of string * Tok.t

(* may raise Error *)
val eval_program :
  Core_jsonnet.program -> Core_jsonnet.env -> Core_jsonnet.value_

(* TODO: at some point we probably would prefer an AST_generic.program
 * which could track the origin of tokens through import, eval, and
 * manifestation and that we could pass to osemgrep to evaluate the rules.
 *)
val manifest_value : Core_jsonnet.value_ -> JSON.t
