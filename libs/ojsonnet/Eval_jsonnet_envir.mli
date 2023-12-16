(* TODO: at some point we probably would prefer an AST_generic.program
 * which could track the origin of tokens through import, eval, and
 * manifestation and that we could pass to osemgrep to evaluate the rules.
 *)

(* entry point; may raise Eval_jsonnet_common.Error *)
val eval_program : Core_jsonnet.program -> Value_jsonnet.t
val manifest_value : Value_jsonnet.t -> JSON.t

val eval_program_with_env :
  Value_jsonnet.env -> Core_jsonnet.program -> Value_jsonnet.t
