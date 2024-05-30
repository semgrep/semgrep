(* Works by side effect on the generic AST by modifying its refs.
 * We pass the lang because some name resolution algorithm may be
 * specific to a language.
 *)
val resolve : Lang.t -> AST_generic.program -> unit

val pro_hook_normalize_ast_generic_type :
  (Lang.t -> AST_generic.type_ -> AST_generic.type_) option ref
