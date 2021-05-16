(*s: pfff/lang_GENERIC/analyze/Naming_AST.mli *)

(*s: signature [[Naming_AST.resolve]] *)
(* Works by side effect on the generic AST by modifying its refs.
 * We pass the lang because some name resolution algorithm may be
 * specific to a language.
 *)
val resolve : Lang.t -> AST_generic.program -> unit

(*e: signature [[Naming_AST.resolve]] *)
(*e: pfff/lang_GENERIC/analyze/Naming_AST.mli *)
