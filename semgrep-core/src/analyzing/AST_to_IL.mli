(*s: pfff/lang_GENERIC/analyze/AST_to_IL.mli *)

val function_definition: AST_generic.function_definition
  -> IL.name list * IL.stmt list

(*s: signature [[AST_to_IL.stmt]] *)
val stmt: AST_generic.stmt -> IL.stmt list
(*e: signature [[AST_to_IL.stmt]] *)
(*e: pfff/lang_GENERIC/analyze/AST_to_IL.mli *)
