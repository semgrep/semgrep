(* This module is currently deprecated. You probably should rely
 * on Ugly_print_AST.ml instead.
 *)

val expr_to_string : Lang.t -> AST_generic.expr -> string
val svalue_to_string : Lang.t -> AST_generic.svalue -> string
val stmt_to_string : Lang.t -> AST_generic.stmt -> string
val arguments_to_string : Lang.t -> AST_generic.argument list -> string
