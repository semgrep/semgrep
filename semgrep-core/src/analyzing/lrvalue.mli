(* this is just used for the reaching/liveness analysis for now *)

val lvalues_of_expr :
  AST_generic.expr -> (AST_generic.ident * AST_generic.id_info) list

val rvalues_of_expr :
  AST_generic.expr -> (AST_generic.ident * AST_generic.id_info) list
