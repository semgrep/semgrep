
(* this is just used for the reaching/liveness analysis for now *)

val lvalues_of_expr:
  AST.expr -> (AST.ident * AST.id_info) list

val rvalues_of_expr:
  AST.expr -> (AST.ident * AST.id_info) list
