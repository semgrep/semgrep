val subexprs_of_expr :
  ?symbolic_propagation:bool -> AST_generic.expr -> AST_generic.expr list

val subexprs_of_expr_implicit :
  ?symbolic_propagation:bool -> AST_generic.expr -> AST_generic.expr list

val subexprs_of_stmt_kind : AST_generic.stmt_kind -> AST_generic.expr list

val flatten_substmts_of_stmts :
  AST_generic.stmt list -> (AST_generic.stmt list * AST_generic.stmt) option
