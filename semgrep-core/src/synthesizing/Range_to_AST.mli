(* limited to expressions for now.
 * TODO: could be extended to stmt and stmts later and so return an
 * AST_generic.any option.
 *)
val expr_at_range : Range.t -> AST_generic.program -> AST_generic.expr option

val any_at_range : Range.t -> AST_generic.program -> AST_generic.any option
