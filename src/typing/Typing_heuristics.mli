val guess_type :
  Lang.t ->
  (* type_of_expr *)
  (AST_generic.expr -> AST_generic.name Type.t) ->
  AST_generic.expr ->
  AST_generic.name Type.t
