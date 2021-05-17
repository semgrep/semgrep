val exprs_of_node : Controlflow.node -> AST_generic.expr list

(* can also be (ab)used as an iter_on_node_and_expr *)
val fold_on_node_and_expr :
  (Controlflow.nodei * Controlflow.node -> AST_generic.expr -> 'a -> 'a) ->
  Controlflow.flow ->
  'a ->
  'a

type visitor_in = Visitor_AST.visitor_in

type visitor_out = Controlflow.node -> unit

val mk_visitor : visitor_in -> visitor_out
