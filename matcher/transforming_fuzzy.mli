
type ('a, 'b) transformer = 
  'a -> 'b -> 
  Metavars_fuzzy.fuzzy_binding list

(* this works by side effect on the second argument and its .transfo field *)
val transform_trees_trees :
  Ast_fuzzy.trees -> Ast_fuzzy.trees -> Metavars_fuzzy.fuzzy_binding -> unit
