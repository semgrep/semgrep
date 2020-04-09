
(* If the list returned is empty, then no match was found.
 * Note that [[]] means one match was found but the environment
 * is empty (because your pattern didn't contain any metavariable for
 * instance).
 *)
type ('a, 'b) matcher = 
  'a -> 'b -> 
  Metavars_fuzzy.fuzzy_binding list

(* right now it does not do side effects on the first argument
 * (as we do in coccinelle), but it could at some point
 *)
val match_trees_trees : (Ast_fuzzy.trees, Ast_fuzzy.trees) matcher
