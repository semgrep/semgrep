(* Mark all expression nodes that may be executed before exiting a function
 * as "returning nodes", so later on these expressions can be matched with
 * return statements.
 *)
val mark_implicit_return_nodes : Lang.t -> AST_generic.program -> unit
