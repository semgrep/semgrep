(* limited to expressions for now.
 * TODO: could be extended to stmt and stmts later and so return an
 * AST_generic.any option.
 *)
val expr_at_range : Range.t -> AST_generic.program -> AST_generic.expr option

(* Returns the first any found within range. *)
val any_at_range_first :
  Range.t -> AST_generic.program -> AST_generic.any option

(* Repeatedly calls any_at_range to consume as much of the target input as possible.
 * If a call at any_at_range does not consume the entire token range, then
 * any_at_range is called again with a new target range starting where the last call
 * left off.

 * For example, range provided by the user is (0, 29). First call to any_at_range
 * returns found (0, 17). The found match is added to the list and then
 * any_at_range is called again with (18, 29). If another match is found
 * then it is appended to the list.

 * Empty list is returned if no matches are found.
 *)
val any_at_range_all : Range.t -> AST_generic.program -> AST_generic.any option

(* Utility function for joining a list of anys together.
 * Primary purpose is converting AST_generic.S list to AST_generic.Ss
 *)
val join_anys : AST_generic.any list -> AST_generic.any option

(* Utility function for splitting an any into a list.
 *  Primary purpose is splitting AST_generic.Ss into AST_generic.S list.
 *)
val split_any : AST_generic.any -> AST_generic.any list
