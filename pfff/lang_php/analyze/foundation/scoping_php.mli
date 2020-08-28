(*s: scoping_php.mli *)

(* !!Annotate via side effects!!. Fill in the scope information that
 * was put to None during parsing. I return a program, but really it
 * works by side effect. 
 *
 * val annotate_toplevel: 
 * Ast_php.toplevel -> Ast_php.toplevel
 * 
 * update: because lots of the logic is the same for the 
 *   check_variables_php script, this module is also now
 *   reponsible for the scope annotation via a bool 
 *   parameter.
 *)
(*x: scoping_php.mli *)
(*e: scoping_php.mli *)
