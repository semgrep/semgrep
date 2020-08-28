(*s: unparse_php.mli *)

(* this does not maintain comments and space but maintain the newlines *)
val string_of_program: Cst_php.program -> string

(* does not maintain comments or space *)
val string_of_any: Cst_php.any -> string
val string_of_expr: Cst_php.expr -> string

(* preserve space and comments and handle the transfo annotation
 * (this function is called by spatch)
 *)
val string_of_program_with_comments_using_transfo:
  Parse_php.program_with_comments -> string

(*e: unparse_php.mli *)
