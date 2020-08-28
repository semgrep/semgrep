
exception ObsoleteConstruct of string
exception TodoConstruct of string

(* used by prettyphp *)
val program_with_comments:
  Parser_php.token list -> Cst_php.program -> Ast_pp.program

(* used by spatch *)
val toplevels:
  Parser_php.token list -> Cst_php.toplevel list -> Ast_pp.stmt list

val class_stmts:
  Parser_php.token list -> Cst_php.class_stmt list -> Ast_pp.class_element list
