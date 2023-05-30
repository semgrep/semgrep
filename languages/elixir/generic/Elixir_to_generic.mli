val args_of_exprs_and_keywords :
  AST_elixir.expr list -> AST_elixir.pair list -> AST_elixir.argument list

val items_of_exprs_and_keywords :
  AST_elixir.expr list -> AST_elixir.pair list -> AST_elixir.item list

val mk_call_parens :
  AST_elixir.expr ->
  AST_elixir.argument list AST_elixir.bracket ->
  AST_elixir.do_block option ->
  AST_elixir.call

val mk_call_no_parens :
  AST_elixir.expr ->
  AST_elixir.argument list ->
  AST_elixir.do_block option ->
  AST_elixir.call

val binary_call :
  AST_elixir.expr ->
  (AST_elixir.ident, AST_generic.operator AST_generic.wrap) Common.either ->
  AST_elixir.expr ->
  AST_elixir.expr

val expr_of_e_or_kwds :
  (AST_elixir.expr, AST_elixir.pair list) Common.either -> AST_elixir.expr

val expr_of_block : AST_elixir.block -> AST_elixir.expr

val stab_clauses_to_function_definition :
  Tok.t -> AST_elixir.stab_clause list -> AST_generic.function_definition

val body_to_stmts : AST_elixir.body -> AST_generic.stmt list
