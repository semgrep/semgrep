(* Note that both functions first call Elixir_to_elixir.map_program
 * internally
 *)
val program : AST_elixir.program -> AST_generic.program
val any : AST_elixir.any -> AST_generic.any
