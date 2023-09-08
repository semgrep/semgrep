(* Perform phase 2 of parsing (see AST_elixit.ml top comment) and
 * transform some Raw elixir constructs in Kernel constructs
 * (e.g., actually parse function definition instead of keeping
 *  them as raw calls)
 *)
val map_program : AST_elixir.program -> AST_elixir.program
val map_any : AST_elixir.any -> AST_elixir.any
