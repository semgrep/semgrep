(*
   Debugging utility for showing which pairs (pattern node, target node)
   get compared.
*)

(*
   Set this flag to true while debugging, and recompile semgrep-core.

   Usage:

     Trace_matching.(if on then print_expr_pair a b);
*)
val on : bool
val print_pair : string -> ('a -> OCaml.v) -> 'a -> 'a -> unit
val print_literal_pair : AST_generic.literal -> AST_generic.literal -> unit
val print_type_pair : AST_generic.type_ -> AST_generic.type_ -> unit

val print_arithmetic_operator_pair :
  AST_generic.operator -> AST_generic.operator -> unit

val print_function_definition_pair :
  AST_generic.function_definition -> AST_generic.function_definition -> unit

val print_class_definition_pair :
  AST_generic.class_definition -> AST_generic.class_definition -> unit

val print_definition_pair :
  AST_generic.definition -> AST_generic.definition -> unit

val print_directive_pair :
  AST_generic.directive -> AST_generic.directive -> unit

val print_expr_pair : AST_generic.expr -> AST_generic.expr -> unit
val print_stmt_pair : AST_generic.stmt -> AST_generic.stmt -> unit
val print_argument_pair : AST_generic.argument -> AST_generic.argument -> unit

val print_arguments_pair :
  AST_generic.argument list AST_generic.bracket ->
  AST_generic.argument list AST_generic.bracket ->
  unit

val print_raw_pair : AST_generic.raw_tree -> AST_generic.raw_tree -> unit
