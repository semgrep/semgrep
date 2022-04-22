(*
   Debugging utility for showing which pairs (pattern node, target node)
   get compared.
*)

(*
   For peace of mind regarding performance, this is not configurable
   at runtime. Having a constant flag 'on' set to 'false' ensures
   that 'if on then ...;' expressions will be removed at compile time.
*)
let on = false

let print_pair name vof a b =
  (* Print the nodes just deep enough to see something useful. *)
  let max_depth = 3 in
  Printf.printf "----- m_%s -----\n%s pattern:\n%s\n~~~~~\n%s target:\n%s\n\n"
    name name
    (vof a |> OCaml.string_of_v ~max_depth)
    name
    (vof b |> OCaml.string_of_v ~max_depth)

let print_literal_pair = print_pair "literal" Meta_AST.vof_literal
let print_type_pair = print_pair "type" Meta_AST.vof_type_

let print_arithmetic_operator_pair =
  print_pair "arithmetic_operator" Meta_AST.vof_arithmetic_operator

let print_function_definition_pair =
  print_pair "function_definition" Meta_AST.vof_function_definition

let print_class_definition_pair =
  print_pair "class_definition" Meta_AST.vof_class_definition

let print_definition_pair = print_pair "definition" Meta_AST.vof_definition
let print_directive_pair = print_pair "directive" Meta_AST.vof_directive
let print_expr_pair = print_pair "expr" Meta_AST.vof_expr
let print_stmt_pair = print_pair "stmt" Meta_AST.vof_stmt
