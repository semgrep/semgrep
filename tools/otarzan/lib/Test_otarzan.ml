(* this can help when writing a code generator to see the constructs
 * you need to handle.
 *)
let dump_ast_ocaml file =
  let ast = Parse.parse file in
  let s = AST_ocaml.show_program ast in
  UCommon.pr s
[@@action]
