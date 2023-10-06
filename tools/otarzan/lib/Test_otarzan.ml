open Common

(* this can help when writing a code generator to see the constructs
 * you need to handle.
 *)
let dump_ast_ml file =
  let ast = Parse.parse file in
  let s = Ast_ml.show_program ast in
  pr s
[@@action]
