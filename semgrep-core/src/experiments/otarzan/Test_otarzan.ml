open Common

(* this can help when writing a code generator to see the constructs
 * you need to handle.
 *)
let dump_ast_ml file =
  let ast = Otarzan.parse file in
  let s = Ast_ml.show_program ast in
  pr s
  [@@action]

let actions () =
  [
    ("-dump_ast_ml", "<file>", Common.mk_action_1_arg dump_ast_ml);
    ( "-generate_boilerplate_map_todo",
      "<file>",
      Common.mk_action_1_arg Otarzan.generate_boilerplate_map_todo );
  ]
