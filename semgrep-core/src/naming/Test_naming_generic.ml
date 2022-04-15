open Common
module H = AST_generic_helpers
module V = Visitor_AST

let test_naming_generic ~parse_program file =
  let ast = parse_program file in
  let lang = List.hd (Lang.langs_of_filename file) in
  Naming_AST.resolve lang ast;
  let s = AST_generic.show_any (AST_generic.Pr ast) in
  pr2 s

let actions ~parse_program =
  [
    ( "-naming_generic",
      " <file>",
      Common.mk_action_1_arg (test_naming_generic ~parse_program) );
  ]
