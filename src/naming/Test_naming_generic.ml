open Common
open File.Operators
module H = AST_generic_helpers

let test_naming_generic ~parse_program file =
  let file = Fpath.v file in
  let ast = parse_program !!file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;
  let s = AST_generic.show_any (AST_generic.Pr ast) in
  pr2 s

let actions ~parse_program =
  [
    ( "-naming_generic",
      " <file>",
      Arg_helpers.mk_action_1_arg (test_naming_generic ~parse_program) );
  ]
