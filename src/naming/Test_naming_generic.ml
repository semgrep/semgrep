module H = AST_generic_helpers

let test_naming_generic ~parse_program file =
  let ast = parse_program file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;
  let s = AST_generic.show_any (AST_generic.Pr ast) in
  UCommon.pr2 s

let actions ~parse_program =
  [
    ( "-naming_generic",
      " <file>",
      Arg_.mk_action_1_conv Fpath.v (test_naming_generic ~parse_program) );
  ]
