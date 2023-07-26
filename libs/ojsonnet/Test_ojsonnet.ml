open Common

let dump_jsonnet_ast file =
  (* let cst = Tree_sitter_jsonnet.Parse.file file in *)
  (* let res = Parse_jsonnet_tree_sitter.parse file in *)
  let ast = Parse_jsonnet.parse_program file in
  pr2 (AST_jsonnet.show_program ast)

let dump_jsonnet_core file =
  let ast = Parse_jsonnet.parse_program file in
  let core = Desugar_jsonnet.desugar_program ~use_std:false file ast in
  pr2 (Core_jsonnet.show_program core)

let dump_jsonnet_value file =
  let ast = Parse_jsonnet.parse_program file in
  let core = Desugar_jsonnet_LC.desugar_program ~use_std:false file ast in
  let value_ = LC_Eval_jsonnet.eval_expr core in
  pr2 (Value_jsonnet_LC.show_value_ value_)

let dump_jsonnet_json file =
  let ast = Parse_jsonnet.parse_program file in
  let core = Desugar_jsonnet_LC.desugar_program file ast in
  let value_ = LC_Eval_jsonnet.eval_expr core in
  let json = LC_Eval_jsonnet.manifest_value value_ in
  let str = JSON.string_of_json json in
  pr2 str

let test_sub_kw file =
  let ast = Parse_jsonnet.parse_program file in
  let core = Desugar_jsonnet_LC.desugar_program file ast in
  let sub = Core_jsonnet_LC.Id ("hello", Tok.unsafe_fake_tok "hi") in
  let subbed =
    LC_Eval_jsonnet.substitute_kw LC_Eval_jsonnet.fake_self sub core
  in
  let str = Core_jsonnet_LC.show_program subbed in
  pr2 str
