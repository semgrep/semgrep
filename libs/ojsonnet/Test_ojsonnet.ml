open Common
module V = Value_jsonnet

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
  let core = Desugar_jsonnet.desugar_program ~use_std:false file ast in
  let value_ = Eval_jsonnet.eval_program core V.empty_env in
  pr2 (Value_jsonnet.show_value_ value_)

let dump_jsonnet_json file =
  let ast = Parse_jsonnet.parse_program file in
  let core = Desugar_jsonnet.desugar_program file ast in
  let value_ = Eval_jsonnet.eval_program core V.empty_env in
  let json = Eval_jsonnet.manifest_value value_ in
  let str = JSON.string_of_json json in
  pr2 str
