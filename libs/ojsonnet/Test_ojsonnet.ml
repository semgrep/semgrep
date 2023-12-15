let dump_jsonnet_ast file =
  (* let cst = Tree_sitter_jsonnet.Parse.file file in *)
  (* let res = Parse_jsonnet_tree_sitter.parse file in *)
  let ast = Parse_jsonnet.parse_program file in
  UCommon.pr2 (AST_jsonnet.show_program ast)

let dump_jsonnet_core file =
  let ast = Parse_jsonnet.parse_program file in
  let core = Desugar_jsonnet.desugar_program file ast in
  UCommon.pr2 (Core_jsonnet.show_program core)

let dump_jsonnet_value file =
  let ast = Parse_jsonnet.parse_program file in
  let core = Desugar_jsonnet.desugar_program file ast in
  let value_ = Eval_jsonnet.eval_program core in
  UCommon.pr2 (Value_jsonnet.show value_)

let dump_jsonnet_json file =
  let ast = Parse_jsonnet.parse_program file in
  let core = Desugar_jsonnet.desugar_program file ast in
  let value_ = Eval_jsonnet.eval_program core in
  let json = Eval_jsonnet.manifest_value value_ in
  let str = JSON.string_of_json json in
  UCommon.pr2 str

(* this is using explicitely the subst model *)
let perf_test_jsonnet file =
  let ast = Parse_jsonnet.parse_program file in
  let core = Desugar_jsonnet.desugar_program file ast in
  let start_time = Sys.time () in
  let value_ = Eval_jsonnet_subst.eval_program core in
  let _ = Eval_jsonnet_subst.manifest_value value_ in
  let end_time = Sys.time () in
  let approx_dif = string_of_float (end_time -. start_time) in
  UCommon.pr2
    ("the approximate time it takes to evaluate and manifest is: " ^ approx_dif)
