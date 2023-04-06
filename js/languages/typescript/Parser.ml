let parse_target x =
  let res = Parse_typescript_tree_sitter.parse ?dialect:None x in
  let _ = Parse_js.parse x in
  res

(* TODO: understand what we use conv for *)
(* let conv x = Js_to_generic.program x *)

let parse_pattern x =
  let res = Parse_typescript_tree_sitter.parse_pattern x in
  let _ = Parse_js.any_of_string x in
  res

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module parse_target parse_pattern
