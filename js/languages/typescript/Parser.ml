let parse_target x =
  let res = Parse_typescript_tree_sitter.parse ?dialect:None x in
  let _ = Parse_js.parse x in
  res

let conv x = Js_to_generic.program x

let parse_pattern x =
  let res = Parse_typescript_tree_sitter.parse_pattern x in
  let _ = Parse_js.any_of_string x in
  res

let _ =
  ignore parse_target;
  ignore parse_pattern;
  ignore conv;
  ()
