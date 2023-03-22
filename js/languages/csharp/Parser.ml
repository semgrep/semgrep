let parse_target x = Parse_csharp_tree_sitter.parse x
let parse_pattern x = Parse_csharp_tree_sitter.parse_pattern x

let _ =
  ignore parse_target;
  ignore parse_pattern;
  ()
