let parse_pattern str = Go_to_generic.any (Parse_go.any_of_string str)

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module Parse_go_tree_sitter.parse parse_pattern;
  ()
