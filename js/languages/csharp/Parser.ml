let _ =
  Common.jsoo := true;
  Semgrep_js_shared.make_js_module Parse_csharp_tree_sitter.parse
    Parse_csharp_tree_sitter.parse_pattern;
  ()
