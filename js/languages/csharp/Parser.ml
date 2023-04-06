let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module Parse_csharp_tree_sitter.parse
    Parse_csharp_tree_sitter.parse_pattern;
  ()
