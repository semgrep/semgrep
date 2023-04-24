let parse_pattern _ _ str = Go_to_generic.any (Parse_go.any_of_string str)

let parse_target _ file =
  Pfff_or_tree_sitter.run file
    [ TreeSitter Parse_go_tree_sitter.parse ]
    Go_to_generic.program

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module [ Lang.Go ] parse_target parse_pattern
