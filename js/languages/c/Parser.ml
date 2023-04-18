let parse_target _ file =
  Pfff_or_tree_sitter.run file
    [ TreeSitter Parse_c_tree_sitter.parse ]
    C_to_generic.program

let parse_pattern _ _ str =
  let any = Parse_c.any_of_string str in
  C_to_generic.any any

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module [ Lang.C ] parse_target parse_pattern
