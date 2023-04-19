let parse_pattern print_errors _ str =
  let res = Parse_bash_tree_sitter.parse_pattern str in
  let program =
    Pfff_or_tree_sitter.extract_pattern_from_tree_sitter_result res print_errors
  in
  Bash_to_generic.any program

let parse_target _ file =
  Pfff_or_tree_sitter.run file
    [ TreeSitter Parse_bash_tree_sitter.parse ]
    Bash_to_generic.program

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module [ Lang.Bash ] parse_target parse_pattern
