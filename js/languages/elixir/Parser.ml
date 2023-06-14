let parse_pattern print_errors _ str =
  let res = Parse_elixir_tree_sitter.parse_pattern str in
  let pattern =
    Pfff_or_tree_sitter.extract_pattern_from_tree_sitter_result res print_errors
  in
  Elixir_to_generic.any pattern

let parse_target _ file =
  Pfff_or_tree_sitter.run file
    [ TreeSitter Parse_elixir_tree_sitter.parse ]
    Elixir_to_generic.program

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module [ Lang.Elixir ] parse_target parse_pattern
