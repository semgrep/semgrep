let parse_pattern _ str =
  let res = Parse_dart_tree_sitter.parse_pattern str in
  Pfff_or_tree_sitter.extract_pattern_from_tree_sitter_result res

let parse_target _ file =
  Pfff_or_tree_sitter.run file [ TreeSitter Parse_dart_tree_sitter.parse ]
    (fun x -> x)

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module [ Lang.Dart ] parse_target parse_pattern
