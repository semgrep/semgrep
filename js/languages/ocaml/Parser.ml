let parse_pattern _ str =
  let any = Parse_ml.any_of_string str in
  Ml_to_generic.any any

let parse_target file =
  Pfff_or_tree_sitter.run file
    [ TreeSitter Parse_ocaml_tree_sitter.parse ]
    Ml_to_generic.program

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module Lang.Ocaml parse_target parse_pattern
