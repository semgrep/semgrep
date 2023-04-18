let parse_pattern _ str =
  let any = Parse_python.any_of_string ~parsing_mode:Parse_python.Python3 str in
  Python_to_generic.any any

let parse_target file =
  Pfff_or_tree_sitter.run file
    [
      Pfff
        (Pfff_or_tree_sitter.throw_tokens
           (Parse_python.parse ~parsing_mode:Parse_python.Python3));
      TreeSitter Parse_python_tree_sitter.parse;
    ]
    Python_to_generic.program

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module Lang.Python3 parse_target parse_pattern
