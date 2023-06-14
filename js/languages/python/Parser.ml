let lang_to_python_parsing_mode = function
  | Lang.Python -> Parse_python.Python
  | Lang.Python2 -> Parse_python.Python2
  | Lang.Python3 -> Parse_python.Python3
  | s -> failwith (Printf.sprintf "not a python language:%s" (Lang.to_string s))

let parse_pattern _ lang str =
  let parsing_mode = lang_to_python_parsing_mode lang in
  let any = Parse_python.any_of_string ~parsing_mode str in
  Python_to_generic.any any

let parse_target lang file =
  let parsing_mode = lang_to_python_parsing_mode lang in
  Pfff_or_tree_sitter.run file
    [
      Pfff (Pfff_or_tree_sitter.throw_tokens (Parse_python.parse ~parsing_mode));
      TreeSitter Parse_python_tree_sitter.parse;
    ]
    Python_to_generic.program

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module
    [ Lang.Python; Lang.Python2; Lang.Python3 ]
    parse_target parse_pattern
