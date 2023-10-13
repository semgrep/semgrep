let parse_target _ file =
  Pfff_or_tree_sitter.run file
    [ TreeSitter Parse_cpp_tree_sitter.parse ]
    Cpp_to_generic.program

let parse_pattern print_errors _ str =
  let any =
    str
    |> Pfff_or_tree_sitter.run_pattern ~print_errors
         (* coupling: check src/parsing_languages/Parse_pattern2.ml *)
         [
           PfffPat (Parse_cpp.any_of_string Flag_parsing_cpp.Cplusplus);
           TreeSitterPat Parse_cpp_tree_sitter.parse_pattern;
         ]
  in
  Cpp_to_generic.any None any

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module [ Lang.Cpp ] parse_target parse_pattern
