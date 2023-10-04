let parse_target _ file =
  Pfff_or_tree_sitter.run file
    [ TreeSitter Parse_c_tree_sitter.parse ]
    C_to_generic.program

let parse_pattern print_errors _ str =
  let any =
    str
    |> Pfff_or_tree_sitter.run_pattern ~print_errors
         [
           (* this internally uses `Parse_cpp` *)
           PfffPat (fun x -> Parse_c.any_of_string x);
           (* this internally uses `Parse_cpp_tree_sitter` *)
           TreeSitterPat Parse_c_tree_sitter.parse_pattern;
         ]
  in
  C_to_generic.any any

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module [ Lang.C ] parse_target parse_pattern
