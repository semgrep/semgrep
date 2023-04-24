let parse_pattern print_errors _ str =
  let any =
    str
    |> Pfff_or_tree_sitter.run_pattern ~print_errors
         [
           PfffPat Parse_java.any_of_string;
           TreeSitterPat Parse_java_tree_sitter.parse_pattern;
         ]
  in
  Java_to_generic.any any

let parse_target _ file =
  Pfff_or_tree_sitter.run file
    [
      (* we used to start with the pfff one; it was quite good and faster
       * than tree-sitter (because we need to wrap tree-sitter inside
       * an invoke because of a segfault/memory-leak), but when both parsers
       * fail, it's better to give the tree-sitter parsing error now.
       *)
      TreeSitter Parse_java_tree_sitter.parse;
      Pfff (Pfff_or_tree_sitter.throw_tokens Parse_java.parse);
    ]
    Java_to_generic.program

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module [ Lang.Java ] parse_target parse_pattern
