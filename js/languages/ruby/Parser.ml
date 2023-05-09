let parse_pattern _ _ str =
  let any = Parse_ruby.any_of_string str in
  Ruby_to_generic.any any

let parse_target _ file =
  Pfff_or_tree_sitter.run file
    [
      TreeSitter Parse_ruby_tree_sitter.parse;
      (* right now the parser is verbose and the token positions
       * may be wrong, but better than nothing. *)
      Pfff (Pfff_or_tree_sitter.throw_tokens Parse_ruby.parse);
    ]
    Ruby_to_generic.program

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module [ Lang.Ruby ] parse_target parse_pattern
