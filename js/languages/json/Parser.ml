let parse_pattern _ _ str =
  let any = Parse_json.any_of_string str in
  Json_to_generic.any any

let parse_target _ file =
  Pfff_or_tree_sitter.run file
    [
      Pfff
        (fun file ->
          (Parse_json.parse_program file, Parsing_stat.correct_stat file));
    ]
    Json_to_generic.program

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module [ Lang.Json ] parse_target parse_pattern
