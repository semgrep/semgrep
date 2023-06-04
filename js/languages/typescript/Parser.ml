let parse_target lang file =
  match lang with
  | Lang.Ts ->
      Pfff_or_tree_sitter.run file
        [ TreeSitter (Parse_typescript_tree_sitter.parse ?dialect:None) ]
        Js_to_generic.program
  | Lang.Js ->
      Pfff_or_tree_sitter.run file
        [
          TreeSitter (Parse_typescript_tree_sitter.parse ~dialect:`TSX);
          Pfff (Pfff_or_tree_sitter.throw_tokens Parse_js.parse);
        ]
        Js_to_generic.program
  | _ -> failwith ("This parser cannot parse lang: " ^ Lang.to_string lang)

let parse_pattern print_errors _ str =
  let js_ast =
    str
    |> Pfff_or_tree_sitter.run_pattern ~print_errors
         [
           PfffPat Parse_js.any_of_string;
           TreeSitterPat Parse_typescript_tree_sitter.parse_pattern;
         ]
  in
  Js_to_generic.any js_ast

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module [ Lang.Js; Lang.Ts ] parse_target
    parse_pattern
