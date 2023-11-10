let parse_pattern print_errors _ str =
  let js_ast =
    str
    |> Pfff_or_tree_sitter.run_pattern ~print_errors
         (* coupling: semgrep/js/languages/typescript/Parser.ml *)
         [
           PfffPat Parse_js.any_of_string;
           TreeSitterPat Parse_typescript_tree_sitter.parse_pattern;
         ]
  in
  Js_to_generic.any js_ast

let parse_target _ file =
  let parse_embedded_js file =
    let { Parsing_result2.ast; errors; _ } =
      Pfff_or_tree_sitter.run file
        [
          TreeSitter (Parse_typescript_tree_sitter.parse ~dialect:`TSX);
          Pfff (Pfff_or_tree_sitter.throw_tokens Parse_js.parse);
        ]
        Js_to_generic.program
    in
    (* TODO: pass the errors down to Parse_vue_tree_sitter.parse
     * and accumulate with other vue parse errors
     *)
    if errors <> [] then failwith "parse error in embedded JS";
    ast
  in
  Pfff_or_tree_sitter.run file
    [ TreeSitter (Parse_vue_tree_sitter.parse parse_embedded_js) ]
    (fun x -> x)

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module [ Lang.Vue ] parse_target parse_pattern
