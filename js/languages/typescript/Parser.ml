let get_dialect_from_lang lang =
  match lang with
  | Lang.Js -> Some `TSX
  | Lang.Ts -> None
  | s ->
      failwith (Printf.sprintf "not a javascript languge:%s" (Lang.to_string s))

let parse_target lang file =
  let dialect = get_dialect_from_lang lang in
  Pfff_or_tree_sitter.run file
    [ TreeSitter (Parse_typescript_tree_sitter.parse ?dialect) ]
    Js_to_generic.program

(* TODO: understand what we use conv for *)
(* let conv x = Js_to_generic.program x *)

let parse_pattern print_errors _ str =
  let res = Parse_typescript_tree_sitter.parse_pattern str in
  Pfff_or_tree_sitter.extract_pattern_from_tree_sitter_result res print_errors

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module [ Lang.Js; Lang.Ts ] parse_target
    parse_pattern
