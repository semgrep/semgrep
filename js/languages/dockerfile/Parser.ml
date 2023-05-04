let parse_pattern print_errors _ str =
  let res = Parse_dockerfile_tree_sitter.parse_docker_or_bash_pattern str in
  Pfff_or_tree_sitter.extract_pattern_from_tree_sitter_result res print_errors

let parse_target _ file =
  Pfff_or_tree_sitter.run file
    [ TreeSitter Parse_dockerfile_tree_sitter.parse ]
    Dockerfile_to_generic.program

let _ =
  Common.jsoo := true;
  Tree_sitter_run.Util_file.jsoo := true;
  Semgrep_js_shared.make_js_module [ Lang.Dockerfile ] parse_target
    parse_pattern
