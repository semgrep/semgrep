(* This allows us to dyamically set the Jsonnet parser, which is necessary
 * in order for semgrep.js to work because Semgrep creates the Jsonnet parser
 * on startup.
 *)
val jsonnet_parser_ref :
  (Fpath.t -> (AST_jsonnet.expr, unit) Tree_sitter_run.Parsing_result.t) ref

(* This should really be in ../parsing/ but that would lead to circular
 * dependencies because we would use Parse_jsonnet_tree_sitter.ml which
 * itself use AST_jsonnet.ml.
 *)
val parse_program : Fpath.t -> AST_jsonnet.program
