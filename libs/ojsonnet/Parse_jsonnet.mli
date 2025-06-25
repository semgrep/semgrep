(* This should really be in ../parsing/ but that would lead to circular
 * dependencies because we would use Parse_jsonnet_tree_sitter.ml which
 * itself use AST_jsonnet.ml.
 *)
val parse_program : Fpath.t -> AST_jsonnet.program
