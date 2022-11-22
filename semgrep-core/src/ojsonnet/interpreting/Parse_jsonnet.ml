(*
 * See https://jsonnet.org/ref/spec.html#lexing
 * and https://jsonnet.org/ref/spec.html#abstract_syntax
 *
 * This should really be in ../parsing/ but that would lead to circular dependencies
 * because we would use Parse_jsonnet_tree_sitter.ml which itself use AST_jsonnet.ml.
 *)

let parse_program (file : Common.filename) : AST_jsonnet.program =
  let res = Parse_jsonnet_tree_sitter.parse file in
  (* similar to Parse_target.run_parser and the TreeSitter case *)
  match (res.program, res.errors) with
  | Some ast, [] -> ast
  | None, [] ->
      failwith "internal error: failed to recover typed tree from treesitter"
  (* we don't care about partial parsing error; we need all the code for
   * ojsonnet; we can't be satisfied by partial code.
   *)
  | (Some _ | None), x :: _xs ->
      let e = Parse_target.error_of_tree_sitter_error x in
      Exception.reraise e
