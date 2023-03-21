type 'ast parser =
  | Pfff of (Common.filename -> 'ast * Parsing_stat.t)
  | TreeSitter of (Common.filename -> 'ast Tree_sitter_run.Parsing_result.t)

(* usage:
    run file [
        TreeSitter (Parse_typescript_tree_sitter.parse);
        Pfff (throw_tokens Parse_js.parse);
     ] Js_to_generic.program
*)
val run :
  Common.filename ->
  'ast parser list ->
  ('ast -> AST_generic.program) ->
  Parsing_result2.t

(* helpers used both in Parse_target.ml and Parse_target2.ml *)

val exn_of_loc : Parse_info.token_location -> Exception.t

val loc_of_tree_sitter_error :
  Tree_sitter_run.Tree_sitter_error.t -> Parse_info.token_location

(* used by Parse_jsonnet *)
val error_of_tree_sitter_error :
  Tree_sitter_run.Tree_sitter_error.t -> Exception.t

val throw_tokens :
  (Common.filename -> ('ast, 'toks) Parsing_result.t) ->
  Common.filename ->
  'ast * Parsing_stat.t

val run_external_parser :
  Common.filename ->
  (Common.filename -> AST_generic.program Tree_sitter_run.Parsing_result.t) ->
  Parsing_result2.t
