type parsing_result = {
  ast : AST_generic.program;
  (* Partial errors while parsing the file (tree-sitter only) *)
  skipped_tokens : Parse_info.token_location list;
  stat : Parse_info.parsing_stat;
}

(* This uses a pfff or tree-sitter parser, or both.
 * This also resolve names and propagate constants.
 *
 * Note that the errors in parsing_result are partial errors
 * that tree-sitter was able to recover from. For other parse errors,
 * the function below will actually raise an exception
 * (e.g. Parse_info.Pasing_error).
 *)
val parse_and_resolve_name : Lang.t -> Common.filename -> parsing_result
val just_parse_with_lang : Lang.t -> Common.filename -> parsing_result
val parse_program : Common.filename -> AST_generic.program

(* will print on stderr in the file was not fully parsed *)
val parse_and_resolve_name_warn_if_partial :
  Lang.t -> Common.filename -> AST_generic.program

(* raise Failure "..." if the file was not fully parsed *)
val parse_and_resolve_name_fail_if_partial :
  Lang.t -> Common.filename -> AST_generic.program

(* returns a Output_from_core.PartialParsing error *)
val errors_from_skipped_tokens :
  Parse_info.token_location list -> Report.ErrorSet.t

(* used by Parse_pattern *)
val lang_to_python_parsing_mode : Lang.t -> Parse_python.parsing_mode

(* used by Parse_jsonnet *)
val error_of_tree_sitter_error :
  Tree_sitter_run.Tree_sitter_error.t -> Exception.t
