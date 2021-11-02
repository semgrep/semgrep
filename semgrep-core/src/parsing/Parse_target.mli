type parsing_result = {
  ast : AST_generic.program;
  (* Partial errors while parsing the file (tree-sitter only) *)
  errors : Semgrep_error_code.error list;
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
val parse_and_resolve_name_use_pfff_or_treesitter :
  Lang.t -> Common.filename -> parsing_result

(* used only for testing purpose *)
val just_parse_with_lang : Lang.t -> Common.filename -> parsing_result

val parse_program : Common.filename -> AST_generic.program

(* used by Parse_pattern *)
val lang_to_python_parsing_mode : Lang.t -> Parse_python.parsing_mode
