(* parse_and_resolve_name uses a pfff or tree-sitter parser, or both.
 * The function also resolve names (hence the name) and propagate constants.
 *
 * Note that the errors in Parsing_result2.t are partial errors
 * that tree-sitter was able to recover from. For other parse errors,
 * the function actually raises an exception (e.g. Parse_info.Pasing_error).
 *)
val parse_and_resolve_name : Lang.t -> Common.filename -> Parsing_result2.t

(* no naming, just parsing *)
val just_parse_with_lang : Lang.t -> Common.filename -> Parsing_result2.t

(* used in test code *)
val parse_program : Common.filename -> AST_generic.program

(* will print on stderr if the file was not fully parsed *)
val parse_and_resolve_name_warn_if_partial :
  Lang.t -> Common.filename -> AST_generic.program

(* raise Failure "..." if the file was not fully parsed *)
val parse_and_resolve_name_fail_if_partial :
  Lang.t -> Common.filename -> AST_generic.program

(* to be set dynamically with the right set of language parsers.
 * hack to reduce the size of the engine.js file.
 *)
val just_parse_with_lang_ref :
  (Lang.t -> Common.filename -> Parsing_result2.t) ref

(* returns a Output_from_core.PartialParsing error *)
val errors_from_skipped_tokens : Tok.location list -> Core_error.ErrorSet.t
