
type annotation =
  | ProvidesModule of Module_pre_es6.module_
  | ProvidesLegacy of Module_pre_es6.module_
  | RunWhenReady
  | Other of string

(* The returned parse_info is the one associated with the whole comment.
 * We use it in the tag generation.
*)
val annotations_of_program_with_comments:
  (Ast_js.a_program, Parser_js.token) Parse_info.parsing_result ->
  (annotation * Parse_info.t) list

(* Helper. The string is the string of a comment (with its markers). *)
val extract_annotations: string -> annotation list
