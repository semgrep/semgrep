
type program2 = Ast_web.web_document * Ast_web.token list

exception Parse_error of Parse_info.t

(* This is the main function *)
val parse:
  Common.filename -> program2
