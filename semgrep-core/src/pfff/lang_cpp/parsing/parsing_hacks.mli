
(* will among other things interally call pp_token.ml to expand some macros *)
val fix_tokens:
  macro_defs:(string, Pp_token.define_body) Hashtbl.t ->
  Flag_parsing_cpp.language ->
  Parser_cpp.token list -> Parser_cpp.token list
