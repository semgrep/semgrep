
(* transform TDefine, filter the TCppEscapedNewline, generate TIdentDefine
 * and other related fresh tokens.
*)
val fix_tokens_define :
  Parser_cpp.token list -> Parser_cpp.token list
