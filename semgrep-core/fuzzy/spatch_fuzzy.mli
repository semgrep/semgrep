
type pattern = Ast_fuzzy.trees

val parse:
  pattern_of_string:(string -> pattern) ->
  ii_of_pattern:(pattern -> Parse_info.t list) ->
  Common.filename -> pattern

(* It will modify by side effects the tokens in trees. It's up to you
 * then to unparse things in the correct way. See unparse_fuzzy.ml
 * for an example.
*)
val spatch:
  pattern -> Ast_fuzzy.trees -> bool
