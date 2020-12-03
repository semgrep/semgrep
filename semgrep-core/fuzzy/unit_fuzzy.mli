(* subsystems unittest *)
val sgrep_fuzzy_unittest:
  ast_fuzzy_of_string:(string -> Ast_fuzzy.trees) ->
  OUnit.test

val spatch_fuzzy_unittest:
  ast_fuzzy_of_string:(string -> Spatch_fuzzy.pattern) ->
  parse_file:(string -> Ast_fuzzy.trees *
                        (Parse_info.token_kind * Parse_info.t) list) ->
  OUnit.test
