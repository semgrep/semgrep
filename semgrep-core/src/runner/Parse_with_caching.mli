val parse_generic :
  string (* use_parsing_cache *) ->
  string (* version *) ->
  Lang.t ->
  string (* file *) ->
  AST_generic.program * Semgrep_error_code.error list

val parse_equivalences : string -> Equivalence.equivalences

val parse_pattern : Lang.t -> string -> AST_generic.any
