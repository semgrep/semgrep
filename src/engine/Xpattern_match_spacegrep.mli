val matches_of_spacegrep :
  Match_env.xconfig ->
  (Spacegrep.Pattern_AST.t * Xpattern.pattern_id * string) list ->
  Common.filename ->
  Core_result.times Core_result.match_result
