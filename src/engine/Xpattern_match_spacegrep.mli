val matches_of_spacegrep :
  Match_env.xconfig ->
  (Spacegrep.Pattern_AST.t * Xpattern.pattern_id * string) list ->
  string (* filename *) ->
  Core_profiling.times Core_result.match_result
