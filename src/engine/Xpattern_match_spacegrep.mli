val matches_of_spacegrep :
  Match_env.xconfig ->
  (Spacegrep.Pattern_AST.t * Xpattern.pattern_id * string) list ->
  Fpath.t ->
  Origin.t ->
  Core_profiling.times Core_result.match_result
