val matches_of_spacegrep :
  Match_env.xconfig ->
  (Spacegrep.Pattern_AST.t * Xpattern.pattern_id * string) list ->
  Common.filename ->
  Report.times Report.match_result
