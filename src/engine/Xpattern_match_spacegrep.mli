val matches_of_spacegrep :
  Config_semgrep.t * Equivalence.equivalences ->
  (Spacegrep.Pattern_AST.t * Xpattern.pattern_id * string) list ->
  Common.filename ->
  Report.times Report.match_result
