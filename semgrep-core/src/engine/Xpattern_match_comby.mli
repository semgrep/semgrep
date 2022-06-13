val matches_of_combys :
  (string * Xpattern.pattern_id * string) list ->
  string Lazy.t ->
  Common.filename ->
  Report.times Report.match_result
