val matches_of_regexs :
  (Regexp_engine.t * Xpattern.pattern_id * string) list ->
  string Lazy.t ->
  Common.filename ->
  Report.times Report.match_result
