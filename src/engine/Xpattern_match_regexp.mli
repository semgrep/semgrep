val matches_of_regexs :
  (Regexp_engine.t * Xpattern.pattern_id * string) list ->
  string Lazy.t ->
  Common.filename ->
  Core_result.times Core_result.match_result
