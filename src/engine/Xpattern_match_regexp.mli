val regexp_matcher :
  (* big_str *) string ->
  (* file *) string ->
  Regexp_engine.t * (string * string) list ->
  ((Parse_info.token_location * Parse_info.token_location)
  * (string * Metavariable.mvalue) list)
  list

val matches_of_regexs :
  ((Regexp_engine.t * (string * string) list (* mvar renames *))
  * Xpattern.pattern_id
  * string)
  list ->
  string Lazy.t ->
  Common.filename ->
  Report.times Report.match_result
