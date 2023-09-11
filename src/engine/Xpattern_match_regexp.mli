(* would label these arguments, but can't to make it work with
   the other generic matchers
*)
val regexp_matcher :
  ?base_offset:int ->
  (* str *) string ->
  (* file *) string ->
  Regexp_engine.t ->
  ((Tok.location * Tok.location) * (string * Metavariable.mvalue) list) list

val matches_of_regexs :
  (Regexp_engine.t * Xpattern.pattern_id * string) list ->
  string Lazy.t ->
  Common.filename ->
  Report.times Report.match_result
