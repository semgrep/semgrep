(* would label these arguments, but can't to make it work with
   the other generic matchers
*)
val regexp_matcher :
  ?base_offset:int ->
  (* str *) string ->
  Fpath.t ->
  Regex.t ->
  ((Tok.location * Tok.location) * (string * Metavariable.mvalue) list) list

val matches_of_regexs :
  (Regex.t * Xpattern.pattern_id * string) list ->
  string Lazy.t ->
  Fpath.t ->
  Origin.t ->
  Core_profiling.times Core_result.match_result
