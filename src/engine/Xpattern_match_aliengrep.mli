(*
   Wrapper around the aliengrep matcher (a generic mode variant)
*)
val matches_of_aliengrep :
  (Aliengrep.Pat_compile.t * Xpattern.pattern_id * string) list ->
  string Lazy.t ->
  Common.filename ->
  Core_profiling.times Core_result.match_result
