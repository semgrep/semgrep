(* Can return an Error because when have a NoTokenLocation exn when
 * trying to get the range of a match or metavar.
 *)
val match_to_match :
  Pattern_match.t ->
  (Output_from_core_t.core_match, Semgrep_error_code.error) Common.either

val match_results_of_matches_and_errors :
  Common.filename list ->
  Report.final_result ->
  Output_from_core_t.core_match_results

(* internal *)
val match_to_error : Pattern_match.t -> unit
