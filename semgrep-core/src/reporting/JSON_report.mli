(* Can return an Error because when have a NoTokenLocation exn when
 * trying to get the range of a match or metavar.
 *)
val match_to_match :
  Pattern_match.t ->
  (Semgrep_core_response_t.match_, Semgrep_error_code.error) Common.either

val match_results_of_matches_and_errors :
  Common.filename list ->
  Report.rule_result ->
  Semgrep_core_response_t.match_results

(* internal *)
val match_to_error : Pattern_match.t -> unit
