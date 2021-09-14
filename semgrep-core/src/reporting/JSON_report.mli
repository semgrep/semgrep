(*s: semgrep/reporting/JSON_report.mli *)

(*s: signature [[JSON_report.match_to_json]] *)
(* Can return an Error because when have a NoTokenLocation exn when
 * trying to get the range of a match or metavar.
 *)
val match_to_match :
  Pattern_match.t ->
  (Semgrep_core_response_t.match_, Semgrep_error_code.error) Common.either

(*e: signature [[JSON_report.match_to_json]] *)

(* takes the starting time of the program *)
val json_of_profile_info : float -> JSON.t

val match_results_of_matches_and_errors :
  Common.filename list ->
  Report.rule_result ->
  Semgrep_core_response_t.match_results

(*s: signature [[JSON_report.match_to_error]] *)
(* internal *)
val match_to_error : Pattern_match.t -> unit

(*e: signature [[JSON_report.match_to_error]] *)

(*e: semgrep/reporting/JSON_report.mli *)
