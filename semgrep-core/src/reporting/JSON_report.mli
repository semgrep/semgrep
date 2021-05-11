(*s: semgrep/reporting/JSON_report.mli *)

(*s: signature [[JSON_report.match_to_json]] *)
(* Can return an Error because when have a NoTokenLocation exn when
 * trying to get the range of a match or metavar.
*)
val match_to_json: Pattern_match.t -> (JSON.t, Error_code.error) Common.either
(*e: signature [[JSON_report.match_to_json]] *)

(* takes the starting time of the program *)
val json_of_profile_info: float -> JSON.t

val json_of_exn: exn -> JSON.t

val json_fields_of_matches_and_errors:
  Common.filename list ->
  Report.rule_result ->
  (string * JSON.t) list

(*s: signature [[JSON_report.match_to_error]] *)
(* internal *)
val match_to_error: Pattern_match.t -> unit
(*e: signature [[JSON_report.match_to_error]] *)

(*e: semgrep/reporting/JSON_report.mli *)
