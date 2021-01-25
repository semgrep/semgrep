(*s: semgrep/reporting/JSON_report.mli *)

(*s: signature [[JSON_report.match_to_json]] *)
(* Can return an Error because when have a NoTokenLocation exn when
 * trying to get the range of a match or metavar.
*)
val match_to_json: Pattern_match.t -> (JSON.t, Error_code.error) Common.either
(*e: signature [[JSON_report.match_to_json]] *)

(*s: signature [[JSON_report.match_to_error]] *)
(* internal *)
val match_to_error: Pattern_match.t -> unit
(*e: signature [[JSON_report.match_to_error]] *)

(*e: semgrep/reporting/JSON_report.mli *)
