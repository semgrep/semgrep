(*s: semgrep/reporting/JSON_report.mli *)

(*s: signature [[JSON_report.match_to_json]] *)
val match_to_json: Match_result.t -> JSON.t
(*e: signature [[JSON_report.match_to_json]] *)

(*s: signature [[JSON_report.match_to_error]] *)
(* internal *)
val match_to_error: Match_result.t -> unit
(*e: signature [[JSON_report.match_to_error]] *)

(*e: semgrep/reporting/JSON_report.mli *)
