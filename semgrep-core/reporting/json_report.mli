(*s: semgrep/reporting/json_report.mli *)

(*s: signature [[Json_report.match_to_json]] *)
val match_to_json: Match_result.t -> Json_type.t
(*e: signature [[Json_report.match_to_json]] *)

(*s: signature [[Json_report.match_to_error]] *)
(* internal *)
val match_to_error: Match_result.t -> unit
(*e: signature [[Json_report.match_to_error]] *)

(*e: semgrep/reporting/json_report.mli *)
