(*s: semgrep/engine/Match_rules.mli *)

(*
   Return matches, errors, match time.
*)
val check :
  (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t ->
  Rule.rules ->
  Equivalence.equivalences ->
  Common.filename * Rule.xlang * (Target.t * Error_code.error list) Lazy.t ->
  Report.times Report.match_result

(*e: semgrep/engine/Match_rules.mli *)
