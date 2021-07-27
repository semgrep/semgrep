(*s: semgrep/engine/Match_rules.mli *)

(*
   Check search-mode rules.
   Return matches, errors, match time.
*)
val check :
  (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t ->
  (Rule.rule * Rule.pformula) list ->
  Equivalence.equivalences ->
  Common.filename * Rule.xlang * (Target.t * Error_code.error list) Lazy.t ->
  Report.times Report.match_result

val matches_of_formula :
  Config_semgrep_t.t ->
  Equivalence.equivalences ->
  string ->
  Common.filename
  * Rule.xlang
  * (AST_generic.program * Error_code.error list) lazy_t ->
  string lazy_t ->
  Rule.formula ->
  Range_with_metavars.t option ->
  Report.times Report.match_result * Range_with_metavars.ranges

(*e: semgrep/engine/Match_rules.mli *)
