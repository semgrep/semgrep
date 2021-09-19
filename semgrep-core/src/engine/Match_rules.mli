(*
   Check search-mode rules.
   Return matches, errors, match time.

   NOTE: We used to filter irrelevant rules here, but now this is done in
        Run_rules.check! If you call this function directly, there is no
        filtering of irrelevant rules.
*)
val check :
  (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t ->
  (Rule.rule * Rule.pformula) list ->
  Equivalence.equivalences ->
  File_and_more.t ->
  Report.times Report.match_result

val matches_of_formula :
  Config_semgrep_t.t ->
  Equivalence.equivalences ->
  Rule.rule_id ->
  File_and_more.t ->
  Rule.formula ->
  Range_with_metavars.t option ->
  Report.times Report.match_result * Range_with_metavars.ranges
