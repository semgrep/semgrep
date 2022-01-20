(*
   Check search-mode rules.
   Return matches, errors, match time.

   NOTE: We used to filter irrelevant rules here, but now this is done in
        Match_rules.check! If you call this function directly, there is no
        filtering of irrelevant rules.
*)
val check :
  match_hook:
    (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t * Equivalence.equivalences ->
  (Rule.rule * Rule.pformula) list ->
  Xtarget.t ->
  Report.rule_profiling Report.match_result list

val matches_of_formula :
  Config_semgrep_t.t * Equivalence.equivalences ->
  Rule.rule ->
  Xtarget.t ->
  Rule.formula ->
  Range_with_metavars.t option ->
  Report.rule_profiling Report.match_result * Range_with_metavars.ranges
