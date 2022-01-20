(*
   Check tainting-mode rules.
   Return matches, errors, match time.
*)
val check :
  match_hook:
    (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t * Equivalence.equivalences ->
  (Rule.rule * Rule.taint_spec) list ->
  Xtarget.t ->
  Report.rule_profiling Report.match_result list

(* used by testing code *)
val check_bis :
  match_hook:
    (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t * Equivalence.equivalences ->
  Rule.rule * Rule.taint_spec ->
  Common.filename ->
  Lang.t ->
  Target.t ->
  Pattern_match.t list
