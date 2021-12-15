(*
   Check tainting-mode rules.
   Return matches, errors, match time.
*)
val check :
  match_hook:
    (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t * Equivalence.equivalences ->
  (Rule.rule * Rule.taint_spec) list ->
  File_and_more.t ->
  Report.times Report.match_result

(* used by testing code *)
val check_bis :
  match_hook:
    (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t * Equivalence.equivalences ->
  (Rule.rule * Rule.taint_spec) list ->
  Common.filename ->
  Lang.t ->
  Target.t ->
  Pattern_match.t list
