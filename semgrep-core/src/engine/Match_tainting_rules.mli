(*
   Check tainting-mode rules.
   Return matches, errors, match time.
*)
val check :
  (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t ->
  (Rule.rule * Rule.taint_spec) list ->
  Equivalence.equivalences ->
  File_and_more.t ->
  Report.times Report.match_result
