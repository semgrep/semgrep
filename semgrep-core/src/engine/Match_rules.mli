(*
   Return matches, errors, match time.

   This will run the search-mode and taint-mode rules.
*)
val check :
  match_hook:
    (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t * Equivalence.equivalences ->
  Rule.rules ->
  Xtarget.t ->
  Report.times Report.match_result
