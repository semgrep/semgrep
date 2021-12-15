(*
   Return matches, errors, match time.

   This will run the search-mode and taint-mode rules.
*)
val check :
  (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t ->
  Rule.rules ->
  Equivalence.equivalences ->
  File_and_more.t ->
  Report.times Report.match_result
