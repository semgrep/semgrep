val check_rule :
  match_hook:
    (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t * Equivalence.equivalences ->
  Rule.rule * Rule.taint_spec ->
  Common.filename ->
  Lang.t ->
  Target.t ->
  Pattern_match.t list
