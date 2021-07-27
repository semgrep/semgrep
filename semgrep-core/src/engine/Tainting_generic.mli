(*s: semgrep/tainting/Tainting_generic.mli *)
(*s: signature [[Tainting_generic.check]] *)
val check :
  (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t ->
  (Rule.rule * Rule.taint_spec) list ->
  Equivalence.equivalences ->
  Common.filename ->
  Target.t ->
  Pattern_match.t list

(*e: signature [[Tainting_generic.check]] *)
(*e: semgrep/tainting/Tainting_generic.mli *)
