(*s: semgrep/tainting/Tainting_generic.mli *)
(*s: signature [[Tainting_generic.check]] *)
val check :
  (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  (Rule.rule * Rule.taint_spec) list ->
  Common.filename ->
  Target.t ->
  Pattern_match.t list

(*e: signature [[Tainting_generic.check]] *)
(*e: semgrep/tainting/Tainting_generic.mli *)
