(*s: semgrep/tainting/Tainting_generic.mli *)
(*s: signature [[Tainting_generic.check]] *)
val check: 
  Tainting_rule.rules ->  Common.filename -> AST_generic.program ->
  Match_result.t list
(*e: signature [[Tainting_generic.check]] *)
(*e: semgrep/tainting/Tainting_generic.mli *)
