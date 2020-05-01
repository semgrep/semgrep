(*s: semgrep/tainting/tainting_generic.mli *)
(*s: signature [[Tainting_generic.check]] *)
val check: 
  Tainting_rule.rules ->  Common.filename -> Ast_generic.program ->
  Match_result.t list
(*e: signature [[Tainting_generic.check]] *)
(*e: semgrep/tainting/tainting_generic.mli *)
