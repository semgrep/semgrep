(*s: semgrep/tainting/Tainting_generic.mli *)
(*s: signature [[Tainting_generic.check]] *)
val check :
  Tainting_rule.rules -> Common.filename -> Target.t -> Pattern_match.t list

(*e: signature [[Tainting_generic.check]] *)
(*e: semgrep/tainting/Tainting_generic.mli *)
