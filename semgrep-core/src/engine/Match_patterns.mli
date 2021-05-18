(*s: semgrep/engine/Match_patterns.mli *)

(*s: signature [[Semgrep_generic.check]] *)
val check :
  hook:(Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t ->
  Mini_rule.rules ->
  Equivalence.equivalences ->
  Common.filename * Lang.t * Target.t ->
  Pattern_match.t list

(*e: signature [[Semgrep_generic.check]] *)

val last_matched_rule : Mini_rule.t option ref

(*s: type [[Semgrep_generic.matcher]] *)
(*e: type [[Semgrep_generic.matcher]] *)

(* used by tainting *)

(*s: signature [[Semgrep_generic.match_e_e]] *)
val match_e_e : Mini_rule.t -> AST_generic.expr Matching_generic.matcher

(*e: signature [[Semgrep_generic.match_e_e]] *)

(*s: signature [[Semgrep_generic.match_any_any]] *)
(* for unit testing *)
val match_any_any : AST_generic.any Matching_generic.matcher

(*e: signature [[Semgrep_generic.match_any_any]] *)

(*e: semgrep/engine/Match_patterns.mli *)
