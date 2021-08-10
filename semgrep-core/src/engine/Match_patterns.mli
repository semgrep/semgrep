(*s: semgrep/engine/Match_patterns.mli *)

(*s: signature [[Semgrep_generic.check]] *)
val check :
  hook:(Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  ?range_filter:(Parse_info.token_location * Parse_info.token_location -> bool) ->
  Config_semgrep.t ->
  Mini_rule.rules ->
  Equivalence.equivalences ->
  Common.filename * Lang.t * Target.t ->
  Pattern_match.t list
(** Match mini-rules (patterns) against a target file.
  *
  * @param range_filter A predicate that defines a range of interest when matching
  *    expressions. This is e.g. used for optimizing `pattern: $X` by filtering out
  *    any matches that are not within the ranges of preceding `pattern-inside`s.
  *    Typically you will check that the range of the expression is strictly inside
  *    your area(s) of interest, no need to worry about sub-expressions, they will
  *    visited regardless. *)

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
