(*s: semgrep/matching/Semgrep_generic.mli *)

(*s: signature [[Semgrep_generic.check]] *)
val check:
  hook:(Metavars_generic.metavars_binding -> Parse_info.t list Lazy.t -> unit)
  ->
  with_caching:bool ->
  Rule.rules ->
  Equivalence.equivalences ->
  Common.filename ->
  Lang.t ->
  AST_generic.program ->
  Match_result.t list
(*e: signature [[Semgrep_generic.check]] *)

val last_matched_rule: Rule.t option ref

(*s: type [[Semgrep_generic.matcher]] *)
type ('a, 'b) matcher = 'a -> 'b -> Matching_generic.tout
(*e: type [[Semgrep_generic.matcher]] *)

(* used by tainting *)

(*s: signature [[Semgrep_generic.match_e_e]] *)
val match_e_e:
  Rule.t ->
  Matching_generic.tout Caching.Cache.t option ->
  (AST_generic.expr, AST_generic.expr) matcher
(*e: signature [[Semgrep_generic.match_e_e]] *)

(*s: signature [[Semgrep_generic.match_any_any]] *)
(* for unit testing *)
val match_any_any:
  Matching_generic.tout Caching.Cache.t option ->
  (AST_generic.any, AST_generic.any) matcher
(*e: signature [[Semgrep_generic.match_any_any]] *)

(*e: semgrep/matching/Semgrep_generic.mli *)
