(*s: semgrep/matching/semgrep_generic.mli *)

(*s: signature [[Semgrep_generic.check]] *)
val check: 
  hook:(Metavars_generic.metavars_binding -> Parse_info.t list Lazy.t -> unit)
  ->
  Rule.rules -> Equivalence.equivalences ->
  Common.filename -> Ast_generic.program -> 
  Match_result.t list
(*e: signature [[Semgrep_generic.check]] *)

(*s: type [[Semgrep_generic.matcher]] *)
type ('a, 'b) matcher = 'a -> 'b ->
  Metavars_generic.metavars_binding list
(*e: type [[Semgrep_generic.matcher]] *)

(* used by tainting *)

(*s: signature [[Semgrep_generic.match_e_e]] *)
val match_e_e: (Ast_generic.expr, Ast_generic.expr) matcher
(*e: signature [[Semgrep_generic.match_e_e]] *)

(*s: signature [[Semgrep_generic.match_any_any]] *)
(* for unit testing *)
val match_any_any: (Ast_generic.any, Ast_generic.any) matcher
(*e: signature [[Semgrep_generic.match_any_any]] *)

(*e: semgrep/matching/semgrep_generic.mli *)
