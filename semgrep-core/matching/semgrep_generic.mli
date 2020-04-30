
val check: 
  hook:(Metavars_generic.metavars_binding -> Parse_info.t list Lazy.t -> unit)
  ->
  Rule.rules -> Equivalence.equivalences ->
  Common.filename -> Ast_generic.program -> 
  Match_result.t list

type ('a, 'b) matcher = 'a -> 'b ->
  Metavars_generic.metavars_binding list

(* for tainting *)
val match_e_e: (Ast_generic.expr, Ast_generic.expr) matcher

(* for unit testing *)
val match_any_any: (Ast_generic.any, Ast_generic.any) matcher

