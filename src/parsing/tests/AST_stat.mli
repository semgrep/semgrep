(*
   Estimate the fraction of untranslated nodes in the generic AST.
*)

type t = Parsing_stat.ast_stat

val stat : AST_generic.program -> t
