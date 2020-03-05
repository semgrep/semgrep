
type mvar = string 

type 'a metavars_binding = (mvar, 'a) Common.assoc

type fuzzy_binding = Ast_fuzzy.trees metavars_binding

val empty_environment: unit -> 'a metavars_binding

