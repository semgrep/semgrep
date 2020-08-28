
(* only for Function, Class (merged with Interface),
 * Method (merged with StaticMethod), Constant for now. 
 * 
 * The 'name option' is For Method where it can be useful
 * to also have the name of the enclosing entity (this is useful in 
 * tags_php.ml for instance)
 *)
type def = 
  Cst_php.ident * Cst_php.ident option * Entity_code.entity_kind

(* only for Function, Class uses for now *)
type use = 
  Cst_php.name * Entity_code.entity_kind

val defs_of_any: Cst_php.any -> def list

(* pre: unsugar_self_parent 
 * todo: works only for functions and classes right now.
*)
val uses_of_any: ?verbose:bool -> Cst_php.any -> use list
