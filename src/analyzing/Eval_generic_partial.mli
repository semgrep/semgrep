type var = string * AST_generic.sid

type env = {
  lang : Lang.t option;
  constants : (var, AST_generic.svalue) Hashtbl.t;
  attributes : (var, AST_generic.attribute list) Hashtbl.t;
}

val default_env : Language.t option -> env

(* Partially evaluate a Generic expression *)
val eval : env -> AST_generic.expr -> AST_generic.svalue option

(* helpers reused in Constant_propagation.ml *)
val is_lang : env -> Language.t -> bool
val is_js : env -> bool

val find_id :
  env -> AST_generic.ident -> AST_generic.id_info -> AST_generic.svalue option
