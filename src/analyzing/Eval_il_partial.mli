type env

val mk_env : Lang.t -> AST_generic.svalue Dataflow_var_env.t -> env
val eval : env -> IL.exp -> AST_generic.svalue

(* internals used also in Dataflow_svalue.ml *)
val eval_concat : env -> IL.exp list -> AST_generic.svalue

(* lattice ops *)
val refine : AST_generic.svalue -> AST_generic.svalue -> AST_generic.svalue
val union : AST_generic.svalue -> AST_generic.svalue -> AST_generic.svalue
val eq : AST_generic.svalue -> AST_generic.svalue -> bool
