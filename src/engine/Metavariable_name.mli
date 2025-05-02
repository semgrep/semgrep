(* Check metavariable-name constraints *)

val hook_is_kind :
  (Rule.metavar_name_kind -> AST_generic.expr -> bool) option Hook.t

val hook_module_resolver : (string list -> String.t Base.List.t) option Hook.t

(* Check whether `expr` satisfies the condition described in
   `metavar_cond_name`. *)
val find_name :
  Match_env.env -> AST_generic.expr -> Rule.metavar_cond_name -> bool
