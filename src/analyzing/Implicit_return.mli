(* True when the given language supports implicit returns. *)
val lang_supports_implicit_return : Lang.t -> bool

(* For each given CFG, mark all expression nodes that may be executed before
 * exiting a function as "returning nodes", so later on these expressions
 * can be matched with return statements.
 *)
val mark_implicit_return_nodes : CFG_build.fun_cfg list -> unit
