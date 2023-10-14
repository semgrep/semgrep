(* True when the given language supports implicit returns. *)
val lang_supports_implicit_return : Lang.t -> bool

(* For the given CFG, mark all expression nodes that may be executed before
 * exiting a function as "returning nodes".
 *)
val mark_implicit_return_nodes : IL.cfg -> unit
