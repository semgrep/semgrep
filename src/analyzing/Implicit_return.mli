(* True when the given language supports implicit returns. *)
val lang_supports_implicit_return : Lang.t -> bool

(* For the given AST, mark all expression nodes that may be executed before
 * exiting a function as "returning nodes".
 * See the 'is_implicit_return' expr field in AST_generic.
 *)
val mark_implicit_return : Lang.t -> AST_generic.program -> unit
