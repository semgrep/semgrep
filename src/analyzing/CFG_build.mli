(* Compute the control flow graph of a sequence of statements.
 * This sequence of statements doesn't necessarily have to be
 * a function body, so no implicit return analysis is done here.
 *
 * This is useful in cases such as handling top-level functions
 * which aren't real functions.
 *)
val cfg_of_stmts : ?tok:Tok.t -> IL.stmt list -> IL.cfg

val cfg_of_fdef : ?tok:Tok.t -> IL.function_definition -> IL.fdef_cfg
(** Compute the control flow graph of a function definition.
 * Implicit return analysis is applied and for languages that support
 * implicit return, the returning expressions will be converted to
 * explicit return nodes in the CFG.
 *)

val cfg_of_gfdef :
  Lang.t -> ?ctx:AST_to_IL.ctx -> AST_generic.function_definition -> IL.fdef_cfg
(** Same as 'cfg_of_fdef' but takes a Generic function definition. *)
