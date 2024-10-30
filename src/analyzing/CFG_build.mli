val cfg_of_stmts : ?tok:Tok.t -> IL.stmt list -> IL.cfg * IL.lambdas_cfgs
(** Compute the control flow graph of a sequence of statements.
 *
 * This is useful in cases such as handling top-level instructions that
 * do not constitute actual function definitions.
 *)

val cfg_of_fdef : IL.function_definition -> IL.fun_cfg
(** Compute the control flow graph of an IL function definition. *)

val cfg_of_gfdef :
  Lang.t -> ?ctx:AST_to_IL.ctx -> AST_generic.function_definition -> IL.fun_cfg
(** Same as 'cfg_of_fdef' but takes a Generic function definition. *)
