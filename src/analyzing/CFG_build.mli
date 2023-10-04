val cfg_of_stmts : IL.stmt list -> IL.cfg

(* Given the language and a program ast, compute the control flow
 * graphs of all the functions in that program.
 *
 * The program itself is also considered a top-level function and
 * included in the result.
 *
 * For each function, returns a pair of the its parameters and its
 * control flow graph.
 *)
val cfgs_in_program :
  Lang.t -> AST_generic.program -> (IL.name list * IL.cfg) list
