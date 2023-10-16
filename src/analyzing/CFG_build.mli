val cfg_of_stmts : IL.stmt list -> IL.cfg

(* The CFG for a single function where `fparams` is the parameter list,
 * and `fcfg` is the CFG itself.
 *)
type fun_cfg = { fparams : IL.name list; fcfg : IL.cfg }

(* Given the language and a program ast, compute the control flow
 * graphs of all the functions in that program represented as a tuple
 *     fun_cfg list * fun_cfg
 * where the first element of the tuple is the list of CFGs of all functions
 * in the program, and the second element is the CFG of the program itself
 * when considered a top-level function.
 *)
val cfgs_in_program : Lang.t -> AST_generic.program -> fun_cfg list * fun_cfg
