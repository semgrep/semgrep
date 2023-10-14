(* Compute the control flow graph of a sequence of statements.
 * This sequence of statements doesn't necessarily have to be
 * a function body, so no implicit return analysis is done here.
 *)
val cfg_of_stmts : IL.stmt list -> IL.cfg

(* The CFG for a single function where `fparams` is the parameter list,
 * and `fcfg` is the CFG itself.
 *)
type fdef_cfg = { fparams : IL.name list; fcfg : IL.cfg }

(* Compute the control flow graph of a function definition.
 * Implicit return analysis is applied and for languages that support
 * implicit return, the returning expressions will be converted to
 * explicit return nodes in the CFG.
 *)
val cfg_of_fdef : Lang.t -> AST_generic.function_definition -> fdef_cfg
