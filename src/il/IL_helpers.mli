val is_pro_resolved_global : IL.name -> bool
(** Test whether a name is global and has been resolved by Pro-naming. *)

val is_class_name : IL.name -> bool
val exp_of_arg : IL.exp IL.argument -> IL.exp

(** Lvalue/Rvalue helpers working on the IL *)

val lval_of_var : IL.name -> IL.lval

val is_dots_offset : IL.offset list -> bool
(** Test whether an offset is of the form .a_1. ... .a_N.  *)

val lval_of_instr_opt : IL.instr -> IL.lval option
(** If the given instruction stores its result in [lval] then it is
    [Some lval], otherwise it is [None]. *)

val lvar_of_instr_opt : IL.instr -> IL.name option
(** If the given instruct stores its result in an lvalue of the form
    x.o_1. ... .o_N, then it is [Some x], otherwise it is [None]. *)

val rlvals_of_node : IL.node_kind -> IL.lval list
(** The lvalues that occur in the RHS of a node. *)

val orig_of_node : IL.node_kind -> IL.orig option

val reachable_nodes : IL.fun_cfg -> IL.node Seq.t
(** Get the reachable nodes from function's CFG, including the nodes in the lambdas' CFGs. *)

val lval_is_lambda : IL.lambdas_cfgs -> IL.lval -> (IL.name * IL.fun_cfg) option
(** Lookup an 'lval' in a 'lambdas_cfgs' table to obtain the lambda's CFG. *)
