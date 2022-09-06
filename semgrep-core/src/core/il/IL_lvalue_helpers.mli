(** Lvalue/Rvalue helpers working on the IL *)

val lval_is_var_and_dots : IL.lval -> bool
(** Test whether an lvalue is of the form x.a_1. ... .a_n *)

val lval_is_dotted_prefix : IL.lval -> IL.lval -> bool
(** [lval_is_dotted_prefix lval1 lval2] tests whether [lval1] is of the
    form x.a_1. ... .a_n, and whether [lval2] is an extension of [lval1],
    that is, x.a_1. ... .a_n.o_1 ... . o_M. *)

val lval_of_instr_opt : IL.instr -> IL.lval option
(** If the given instruction stores its result in [lval] then it is
    [Some lval], otherwise it is [None]. *)

val lvar_of_instr_opt : IL.instr -> IL.name option
(** If the given instruct stores its result in an lvalue of the form
    x.o_1. ... .o_N, then it is [Some x], otherwise it is [None]. *)

val rlvals_of_node : IL.node_kind -> IL.lval list
(** The lvalues that occur in the RHS of a node. *)

(** Useful to instantiate data strutures like Map and Set. *)
module LvalOrdered : sig
  type t = IL.lval

  val compare : t -> t -> int
end
