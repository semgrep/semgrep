module LvalOrdered : sig
  type t = IL.lval

  val compare : t -> t -> int
end

val string_of_lval : IL.lval -> string
val lval_is_var_and_dots : IL.lval -> bool
val lval_is_dotted_prefix : IL.lval -> IL.lval -> bool
val lval_of_instr_opt : IL.instr -> IL.lval option
val lvar_of_instr_opt : IL.instr -> IL.name option
val rlvals_of_node : IL.node_kind -> IL.lval list
