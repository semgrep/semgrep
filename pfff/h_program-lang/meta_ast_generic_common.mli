
(* reused in other dumpers *)
val vof_arithmetic_operator: AST_generic.operator -> OCaml.v
val vof_incr_decr: AST_generic.incr_decr -> OCaml.v
val vof_inc_dec: AST_generic.incr_decr * AST_generic.prefix_postfix -> OCaml.v
val vof_prepost: AST_generic.prefix_postfix -> OCaml.v

