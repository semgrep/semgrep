(** Dataflow S-value analysis (constant and symbolic propagation) *)

type mapping = AST_generic.svalue Dataflow_var_env.mapping

val hook_constness_of_function :
  (AST_generic.expr -> AST_generic.svalue option) option ref

val hook_transfer_of_assume :
  (bool ->
  IL.exp_kind ->
  AST_generic.svalue Dataflow_var_env.t ->
  AST_generic.svalue Dataflow_var_env.t)
  option
  ref

val is_symbolic_expr : AST_generic.expr -> bool
(*
val eq : AST_generic.svalue -> AST_generic.svalue -> bool
val union : AST_generic.svalue -> AST_generic.svalue -> AST_generic.svalue
*)

val fixpoint : Lang.t -> IL.fun_cfg -> mapping
(** Flow-sensitive constant-propagation.
 * !Note that this assumes Naming_AST.resolve has been called before!
*)

val set_svalue_ref : AST_generic.id_info -> AST_generic.svalue -> unit

val update_env_with :
  AST_generic.svalue Dataflow_var_env.t ->
  IL.name ->
  AST_generic.svalue ->
  AST_generic.svalue Dataflow_var_env.t

val update_svalue : IL.cfg -> mapping -> unit
(**
 * Updates the [IL.lval.svalue] refs according to the mapping.
 * Note that the svalue refs in IL are shared with the Generic AST, so
 * running this analysis also updates svalue info in the Generic AST.
 * The update respects previous constant propagation passes, updating
 * svalue info when we have deduced more specific facts, but leaving it
 * untouched otherwise.
*)
