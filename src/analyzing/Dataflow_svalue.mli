(** Dataflow S-value analysis (constant and symbolic propagation) *)

type mapping = AST_generic.svalue Dataflow_var_env.mapping

(* Indicates guarantees on the return value of a function *)
(* TODO: This should be AST_generic.svalue, if we need something more complex,
 * then we build it on top of the svalue type. *)
type constness = Constant | NotConstant [@@deriving show]

val hook_constness_of_function :
  (AST_generic.expr -> constness option) option ref

val is_symbolic_expr : AST_generic.expr -> bool
val eq : AST_generic.svalue -> AST_generic.svalue -> bool
val union : AST_generic.svalue -> AST_generic.svalue -> AST_generic.svalue

val fixpoint : Lang.t -> IL.name list -> IL.cfg -> mapping
(** Flow-sensitive constant-propagation.
 * !Note that this assumes Naming_AST.resolve has been called before!
*)

val set_svalue_ref : AST_generic.id_info -> AST_generic.svalue -> unit

val update_svalue : IL.cfg -> mapping -> unit
(**
 * Updates the [IL.lval.svalue] refs according to the mapping.
 * Note that the svalue refs in IL are shared with the Generic AST, so
 * running this analysis also updates svalue info in the Generic AST.
 * The update respects previous constant propagation passes, updating
 * svalue info when we have deduced more specific facts, but leaving it
 * untouched otherwise.
*)
