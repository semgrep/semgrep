(** Instantiation of taint types. *)

open Shape_and_sig.Shape

val instantiate_taint_var :
  inst_lval:(Taint.lval -> (Taint.taints * shape) option) ->
  inst_ctrl:(unit -> Taint.taints) ->
  Taint.taint ->
  (Taint.taints * shape) option

val instantiate_taint :
  callee:IL.exp ->
  inst_lval:(Taint.lval -> (Taint.taints * shape) option) ->
  inst_ctrl:(unit -> Taint.taints) ->
  Taint.taint ->
  Taint.taints
(** Instantiate taints. Instantiation is meant to replace the taint variables
 * in the taint signature of a callee function, with the taints assigned by
 * the caller. *)

val instantiate_shape :
  callee:IL.exp ->
  inst_lval:(Taint.lval -> (Taint.taints * shape) option) ->
  inst_ctrl:(unit -> Taint.taints) ->
  shape ->
  shape
(** Instantiate a shape. Instantiation is meant to replace the taint variables
 * in the taint signature of a callee function, with the taints assigned by
 * the caller. *)

val subst_in_precondition :
  inst_lval:(Taint.lval -> (Taint.taints * shape) option) ->
  inst_ctrl:(unit -> Taint.taints) ->
  Taint.taint ->
  Taint.taint option
(** Instantiate preconditions. Instantiation is meant to replace the taint variables
 * in the taint signature of a callee function, with the taints assigned by
 * the caller. *)
