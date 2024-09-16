(** Operations on taint shapes (shape types are defined in 'Taint_types'). *)

open Shape_and_sig.Shape

val taints_and_shape_are_relevant : Taint.taints -> shape -> bool
(** [true] iff the union of [taints] and [gather_all_taints_in_shape shape]
 * is non-empty, or if [shape] contains a cleaned offset. *)

val tuple_like_obj : (Taint.taints * shape) list -> obj
(** Constructs a 0-indexed tuple-like 'obj' from a list of pairs, taints and shape,
 * for each element in the tuple.  *)

val unify_cell : cell -> cell -> cell
(** Unify two 'cell's into one. *)

val unify_shape : shape -> shape -> shape
(** Unify two 'shapes's into one. *)

val gather_all_taints_in_cell : cell -> Taint.taints
(** Gather and union all taints reachable through a cell. *)

val gather_all_taints_in_shape : shape -> Taint.taints
(** Gather and union all taints reachable through a shape. *)

val find_in_cell : Taint.offset list -> cell -> cell option
val find_in_shape : Taint.offset list -> shape -> cell option

val update_offset_in_cell :
  f:(Xtaint.t -> shape -> Xtaint.t * shape) ->
  Taint.offset list ->
  cell ->
  cell option

val update_offset_and_unify :
  Taint.taints -> shape -> Taint.offset list -> cell option -> cell option
(** Given a 'cell' and an 'offset', it finds the corresponding sub-'cell'
 * for that 'offset', and it updates its 'taints' and 'shape'. If no 'cell'
 * is given (i.e. 'None'), it creates a fresh one. If 'taints' are empty
 * and 'shape' is 'Bot', it just returns the given 'cell' (or 'None'). *)

val clean_cell : Taint.offset list -> cell -> cell
(** [clean_cell offset cell] marks the 'offset' in 'cell' as clean.  *)

val enum_in_cell : cell -> (Taint.offset list * Taint.taints) Seq.t
(**
 * Enumerate all offsets in a cell and their taint.
 *
 * For example,
 *
 *     enum_in_cell (cell<0>( obj {| a: cell<{"tainted"}>(_|_) |} ))
 *
 * would return a sequence with the pair (.a, "tainted").
 *)
