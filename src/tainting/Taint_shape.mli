(** Operations on taint shapes (shape types are defined in 'Taint_types'). *)

open Shape_and_sig.Shape

val taints_and_shape_are_relevant : Taint.taints -> shape -> bool
(** [true] iff the union of [taints] and [gather_all_taints_in_shape shape]
 * is non-empty, or if [shape] contains a cleaned offset. *)

val fix_poly_taint_with_offset :
  Taint.offset list -> Taint.taints -> Taint.taints
(** Fix taints with an offset. It just attaches the offset to each polymorphic
    taint variable (see 'Taint.Var') in the set.

    FEATURE(field-sensitivity) *)

val tuple_like_obj : (Taint.taints * shape) list -> shape
(** Constructs a 0-indexed tuple-like 'obj' from a list of pairs, taints and shape,
 * for each element in the tuple.  *)

(* THINK: Replace polymorphic variant with a parameterized IL.field_or_entry ? *)
val record_or_dict_like_obj :
  [< `Entry of IL.exp * Taint.taints * shape
  | `Field of IL.name * Taint.taints * shape
  | `Spread of shape ]
  list
  (** see 'IL.field_or_entry' *) ->
  shape
(** Constructs an 'Obj' shape from a list of taints and shapes associated with
    a record/dict expression. *)

val unify_cell : cell -> cell -> cell
(** Unify two 'cell's into one. *)

val unify_shape : shape -> shape -> shape
(** Unify two 'shapes's into one. *)

val gather_all_taints_in_cell : cell -> Taint.taints
(** Gather and union all taints reachable through a cell. *)

val gather_all_taints_in_shape : shape -> Taint.taints
(** Gather and union all taints reachable through a shape. *)

val find_in_cell :
  Taint.offset list ->
  cell ->
  [ `Found of cell
  | `Clean
  | `Not_found of Taint.taints * shape * Taint.offset list ]
(** Find an offset in a cell.

    This is a somewhat "low-level" version.

    If the offset could not be found in the cell, then it returns `Not_found
    with the base taints and shape for the offset prefix that was found, and
    the offset suffix that was not.

    For example, given this shape:

        Cell(`None, Obj {
                .a -> Cell({"taint"}, Obj {
                        .u -> Cell(`Clean, _|_)
                        })
                })

    with the offset .a we get:

        `Found (Cell({"taint"}, Obj { .u -> Cell(`Clean, _|_) }))

    with the offset .a.u we get:

        `Clean

    and with the offset .a.v we get:

        `Not_found({"taint"}, Obj { .u -> Cell(`Clean, _|_) }, .v)
  *)

val find_in_cell_poly :
  Taint.offset list -> cell -> (Taint.taints * shape) option
(** Find an offset in a cell, BUT if the full offset cannot be found, then
    it returns the taints of the offset prefix that was found; and if those
    taints are polymorphic, then it adds to them the remaining offset.

    For example, if `x` is tainted but `x.a` is not being tracked, it just
    assigns to `x.a` the same taints as `x`. If `x` had polymorphic taint
    (see 'Taint.Var'), then it would attach the offset `.a` to it.

    TODO: We need to fix polymorphic shapes too instad of just returning 'Bot'.

    FEATURE(field-sensitivity) *)

val find_in_shape_poly :
  taints:Taint.taints ->
  Taint.offset list ->
  shape ->
  (Taint.taints * shape) option
(** Like 'find_in_cell_poly' but to find an offset in a shape, the 'taints'
    are the "base taints" in case the offset cannot be found. *)

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
