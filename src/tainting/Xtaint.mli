(** eXtended taint
 *
 * Represents the "taint status" of a variable or l-value.
 *)

type t = [ `Tainted of Taint.taints | `None  (** no info *) | `Clean ]
(** See 'Taint_shape.ref'. *)

type t_or_sanitized = [ t | `Sanitized ]
(** When checking the taint of an l-value, we want to distinguish between "clean",
 * meaning the l-value was cleaned in the past and it's marked as clean in the
 * environment, and "sanitized", meaning this very occurrence of the l-value is
 * matching a sanitizer. See NOTE [lval/sanitized] in 'Dataflow_tainting'. *)

val equal : t -> t -> bool
val compare : t -> t -> int
val show : t -> string

val union : t -> t -> t
(** Merge xtaints at JOIN nodes of the CFG. *)

val is_tainted : [> `Tainted of Taint.taints ] -> bool
val of_taints : Taint.taints -> t
val to_taints : [< t | `Sanitized ] -> Taint.taints
