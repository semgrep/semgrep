type 'a t =
  | And of 'a t list  (** A conjunction of requirements. *)
  | Or of 'a t list  (** A disjunction of requirements. *)
  | Pred of 'a  (** A single requirement. *)
[@@deriving show, eq, ord, hash]

val map_opt : ('a -> 'b t option) -> 'a t -> 'b t option
(** [map_opt f formula] applies [f] to each predicate in [formula].
    If [f] returns [None] for any predicate, the behavior depends on context:
    - In AND contexts: [None] predicates are filtered out
    - In OR contexts: if any predicate maps to [None], the whole OR becomes
        [None]
    - Returns [None] if the result would be empty

    This allows various requirements to be expanded to arbitrary trees of
    requirements, or, using [None], removed. Essentially, if [f] returns [None]
    it is treated as if that requirement is always true. *)

val fold : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
(** [fold f formula acc] folds [f] over all predicates in [formula] *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f formula] applies [f] to each predicate in [formula] *)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all p formula] checks if [p] holds for all predicates in [formula] *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists p formula] checks if [p] holds for any predicate in [formula] *)

val eval : ('a -> bool) -> 'a t -> bool
(** [eval p formula] evaluates [formula] by applying predicate [p] to each leaf.
    Returns [true] if the formula is satisfied, [false] otherwise. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f formula] applies [f] to each predicate in [formula] *)

val predicates : 'a t -> 'a list
(** [predicates formula] returns a list of all predicates in [formula] *)
