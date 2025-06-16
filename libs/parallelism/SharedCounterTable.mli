(** [SharedCounterTable] is a table of different atomic counters. This is useful
    for if we want to keep track of global values by some key, such as some
    metrics *)

type 'a t

val create : int -> 'a t
(** [create size] creates a new shared counter table with the given initial
    size, and it grows as needed *)

val add_and_fetch : 'a t -> 'a -> int -> int
(** [add_and_fetch counter_table key increment] adds [increment] to the counter
    associated with [key] in [counter_table], and returns the new value of the
    counter. If the key does not exist, it is created with an initial value of
    0 before incrementing and returning. *)

val reset : 'a t -> 'a -> unit
(** [reset counter_table key] resets the counter associated with [key] in
    [counter_table] to zero. If the key does not exist, it does nothing. *)
