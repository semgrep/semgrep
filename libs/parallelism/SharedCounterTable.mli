(** [SharedCounterTable] is a table of different atomic counters. This is useful
    for if we want to keep track of global values by some key, such as some
    metrics *)

type ('a, 'b) t

val create : default:'b -> add:('b -> 'b -> 'b) -> int -> ('a, 'b) t
(** [create ~default ~add size] creates a new shared counter table with the
    given initial size, and it grows as needed. [default] is what a counter will
    be reset to when [reset table key] is called. [add] will be called during
    [add_and_fetch], and will be passed the current value of the counter,
    followed by the increment. Ex
    {[
     let tbl = create ~default:0 ~add:(+) 10 in
     let count = add_and_fetch tbl "key1" 5 in
     Printf.printf "Count for key1: %d\n" count;
     ]}
 *)

val create_int_table : int -> ('a, int) t
(** [create_int_table size] creates a shared counter table for integer counters,
    with an initial value of 0 and addition defined as integer addition. *)

val create_float_table : int -> ('a, float) t
(** [create_float_table size] creates a shared counter table for float counters,
    with an initial value of 0.0 and addition defined as float addition. *)

val create_float_list_table : int -> ('a, float list) t
(** [create_float_list_table size] creates a shared counter table for float
    lists, with an initial value of an empty list and addition defined as list
    concatenation. This is useful for collecting multiple float values under a
    single key. *)

val add_and_fetch : ('a, 'b) t -> 'a -> 'b -> 'b
(** [add_and_fetch counter_table key increment] [add]s [increment] to the counter
    associated with [key] in [counter_table], and returns the new value of the
    counter. If the key does not exist, it is created with an initial value of
    [default] before incrementing and returning. *)

val reset : ('a, 'b) t -> 'a -> unit
(** [reset counter_table key] resets the counter associated with [key] in
    [counter_table] to [default]. If the key does not exist, it does nothing. *)
