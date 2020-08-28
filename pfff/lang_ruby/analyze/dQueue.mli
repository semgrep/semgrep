

type + 'a t

val empty : 'a t

val is_empty : 'a t -> bool

val hd : 'a t -> 'a
val tl : 'a t -> 'a t
val pop : 'a t -> 'a * 'a t

val pop_back : 'a t -> 'a t * 'a

val insert  : 'a -> 'a t -> 'a t

val enqueue : 'a -> 'a t -> 'a t

val dequeue : 'a t -> 'a * 'a t

val length : 'a t -> int

val rev : 'a t -> 'a t

val append : 'a t -> 'a t -> 'a t

val iter : ('a -> unit) -> 'a t -> unit

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val rev_map : ('a -> 'b) -> 'a t -> 'b t

val map : ('a -> 'b) -> 'a t -> 'b t

val to_list : 'a t -> 'a list

val from_list : 'a list -> 'a t

val flatten : 'a t t -> 'a t

val to_string : ('a -> string) -> 'a t -> string

val cmp : ('a -> 'a -> int) -> 'a t -> 'a t -> int
val compare : 'a t -> 'a t -> int

val gen : (?size:int -> Random.State.t -> 'a) -> ?size:int -> Random.State.t -> 'a t


type 'a cursor

val to_cursor : 'a t -> 'a cursor
val from_cursor : 'a cursor -> 'a t

val current : 'a cursor -> 'a

val at_right : 'a cursor -> bool
val at_left : 'a cursor -> bool

val move_right : 'a cursor -> 'a cursor
val move_left : 'a cursor -> 'a cursor

val goto_front : 'a cursor -> 'a cursor
val goto_back : 'a cursor -> 'a cursor


