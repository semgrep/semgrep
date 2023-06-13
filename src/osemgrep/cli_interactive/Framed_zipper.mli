(* This module implements something I call a "framed zipper".

   A zipper is the classic data structure for a list's "one hole context",
   of traversing a list via being located at a single point.
   https://en.wikipedia.org/wiki/Zipper_(data_structure)

   A "framed zipper" is a zipper which also has a "frame", which
   is of a certain size, and a pointer which can move around the
   frame freely. Essentially, a framed zipper is not just located
   at a single element in the list, but a frame of many such
   elements.

   The main use case here is for scrolling purposes on a list of
   n entries with a frame of length m. In this case, we want to
   keep a pointer around so we can move around on the entries we
   already see, but we also want to be able to move elements
   through the zipper, for when the elements in our frame change.

   So for instance, we might have a zipper like:

   A
   B <- only these
   C <- are in
   D <- our frame
   E

   Our pointer moves freely between B,C,D, but if we try to go
   up, the whole zipper moves, and we get:

   A <- only these
   B <- are in
   C <- our frame
   D
   E
*)

type 'a t

(* creators *)
(* A move necessitates a pointer move, which may or may
   not cause a frame move, depending on if the pointer is
   at the boundaries of the frame.
*)
val move_up : 'a t -> 'a t
val move_down : 'a t -> 'a t
val of_list : int -> 'a list -> 'a t

(* Append only modifies the data in the zipper, it doesn't
   affect the current position or the frame.
*)
val append : 'a -> 'a t -> 'a t
val empty_with_max_len : int -> 'a t
val change_max_len : 'a t -> int -> 'a t
val map_current : ('a -> 'a) -> 'a t -> 'a t

(* destructors *)

(* Get the next n elements from the zipper. Doesn't need
   to be within the frame.
*)
val take : int -> 'a t -> 'a list
val get_current : 'a t -> 'a
val is_empty : 'a t -> bool
val length : 'a t -> int

(* The absolute position is with respect to the number of
   things in the frame, including stufff behind us.
   The relative position is only with respect to the current
   frame.
   0-indexed.
*)
val absolute_position : 'a t -> int
val relative_position : 'a t -> int
val show : ('a -> string) -> 'a t -> string
