(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A zipper on lists.

   See the .mli for more information
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type 'a t = 'a Framed_zipper.t

(* A simple library for zippers on lists.
   https://en.wikipedia.org/wiki/Zipper_(data_structure)
*)

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

let move_up = Framed_zipper.move_up
let move_down = Framed_zipper.move_down
let to_list = Framed_zipper.to_list
let of_list l = Framed_zipper.of_list 1 l
let append = Framed_zipper.append
let position = Framed_zipper.absolute_position
let length = Framed_zipper.length
let is_top = Framed_zipper.is_top
let is_bottom = Framed_zipper.is_top
let get_current = Framed_zipper.get_current
let map_current = Framed_zipper.map_current
