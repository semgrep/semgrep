(* Shortcut for xs =*= [].
 * It is not that shorter, but it avoids to use =*= which helps
 * to reduce the number of places to look for possible problems with =*=.
 *)
val null : 'a list -> bool

val map : ('a -> 'b) -> 'a list -> 'b list
(** Same as [List.map] but stack-safe and slightly faster on short lists.
    Additionally, we guarantee that the mapping function is applied from
    left to right like for [List.iter].
*)

(* List_.hd_exn msg []' will raise
   the exception 'Failure msg' which is only a slight improvement over
   'List_.hd_exn "unexpected empty list"'.

   In general, you should prefer a match-with and not have to call a
   function to extract the first element of a list.

   Usage: List_.hd_exn "found an empty list of things" xs

   If receiving an empty list is a bug, prefer the following:

     match xs with
     | [] -> assert false
     | xs -> ...
*)
val hd_exn : string -> 'a list -> 'a

(* The same recommendations as for 'List_.hd_exn "unexpected
   empty list"' apply. *)
val tl_exn : string -> 'a list -> 'a list

(* The same recommendations as for 'List_.hd_exn "unexpected empty list" apply. *)
val last_exn : string -> 'a list -> 'a

val last_opt : 'a list -> 'a option
(** Returns the last element of the list or none if the list is empty. *)
    
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
(** Same as [List.map2] but stack-safe and slightly faster on short lists.
    Additionally, we guarantee that the mapping function is applied from
    left to right like for [List.iter].
*)

val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
(** Same as [List.mapi] but stack-safe and slightly faster on short lists.
    Additionally, we guarantee that the mapping function is applied from
    left to right like for [List.iter].
*)

val flatten : 'a list list -> 'a list
(** Same as [List.flatten] but tail recursive. *)

(* opposite of List.filter *)
val exclude : ('a -> bool) -> 'a list -> 'a list

(* Sort in a polymorphic way. You should really use 'deriving ord' instead *)
val sort : 'a list -> 'a list
val uniq_by : ('a -> 'a -> bool) -> 'a list -> 'a list

(* options and lists *)

val map_filter : ('a -> 'b option) -> 'a list -> 'b list
(** Same as [List.filter_map] but tail recursive. *)

val find_some : ('a -> 'b option) -> 'a list -> 'b
val find_some_opt : ('a -> 'b option) -> 'a list -> 'b option
val filter_some : 'a option list -> 'a list
val optlist_to_list : 'a list option -> 'a list

(* Haskell-inspired list combinators (take/drop/span) *)

(* this may raise Failure "List_.take: not enough" *)
val take : int -> 'a list -> 'a list

(* this does not raise a Failure and take only the first n elements *)
val take_safe : int -> 'a list -> 'a list

(* this may raise Failure "drop: not enough" *)
val drop : int -> 'a list -> 'a list
val span : ('a -> bool) -> 'a list -> 'a list * 'a list

(* zip a list with an increasing list of numbers.
 * e.g., index_list ["a"; "b"] -> ["a", 0; "b", 1].
 * An alternative is to use functions like List.iteri.
 *)
val index_list : 'a list -> ('a * int) list
val index_list_0 : 'a list -> ('a * int) list

(* similar to index_list_0 but start the index at 1 *)
val index_list_1 : 'a list -> ('a * int) list
val join_gen : 'a -> 'a list -> 'a list
val enum : int -> int -> int list
