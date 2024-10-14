(*
   Extended module for list manipulation
*)

(*
   Shadow the original, unsafe (@) by using 'open List_.Operators'.
*)
module Operators : sig
  (* same as 'List_.append' defined below. *)
  val ( @ ) : 'a list -> 'a list -> 'a list
end

(* Stack-safe implementation of List.append aka (@).
   See also the Operators submodule which provides a safe (@). *)
val append : 'a list -> 'a list -> 'a list

(* Shortcut for xs =*= [].
 * It is not that shorter, but it avoids to use =*= which helps
 * to reduce the number of places to look for possible problems with =*=.
 *)
val null : 'a list -> bool

(* Stack-safe implementation of List.fold_right *)
val fold_right : ('elt -> 'acc -> 'acc) -> 'elt list -> 'acc -> 'acc

val map : ('a -> 'b) -> 'a list -> 'b list
(** Same as [List.map] but stack-safe and slightly faster on short lists.
    Additionally, we guarantee that the mapping function is applied from
    left to right like for [List.iter].
*)

(* Generic iteration over a list, with a view into the previous and the next
   element.

   This function isn't used very often but when it is, it's good to have it.
   It's convenient for printing lists with special handling for the first and
   last elements.

   Another potential use is for collecting or printing intervals or
   differences between successive elements.
*)
val iter_with_view_into_neighbor_elements :
  (prev:'a option -> cur:'a -> next:'a option -> unit) -> 'a list -> unit

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

val last_opt : 'a list -> 'a option
(** Returns the last element of the list or none if the list is empty. *)

val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
(** Same as [List.map2] but stack-safe and slightly faster on short lists.
    Additionally, we guarantee that the mapping function is applied from
    left to right like for [List.iter].
*)

val split : ('a * 'b) list -> 'a list * 'b list
(** Same as [List.split] but stack-safe. *)

val combine : 'a list -> 'b list -> ('a * 'b) list
(** Same as [List.combine] but stack-safe. *)

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

val sort_by_key : ('a -> 'b) -> ('b -> 'b -> int) -> 'a list -> 'a list
(** [sort_by_key key cmp xs] is [xs] sorted (in ascending order) according to
    the [cmp]-based order of [key] applied to each element.

    [key] is applied only once per element.
  *)

(* Warning: O(n^2) complexity. Prefer 'deduplicate' or 'deduplicate_gen'. *)
val uniq_by : ('a -> 'a -> bool) -> 'a list -> 'a list

(* Deduplicate a list of elements from left to right i.e. the first occurrence
   of each element is preserved.

   The 'get_key' function extracts a key that serves as the base for
   deduplication. It must be suitable for use with 'Hashtbl'.
*)
val deduplicate_gen : get_key:('a -> 'key) -> 'a list -> 'a list

(* Same as 'deduplicate_gen' but use the whole element as a key. *)
val deduplicate : 'a list -> 'a list

(* options and lists *)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
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

(* Cut the list right before the first element that doesn't match the
   predicate. *)
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
