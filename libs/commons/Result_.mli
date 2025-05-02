(*
   Same as 'Either_.partition' but operates on the standard type
   'result' (Ok or Error).
*)
val partition :
  ('a -> ('ok, 'error) result) -> 'a list -> 'ok list * 'error list

val collect : ('a, 'e) result list -> ('a list, 'e) result
(** Distributes out the successful results if zero errors inhabit the supplied
  * list.  If multiple errors inhabit the list, all but one is discarded. *)

module Operators : sig
  val ( >>= ) :
    ('a, 'error) result -> ('a -> ('b, 'error) result) -> ('b, 'error) result
end

val transpose_result_option :
  ('a, 'err) result option -> ('a option, 'err) result
(** [transpose_result_option x] transposes an optional result into a
    result of an option. *)
