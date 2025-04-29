(*
   Same as 'Either_.partition' but operates on the standard type
   'result' (Ok or Error).
*)
val partition :
  ('a -> ('ok, 'error) result) -> 'a list -> 'ok list * 'error list

module Operators : sig
  val ( >>= ) :
    ('a, 'error) result -> ('a -> ('b, 'error) result) -> ('b, 'error) result
end
