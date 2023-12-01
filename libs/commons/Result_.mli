(*
   Same as 'Either_.partition_either' but operates on the standard type
   'result' (Ok or Error).
*)
val partition_result :
  ('a -> ('ok, 'error) result) -> 'a list -> 'ok list * 'error list
