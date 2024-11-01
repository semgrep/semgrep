(* Contains the name given by the user to the timer and the time limit *)
type timeout_info

(*
   If ever caught, this exception must be re-raised immediately so as
   to not interfere with the timeout handler. See function 'set_timeout'.
*)
exception Timeout of timeout_info

(* Show name and time limit in a compact format for debugging purposes. *)
val string_of_timeout_info : timeout_info -> string

(*
   Launch the specified computation and abort if it takes longer than
   specified (in seconds).

   This uses a global timer. An Invalid_argument exception will be raised
   if the timer is already running.

   tl;dr nesting will fail
*)
val set_timeout :
  < Cap.time_limit > -> name:string -> float -> (unit -> 'a) -> 'a option

(*
   Only set a timer if a time limit is specified. Uses 'set_timeout'.
*)
val set_timeout_opt :
  name:string ->
  (float * < Cap.time_limit >) option ->
  (unit -> 'a) ->
  'a option
