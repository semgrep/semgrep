(*
 * The unit argument is actually so that a call to
 * [invoke_in_child_process caps f args] can return a promise on the result.
 *)

val apply_in_child_process : < Cap.fork > -> ('a -> 'b) -> 'a -> unit -> 'b
