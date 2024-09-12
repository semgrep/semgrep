val apply_in_child_process :
  < Cap.fork > -> ?flags:Marshal.extern_flags list -> ('a -> 'b) -> 'a -> 'b

(*
 * The unit argument is actually so that a call to
 * [apply_in_child_process_promise caps f args] can return a promise on the
 * result.
 *)
val apply_in_child_process_promise :
  < Cap.fork > ->
  ?flags:Marshal.extern_flags list ->
  ('a -> 'b) ->
  'a ->
  unit ->
  'b
