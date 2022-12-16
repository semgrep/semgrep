(*s: distribution.mli *)
val map_reduce:
  ?timeout:int ->
  fmap:('a -> 'b) -> freduce:('c -> 'b -> 'c) ->
  'c -> 'a list -> 'c * 'a list
(*x: distribution.mli *)
val map_reduce_lazy:
  ?timeout:int ->
  fmap:('a -> 'b) -> freduce:('c -> 'b -> 'c) ->
  'c -> (unit -> 'a list) -> 'c * 'a list
(*x: distribution.mli *)
val debug_mpi: bool ref
(*x: distribution.mli *)
(*****************************************************************************)
(* Private *)
(*****************************************************************************)
(*s: distribution.mli private *)
val under_mpirun : unit -> bool
(*x: distribution.mli private *)
val master :
  ?timeout:int ->
  freduce:('c -> 'b -> 'c) -> 'c -> 'a list -> 'c * 'a list
(*x: distribution.mli private *)
val worker :
  fmap:('a -> 'b) -> unit
(*x: distribution.mli private *)
exception TaskFinished
(*x: distribution.mli private *)
val mpi_adjust_argv : string array -> string array
(*e: distribution.mli private *)
(*e: distribution.mli *)
