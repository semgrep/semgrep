(* Capability aware wrapper over Parmap (and a few helpers) *)

val parmap :
  < Cap.fork > ->
  ncores:int ->
  chunksize:int ->
  ('a -> 'b) ->
  'a list ->
  'b list

val disable_core_pinning : unit -> unit

(* return the number of cpus *)
val get_cpu_count : unit -> int
