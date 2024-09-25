(* Capability aware wrapper over Parmap (and a few helpers) *)

val default_exception_handler : 'a -> Exception.t -> string

val parmap :
  < Cap.fork > ->
  ncores:int ->
  chunksize:int ->
  exception_handler:('a -> Exception.t -> 'c) ->
  ('a -> 'b) ->
  'a list ->
  ('b, 'c) result list
(** [parmap caps ~ncores ~chunksize ~exception_handler f xs] is like
    [Parmap.parmap], but will return a result, containing [Ok(f x)] if it
    succeeds, or if an exception is raised while [f x] is being computed and is
    not caught, the result will contain [Error (exception_handler x e)] where
    [e] is the caught exception. [exception_handler] is needed as OCaml cannot
    marshal real exceptions, so we must handle them in the parmap child
    process *)

val disable_core_pinning : unit -> unit

(* return the number of cpus *)
val get_cpu_count : unit -> int
