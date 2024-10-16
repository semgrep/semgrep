(* Capability aware wrapper over Parmap (and a few helpers) *)

val default_exception_handler : 'a -> Exception.t -> string

val parmap :
  < Cap.fork > ->
  ?init:(int -> unit) ->
  ?finalize:(unit -> unit) ->
  ncores:int ->
  chunksize:int ->
  exception_handler:('a -> Exception.t -> 'c) ->
  ('a -> 'b) ->
  'a list ->
  ('b, 'c) result list
(** [parmap caps ?init ?finalize ~ncores ~chunksize ~exception_handler f xs] is
    like [Parmap.parmap], but will return a result, containing [Ok(f x)] if it
    succeeds, or if an exception is raised while [f x] is being computed and is
    not caught, the result will contain [Error (exception_handler x e)] where
    [e] is the caught exception. [exception_handler] is needed as OCaml cannot
    marshal real exceptions, so we must handle them in the parmap child process.
    [?init] takes in the job number and will run before any calls to [f].
    Similarly [?finalize] will run after all calls to [f].

    Example:
    {[
      let f x = print_endline (string_of_int x); x+1 in
      let init n = Printf.printf "Starting job %d\n" n in
      let finalize () = Printf.printf "All jobs done\n" in
      let results = parmap ~init ~finalize~ncores ~chunksize ~exception_handler f [1;2;3;4] in
      List.iter (function
        | Ok x -> Printf.printf "Result: %d\n" x
        | Error e -> Printf.printf "Error: %s\n" e
      ) results
    ]}
    will produce:
    {[
      Starting job 0
      Starting job 1
      Starting job 2
      Starting job 3
      1
      2
      3
      4
      All jobs done
      Result: 2
      Result: 3
      Result: 4
      Result: 5
    ]}
 *)

val disable_core_pinning : unit -> unit

(* return the number of cpus *)
val get_cpu_count : unit -> int
