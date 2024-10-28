(* Capability and exception aware wrapper over Parmap (and a few helpers) *)

exception Parmap_unhandled_children of (string * int) list
(** List of children spawned by [Parmap] that were not awaited correctly by
    [Parmap]. This usually happens when a child is sigkilled, or segfaults *)

exception Parmap_marshalling_failure
(** Parmap failed to unmarshal data from a child. This usually only happens when
    an exception is raised in the [exception_handler] passed to [parmap] *)

val debugging : bool -> unit
(** [debugging true] will enable debugging in [Parmap]. This will print which
    tasks are sent to what workers, how long they took, and some other info.
    Useful for debugging issues unique to Parmap *)

val default_exception_handler : 'a -> Exception.t -> string
(** the default exception handler for [parmap], it will just convert the
    exception to a string *)

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

    Can raise [Parmap_unhandled_children] if [f] segfaults or OOMS, but only
    sometimes. See [Parmap_unhandled_children] for more info.

    Can raise [Parmap_marshalling_failure], usually if an exception is raised in
    [exception_handler]

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

(* internal helper useful outside if you want to reproduce with
 * List.map the semantic of Parmap_.parmap()
 *)
val wrap_result :
  ('a -> 'b) ->
  exception_handler:('a -> Exception.t -> 'err) ->
  'a ->
  ('b, 'err) result

val disable_core_pinning : unit -> unit

(* return the number of cpus *)
val get_cpu_count : unit -> int
