(** A really basic profiler.

    This module implements a very basic tool for recording the time consumed by
    a user-specified key. Its use corresponds to:

    {[
      let run profiler =
        Profiler.save profiler ~name:"my computation";
        my_computation ();
        Profiler.save profiler ~name:"my computation"

      let () =
        let profiler = Profiler.make () in
        run profiler;
        List.iter (fun (name, time) ->
          Format.printf "%S took %fs\n%!" name time)
          (Profiler.dump profiler)
    ]}

    We can only record sequential tasks - the use of lwt {b is not} recommended.
    {!module:Profiling} also provides a poor's man profiler but here we want to
    restrict profiling to broad categories that are compatible with [pysemgrep]
    and needed by {!module:Metrics_}.
*)

type t
(** The type of profilers. *)

val make : unit -> t
(** [make ()] creates a new profiler. *)

val save : t -> name:string -> unit
(** [save t ~name] starts or records the time associated to a given [name]. *)

val record : t -> name:string -> (unit -> unit) -> unit
(** [record t ~name fn] records the time spent by the given [fn] and save it
    into the profiler with the name [name]. *)

val dump : t -> (string * float) list
(** [dump profiler] returns all recorded metrics. *)
