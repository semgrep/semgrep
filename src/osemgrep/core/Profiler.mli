(** A really basic profiler.

    This module implements a very basic tool for recording the time consumed by
    a user-specified key. Its use corresponds to:

    {[
      let run profiler =
        Profiler.start profiler ~name:"my computation";
        my_computation ();
        Profiler.stop profiler ~name:"my computation"

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

val start : t -> name:string -> unit
(** [start profiler ~name] starts to record [name]. *)

val stop : t -> name:string -> unit
(** [stop profiler ~name] stops to profile [name] and record the time spent
    internally. If [name] does not exists or already recorder, this function
    raises an [Invalid_arg]. *)

val stop_ign : t -> name:string -> unit
(** Same as {!val:stop} but ignores errors. *)

val record : t -> name:string -> (unit -> 'a) -> 'a
(** [record t ~name fn] records the time spent by the given [fn] and save it
    into the profiler with the name [name]. *)

val dump : t -> (string * float) list
(** [dump profiler] returns all recorded metrics. *)
