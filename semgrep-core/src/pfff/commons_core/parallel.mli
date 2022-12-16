(* returns a futur *)
val invoke :
  ('a -> 'b) -> 'a ->
  (unit -> 'b)

val parallel_map : ('a -> 'b) -> 'a list -> 'b list

type 'a job = unit -> 'a
type 'a jobs = ('a job) list

(* This will create (List.length xs) forks *)
val map_jobs: tasks:int -> 'a jobs -> 'a list
(* This will create (tasks) forks *)
val map_batch_jobs: tasks:int -> 'a jobs -> 'a list

(* this helps debug exceptions in the invoked function *)
val backtrace_when_exn: bool ref
