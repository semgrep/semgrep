open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* 
 * Some simple helpers around harrop's invoke() to do some
 * map-reduce like parallel programming using fork.
 * 
 * related work:  
 *  - https://gitorious.org/parmap, very similar, but also contains
 *    a master/workers model which helps do some form of load balancing
 * 
 *  - https://github.com/MyLifeLabs/nproc, but it uses lwt
 * 
 *  - distribution.ml which use MPI and so can leverage multiple
 *    machines (but MPI turned out to be quite unstable in my experience)
 * 
 *  - julien's MultiWorker but works only in native code
 *  
 * less: could be useful to autodetect a good number based on the
 * number of cores and available memory like we do in our libphutil 'Future'
 * library.
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
let backtrace_when_exn = ref true

(*****************************************************************************)
(* Building block *)
(*****************************************************************************)

(* src: harrop article on fork-based parallelism 
 * returns a futur
*)
let invoke2 f x =
  let input, output = Unix.pipe() in
  match Unix.fork() with
  (* error, could not create process, well compute now then *)
  | -1 -> 
    let v = f x in 
    (fun () -> v)
  (* child *)
  | 0 ->
      Unix.close input;
      let output = Unix.out_channel_of_descr output in
      Marshal.to_channel output 
          (try `Res(f x) 
           with e -> 
              if !backtrace_when_exn
              then begin
                let backtrace = Printexc.get_backtrace () in
                pr2 (spf "Exception in invoked func: %s" (Common.exn_to_s e));
                pr2 backtrace;
              end;
             `Exn e
          ) [];
      close_out output;
      exit 0
  (* parent *)
  | pid ->
      Unix.close output;
      let input = Unix.in_channel_of_descr input in
      fun () ->
        let v = 
          try 
            Marshal.from_channel input 
          with End_of_file -> 
          `Exn 
           (Failure "End_of_file in Parallel.invoke parent, probably segfault in child")
        in
        ignore (Unix.waitpid [] pid);
        close_in input;
        match v with
        | `Res x -> x
        | `Exn e -> raise e

let invoke a b = 
  Common.profile_code "Parallel.invoke" (fun () -> invoke2 a b)

let parallel_map f xs =
  (* create all the fork *)
  let futures = List.map (invoke f) xs in
  (* sync, get all parents to waitpid *)
  List.map (fun futur -> futur ()) futures

(*****************************************************************************)
(* Poor's man job scheduler *)
(*****************************************************************************)

type 'a job = unit -> 'a
type 'a jobs = ('a job) list

(* 
 * This is a very naive job scheduler. One limitation is that before
 * launching another round we must wait for the slowest process. A
 * set of workers and a master model would be more efficient by always
 * feeding processors. A partial fix is to give a tasks number that
 * is quite superior to the actual number of processors.
 * 
 * This will create (List.length xs) forks, but n at a time, in multiple
 * rounds, where n=tasks.
 * 
 * I use it for now to //ize the code coverage computation for PHP.
 *)
let map_jobs ~tasks xs =
  if tasks = 1 
  then List.map (fun job -> job ()) xs
  else 
    let xxs = Common2.pack_safe tasks xs in
    xxs |> List.map (fun xs ->
      (* do in parallel a batch of job *)
      parallel_map (fun job -> job ()) xs
    ) |> List.flatten


(* 
 * For some computation, it doesn't help to process every item in a 
 * separate process because the cost of fork is higher than the
 * computation cost. But it can still makes sense to group the files
 * into batches and process them in parallel.
 * 
 * This will create (tasks) forks.
 * 
 * I use it for now to //ize the abstract-interpreter-based callgraph
 * generation.
 * 
 * Thx to Michal burger for the initial idea.
 *)
let map_batch_jobs ~tasks xs =
  if tasks = 1
  then List.map (fun job -> job ()) xs
  else
    (* todo? a double pack ? because the initial pack/chunks can 
     * be computationaly "inbalanced".
     *)
    let xxs = Common2.chunks tasks xs in
    let jobs = xxs |> List.map (fun xs ->
      (fun () ->
        xs |> List.map (fun job -> job ())
      ))
    in
    parallel_map (fun job -> job ()) jobs |> List.flatten
