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

let string_of_signal signal_number =
  match Signal.get_info signal_number with
  | None -> spf "unknown signal %i" signal_number
  | Some (name, descr) -> spf "signal %s: %s" name descr

let check_status ((pid, process_status) : int * Unix.process_status) =
  match process_status with
  | WEXITED 0 -> ()
  | WEXITED exit_code ->
      let msg = spf "process %i exited with code %i" pid exit_code in
      failwith msg
  | WSIGNALED signal_number ->
      let msg =
        spf "process %i was killed by %s"
          pid (string_of_signal signal_number)
      in
      failwith msg
  | WSTOPPED signal_number ->
      let msg =
        spf "process %i was stopped by %s"
          pid (string_of_signal signal_number)
      in
      failwith msg

(*
   Avoid an 'Invalid_argument' exception on Windows.
*)
let safe_fork () =
  match Sys.os_type with
  | "Win32" -> -1 (* indicates error *)
  | _ -> Unix.fork ()

(* src: harrop article on fork-based parallelism
 * returns a futur
*)
let invoke2 f x =
  flush_all (); (* avoid duplicate output *)
  let input, output = Unix.pipe() in
  match safe_fork () with
  | n when n < 0 ->
    (*
       Error, could not create process.
       One possible reason is that we're on Windows (native).

       We fall back to computing the task in the same process.
    *)
      let v = f x in
      (fun () -> v)

  (* child *)
  | 0 ->
      (* bugfix: subtle: the parent may die (for example because of a Timeout),
       * which would generate a Sys_error "broken pipe" exn. This exn may
       * not be captured by the inner try (maybe because
       * the child didn't get a chance to run yet, or because it finished
       * correctly but the exn got generated while inside the call to
       * Marshal.to_channel). In that case, without the outer try, the exn
       * would bubble up and the exit() below would never be executed which
       * would cause the child to execute code after the caller of invoke.
      *)
      (try
         Unix.close input;
         let output = Unix.out_channel_of_descr output in

         Marshal.to_channel output
           (try Ok (f x)
            with exn ->
              let e = Exception.catch exn in
              (* Should we remove this printing since the trace is now
                 propagated along with the exception, even across processes
                 through Marshal to/from_string? *)
              if !backtrace_when_exn
              then begin
                pr2 (spf "Exception in invoked func: %s"
                       (Exception.to_string e));
              end;
              Error e
           ) [];
         close_out output;

         (* if it happens, it's probably a Sys_error "Broken pipe" *)
       with
       | Sys_error _ ->
           (* we always want to execute exit below, hence this catch all try
            * which is the equivalence of Common.finalize ... (fun () exit 0)
           *)
           ()
       | exn ->
           let e = Exception.catch exn in
           pr2 (spf "really unexpected exn in invoke child: %s"
                  (Exception.to_string e))
      );
      exit 0
  (* parent *)
  | pid ->
      Unix.close output;
      let input = Unix.in_channel_of_descr input in
      fun () ->
        let v =
          try
            Some (Marshal.from_channel input)
          with End_of_file ->
            None
        in
        let status = Unix.waitpid [] pid in
        close_in input;
        check_status status;
        match v with
        | Some (Ok x) -> x
        | Some (Error (e : Exception.t)) ->
            (* TODO: the exception we get here cannot be pattern-matched or
               tested for equality!
               The documentation in marshal.mli is pretty clear:

               Values of extensible variant types, for example exceptions (of
               extensible type [exn]), returned by the unmarshaller should not be
               pattern-matched over through [match ... with] or [try ... with],
               because unmarshalling does not preserve the information required for
               matching their constructors. Structural equalities with other
               extensible variant values does not work either.  Most other uses such
               as Printexc.to_string, will still work as expected.

              * which means you can't match or intercept the raised
              * exception. You can just print it or do ugly Obj.magic
              * level stuff with it.
            *)
            Exception.reraise e
        | None ->
            failwith "Bug in Parallel.invoke: child process appears to have \
                      exited successfully but produced invalid output."

let invoke f x =
  Common.profile_code "Parallel.invoke" (fun () -> invoke2 f x)

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
