(*
   Exit the process after the specified timeout (in seconds).
*)
let exit_after ?stderr_msg ~duration exit_code =
  (*
     Warning: the signal handler executes asynchronously, much like a thread.
     Don't use printf or other functions that are not thread-safe.
  *)
  let write_msg =
    match stderr_msg with
    | None -> fun () -> ()
    | Some msg ->
        let msg = Bytes.of_string msg in
        fun () -> ignore (Unix.write Unix.stderr msg 0 (Bytes.length msg))
  in
  let handler _signal =
    write_msg ();
    exit exit_code
  in
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle handler);
  ignore (Unix.alarm duration)
