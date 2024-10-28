exception Parmap_unhandled_children of (string * int) list
(** List of children spawned by [Parmap] that were not awaited correctly by
    [Parmap]. The list is tuples of [status, code] where [code] is a standard
    Unix exit code, and [status] is one of:
    - [WEXITED] -> exited on its own
    - [WSIGNALED] -> Received a signal like sigkill or sigsegv
    - [WSTOPPED] -> process was paused for some reason, this is unlikely

    This exception is usually raised when a child (process running [f] passed to
    [parmap]) is killed from OOM, or segfaults, because Parmap does not handle
    it. Sadly due to the way that Parmap is written, this may only be raised if
    all children are unhandled, a lot of times Parmap in the parent process
    (caller of [parmap]) will simply segfault if only some of the children
    segfault/exit abnormally. This is a bug in Parmap, but because of OCaml 5 it
    is unlikely to be fixed except by us. *)

exception Parmap_marshalling_failure
(** Parmap failed to unmarshal data from a child. This could be due to an
    uncaught [Parmap_unhandled_children] exception, or more likely the type of
    the value returned by the [f] passed to [parmap] was not what was expected,
    and was shorter than the type expected (i.e. returned an [exn] when type
    [some_large_struct_or_buffer] was expected). This happens due to how the
    marshaling library works, as it just essentially reads a certain length of
    data from some buffer somewhere, and when it expects to be able to keep
    reading this data when unmarshaling, but reaches the end of the buffer, it
    raises [End_of_file] which we then convert to this exception. This should
    not happen as we try and catch all exceptions. Most likely this means an
    exception is being raised in the [exception_handler] passed to [parmap] *)

let () =
  Printexc.register_printer (fun e ->
      match e with
      | Parmap_unhandled_children status_and_codes ->
          let tuples =
            status_and_codes
            |> List_.map (fun (status_str, code) ->
                   Printf.sprintf "(%s,%d)" status_str code)
            |> String.concat ","
          in
          Some (Printf.sprintf "Parmap_unhandled_children([%s])" tuples)
      | _ -> None)

(* alt: use Logs src in Parmap code? *)
let debugging = Parmap.debugging

let default_exception_handler (_x : 'a) (e : Exception.t) =
  Exception.to_string e

let wrap_result f ~exception_handler x =
  try Ok (f x) with
  | exn ->
      let e = Exception.catch exn in
      (* From marshal.mli in the OCaml stdlib:
       *  "Values of extensible variant types, for example exceptions (of
       *  extensible type [exn]), returned by the unmarshaller should not be
       *  pattern-matched over through [match ... with] or [try ... with],
       *  because unmarshalling does not preserve the information required for
       *  matching their constructors. Structural equalities with other
       *  extensible variant values does not work either.  Most other uses such
       *  as Printexc.to_string, will still work as expected."
       *)
      (* Because of this we cannot just catch the exception here and return
         it, as then it won't be super usable. Instead we ask the user of the
         library to handle it in the process, since then they can pattern
         match on it. They can choose to convert it to a string, a different
         datatype etc. *)
      Error (exception_handler x e)

let parmap _caps ?init ?finalize ~ncores ~chunksize ~exception_handler f xs =
  (* Why do this? The nanny state doesn't trust you to to use parmap AND catch
     all your exceptions. And if you don't catch all your exceptions and one
     happens, then parmap will try to unmarshal your exception into the data
     type you wanted from [f], and it will fail horribly since the return type
     of your [f] is almost certainly not [exn]. So what we do here is catch all
     exceptions and return a result instead, meaning parmap will ALWAYS receive
     the correct marshaled data type *)
  let f' x = wrap_result f ~exception_handler x in
  let finally () =
    (* Parmap doesn't handle its child processes that well, and so if they exit
       abnormally (e.g. segfault) then we have to clean up its mess.

       See: Parmap_unhandled_children for more info *)
    (* wait () will wait for any children that exited up until this point, and
       return their reason for exiting and the associated code *)
    let wait () =
      (* status_and_codes is a list of (string * int) option list where the
         string is the kind of exit a child process had, and the corresponding
         code, or the item is None if the exit was normal. the same data is
         returned, as on every call we call waitpid and try reaping another
         child. See [Parmap_unhandled_children] for more info *)
      let rec wait_aux status_and_codes =
        try
          (* WNOHANG so we don't block, and -1 to reap all exited children *)
          let pid, status = Unix.waitpid [ Unix.WNOHANG ] (-1) in
          (* If there are unreaped children that have yet to exit, let's not
             wait since we only really care about the parmap children who
             signaled/exit/stopped. This means some parmap children may become
             zombies/keep running but usually this is a fatal error anyways so
             we should be fine. *)
          if pid = 0 then status_and_codes
          else
            let status_and_code_opt =
              match status with
              | WEXITED 0 ->
                  (* This is a really weird edgecase, in which there is /some/
                     process that exited normally, but was not reaped. The
                     expectation here is that this is a parmap child process,
                     and what most likely has happened is that End_of_file was
                     raised before parmap could reap its own children. See the
                     comment for Parmap_marshalling_failure for more info on
                     this. the other option is that sometime before this parmap
                     call, a child process was spawned and never reaped.*)
                  (* nosemgrep here as this is almost always a fatal error so
                     let's always print it *)
                  (* nosemgrep: no-logs-in-library *)
                  Logs.warn (fun m ->
                      m
                        "Child process %d exited normally but was not reaped \
                         before a parmap call finished"
                        pid);
                  None
              | WEXITED c -> Some ("WEXITED", c)
              | WSTOPPED c -> Some ("WSTOPPED", c)
              | WSIGNALED c -> Some ("WSIGNALED", c)
            in
            match status_and_code_opt with
            | Some (status_str, code) ->
                (* nosemgrep here as this is almost always a fatal error so
                   let's always print it *)
                (* nosemgrep: no-logs-in-library *)
                Logs.err (fun m ->
                    m "Parmap child %d was not reaped and exited with: %s: %d"
                      pid status_str code);
                wait_aux ((status_str, code) :: status_and_codes)
            | None -> wait_aux status_and_codes
        with
        | Unix.Unix_error (Unix.ECHILD, _, _) -> status_and_codes
      in
      wait_aux []
    in
    match wait () with
    | [] -> ()
    | status_and_codes -> raise (Parmap_unhandled_children status_and_codes)
  in
  try
    Common.protect ~finally (fun () ->
        Parmap.parmap ?init ?finalize ~ncores ~chunksize f' (Parmap.L xs))
  with
  | End_of_file as exn ->
      let e = Exception.catch exn in
      (* nosemgrep here as this is almost always a fatal error so
         let's always print it *)
      (* nosemgrep: no-logs-in-library *)
      Logs.err (fun m ->
          m "Parmap marshalling failure: %s" Exception.(to_string e));
      raise Parmap_marshalling_failure

(* this is just because we forget every call to Parmap.$F in
 * TCB/forbid_process.jsonnet so we need that
 *)
let disable_core_pinning = Parmap.disable_core_pinning

let get_cpu_count () : int =
  (* Parmap subtracts 1 from the number of detected cores.
     This comes with no guarantees. *)
  max 1 (Parmap.get_default_ncores () + 1)
