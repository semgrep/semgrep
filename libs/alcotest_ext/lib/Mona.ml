(*
   A monad-like structure that accommodates promises as implemented by
   different libraries (Lwt, Async) as well as the synchronous case where
   a "promise" is the unit type.
*)
type 'unit_promise t = {
  return : unit -> 'unit_promise;
  bind : 'unit_promise -> (unit -> 'unit_promise) -> 'unit_promise;
  catch :
    (unit -> 'unit_promise) ->
    (exn -> Printexc.raw_backtrace -> 'unit_promise) ->
    'unit_promise;
}

let protect m ~finally func =
  let safe_finally () =
    m.catch finally (fun exn trace ->
        failwith
          (Printf.sprintf
             "Internal error in test framework: exception raised by 'finally': \
              %s\n\
              %s\n"
             (Printexc.to_string exn)
             (Printexc.raw_backtrace_to_string trace)))
  in
  m.catch
    (fun () ->
      m.bind (func ()) (fun res ->
          m.bind (safe_finally ()) (fun () -> m.return res)))
    (fun exn trace ->
      m.bind (safe_finally ()) (fun () ->
          Printexc.raise_with_backtrace exn trace))

let sync : unit t =
  {
    return = (fun () -> ());
    bind = (fun () func -> func ());
    catch =
      (fun func handler ->
        try func () with
        | exn ->
            let trace = Printexc.get_raw_backtrace () in
            handler exn trace);
  }

(* TODO: move out of the core library to avoid a systematic dependency
   on Lwt. *)
let lwt : unit Lwt.t t =
  let catch func handler =
    Lwt.catch func (fun exn ->
        (* TODO: need to capture the stack trace earlier? How? *)
        let trace = Printexc.get_raw_backtrace () in
        handler exn trace)
  in
  { return = Lwt.return; bind = Lwt.bind; catch }

(*
let async : unit Async.Deferred.t t = {
  return = Async.return;
  bind = Async.(>>=);
  catch = (fun func handler ->
    Async.(
      try_with func >>= function
      | Ok a -> return a
      | Error exn -> handler exn
    )
  );
}
*)
