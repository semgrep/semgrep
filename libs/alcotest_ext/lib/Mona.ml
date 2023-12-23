(*
   A monad-like structure that accommodates promises as implemented by
   different libraries (Lwt, Async) as well as the synchronous case where
   a "promise" is the unit type.
*)
type 'unit_promise t = {
  return : unit -> 'unit_promise;
  bind : 'unit_promise -> (unit -> 'unit_promise) -> 'unit_promise;
  catch : (unit -> 'unit_promise) -> (exn -> 'unit_promise) -> 'unit_promise;
}

let protect m ~finally func =
  m.catch func (fun exn ->
      let bt = Printexc.get_raw_backtrace () in
      m.bind (finally ()) (fun () -> Printexc.raise_with_backtrace exn bt))

let sync : unit t =
  {
    return = (fun x -> x);
    bind = (fun x func -> func x);
    catch =
      (fun func handler ->
        try func () with
        | exn -> handler exn);
  }

(* TODO: move out of the core library to avoid a systematic dependency
   on Lwt. *)
let lwt : unit Lwt.t t =
  { return = Lwt.return; bind = Lwt.bind; catch = Lwt.catch }

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
