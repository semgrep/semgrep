(*
   A monad-like structure that
   - is limited to wrapping the 'unit' type;
   - provides an exception catcher 'catch'.

   It is designed for wrapping a sequence of tests that provide their result
   by either returning (success) or by raising an exception (failure).
   This wrapping allows asynchronous computations i.e. allows running
   a sequence of tests interleaved with other computations. While such
   concurrency is not a property we need for running a test suite,
   it is necessary due to environments where we can't turn asynchronous
   computations into synchronous ones such as JavaScript.
   See https://github.com/mirage/alcotest/issues/119
*)
type 'unit_promise t = {
  return : unit -> 'unit_promise;
  bind : 'unit_promise -> (unit -> 'unit_promise) -> 'unit_promise;
  catch :
    (unit -> 'unit_promise) ->
    (exn -> Printexc.raw_backtrace -> 'unit_promise) ->
    'unit_promise;
}

(* Generalized version of Fun.protect *)
val protect :
  'unit_promise t ->
  finally:(unit -> 'unit_promise) ->
  (unit -> 'unit_promise) ->
  'unit_promise

(* The usual way to carry out computations and catch exceptions. *)
val sync : unit t
