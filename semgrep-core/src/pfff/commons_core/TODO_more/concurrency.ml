(*s: concurrency.ml *)
open Common

let _biglock =
  Mutex.create ()

let atomic f =
  Mutex.lock _biglock;
  Common.finalize f (fun () -> Mutex.unlock _biglock)

(*e: concurrency.ml *)
