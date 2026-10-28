(*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Unit tests for the GC alarm-based timeouts of Time_limit.
*)

let t = Testo.create

(* Allocate and finish major GC cycles until the timeout fires. *)
let rec spin_until_timeout () =
  Gc.full_major ();
  spin_until_timeout ()

let test_timeout () =
  match Time_limit.set_timeout ~name:"test" 0.01 spin_until_timeout with
  | None -> ()
  | Some _ -> assert false

(*
   The deadline passes while the guarded fiber is suspended, and the GC alarm
   then runs while a sibling fiber is running. The Timeout must not be raised
   in the sibling fiber (or in the eio scheduler), where it would escape the
   guard of [set_timeout]; it must be raised once the guarded fiber runs
   again.
*)
let test_timeout_while_suspended () =
  Eio_main.run @@ fun _env ->
  let resumed, resume = Eio.Promise.create () in
  Eio.Switch.run @@ fun sw ->
  let res =
    Time_limit.set_timeout ~name:"test" 0.01 (fun () ->
        Eio.Fiber.fork ~sw (fun () ->
            (* The guarded fiber is now suspended, waiting for us. *)
            Unix.sleepf 0.05;
            (* Run the GC alarm in this fiber, past the deadline. *)
            Gc.full_major ();
            Gc.full_major ();
            Eio.Promise.resolve resume ());
        Eio.Promise.await resumed;
        spin_until_timeout ())
  in
  match res with
  | None -> ()
  | Some _ -> assert false

let tests =
  Testo.categorize "time limits"
    [
      t "gc alarm timeout" test_timeout;
      t "gc alarm timeout while suspended" test_timeout_while_suspended;
    ]
