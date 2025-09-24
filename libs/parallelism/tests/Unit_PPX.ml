(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

let t = Testo.create

(* Baseline to sanity check against *)
let without_yield () = ()

(* Try with auto annotation *)
[@@@maybe_yield "auto"]

let with_yield1 () = ()
let with_yield2 () = ()
let with_yield3 () = ()

[@@@maybe_yield "auto-off"]

(* With annotation *)
let with_yield3 () = () [@@maybe_yield]

let test_func f expect_yield_count_changed =
  let yield_count = Concurrent.yield_attempts () in
  Eio_main.run @@ fun _ ->
  f ();
  let yield_count_after = Concurrent.yield_attempts () in
  let yield_count_changed = yield_count_after <> yield_count in
  Alcotest.(check bool) __LOC__ expect_yield_count_changed yield_count_changed

(* PPX_Test_Lib automatically annotates all modules with maybe_yield *)

let test_without_yield () = test_func without_yield false
let test_auto_lib_without_yield () = test_func PPX_Test_Lib.without_yield false
let test_with_yield1 () = test_func with_yield1 true
let test_with_yield2 () = test_func with_yield2 true
let test_with_yield3 () = test_func with_yield3 true
let test_auto_lib_with_yield () = test_func PPX_Test_Lib.with_yield true

let tests =
  Testo.categorize "PPX"
    [
      t "test_without_yield" test_without_yield;
      t "test_auto_lib_without_yield" test_auto_lib_without_yield;
      t "test_with_yield1" test_with_yield1;
      t "test_with_yield2" test_with_yield2;
      t "test_with_yield3" test_with_yield3;
      t "test_auto_lib_with_yield" test_auto_lib_with_yield;
    ]
