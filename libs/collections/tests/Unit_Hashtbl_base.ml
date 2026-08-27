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
   Unit tests for Hashtbl_.Base
*)

let t = Testo.create

let test_find () =
  let h = Hashtbl_.Base.hash_of_list [ (1, "a"); (2, "b") ] in
  Alcotest.(check string) __LOC__ "a" (Hashtbl_.Base.find h 1);
  Alcotest.check_raises __LOC__ Not_found (fun () ->
      ignore (Hashtbl_.Base.find h 3))

let test_hash_of_list_last_wins () =
  (* key 1 appears twice; the later binding ("c") must win, and must not
     leave a stray second entry behind. *)
  let h = Hashtbl_.Base.hash_of_list [ (1, "a"); (2, "b"); (1, "c") ] in
  Alcotest.(check string) __LOC__ "c" (Hashtbl_.Base.find h 1);
  Alcotest.(check string) __LOC__ "b" (Hashtbl_.Base.find h 2);
  Alcotest.(check (list int)) __LOC__ [ 1; 2 ] (Hashtbl_.Base.hkeys h)

let test_stack () =
  let tbl = Hashtbl_.Base.hash_of_list [] in
  Hashtbl_.Base.push tbl 42 "a";
  Hashtbl_.Base.push tbl 17 "b";
  Hashtbl_.Base.push tbl 42 "c";
  Alcotest.(check (list string))
    __LOC__ [ "c"; "a" ]
    (Hashtbl_.Base.get_stack tbl 42);
  Alcotest.(check (list string))
    __LOC__ [ "b" ]
    (Hashtbl_.Base.get_stack tbl 17);
  Alcotest.(check (list string)) __LOC__ [] (Hashtbl_.Base.get_stack tbl 110);
  Alcotest.(check (option string))
    __LOC__ (Some "c")
    (Hashtbl_.Base.peek_opt tbl 42);
  Alcotest.(check (option string))
    __LOC__ (Some "b")
    (Hashtbl_.Base.peek_opt tbl 17);
  Alcotest.(check (option string)) __LOC__ None (Hashtbl_.Base.peek_opt tbl 110)

let test_sorted_outputs () =
  (* Inserted out of order; outputs must come back sorted regardless of
     internal (hash-based) iteration order. *)
  let h =
    Hashtbl_.Base.hash_of_list
      [ (5, "e"); (3, "c"); (1, "a"); (6, "f"); (2, "b"); (4, "d") ]
  in
  Alcotest.(check (list (pair int string)))
    __LOC__
    [ (1, "a"); (2, "b"); (3, "c"); (4, "d"); (5, "e"); (6, "f") ]
    (Hashtbl_.Base.hash_to_list h);
  Alcotest.(check (list int))
    __LOC__ [ 1; 2; 3; 4; 5; 6 ] (Hashtbl_.Base.hkeys h);
  let hs = Hashtbl_.Base.hashset_of_list [ 5; 3; 1; 6; 2; 4 ] in
  Alcotest.(check (list int))
    __LOC__ [ 1; 2; 3; 4; 5; 6 ]
    (Hashtbl_.Base.hashset_to_list hs)

let tests =
  Testo.categorize "Hashtbl_.Base"
    [
      t "find raises Not_found" test_find;
      t "hash_of_list last-wins" test_hash_of_list_last_wins;
      t "push/peek_opt/get_stack" test_stack;
      t "sorted outputs" test_sorted_outputs;
    ]
