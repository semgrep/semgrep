(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Tests for our Random_ module
*)

let t = Testo.create

let test_shuffle () =
  let check input =
    let shuffled = Random_.shuffle input in
    (* Check that shuffled list has same length *)
    Alcotest.(check int) __LOC__ (List.length input) (List.length shuffled);
    (* Check that shuffled list has same elements (ignoring order) *)
    let sorted_input = List.sort compare input in
    let sorted_shuffled = List.sort compare shuffled in
    Alcotest.(check (list int)) __LOC__ sorted_input sorted_shuffled
  in
  (* Test empty list *)
  check [];
  (* Test single element *)
  check [ 1 ];
  (* Test multiple elements *)
  check [ 1; 2; 3 ];
  (* Test with duplicates *)
  check [ 1; 1; 2; 2; 3 ]

let tests = Testo.categorize "Random_" [ t "shuffle" test_shuffle ]
