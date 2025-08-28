(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Unit tests for Hashtbl_
*)

let test_stack () =
  let tbl = Hashtbl.create 100 in
  Hashtbl_.push tbl 42 "a";
  Hashtbl_.push tbl 17 "b";
  Hashtbl_.push tbl 42 "c";
  assert (Hashtbl_.get_stack tbl 42 = [ "c"; "a" ]);
  assert (Hashtbl_.get_stack tbl 17 = [ "b" ]);
  assert (Hashtbl_.get_stack tbl 110 = [])

let tests =
  Testo.categorize "Hashtbl_" [ Testo.create "push/get_stack" test_stack ]
