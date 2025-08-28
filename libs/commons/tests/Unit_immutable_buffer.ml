(*
   Copyright (c) 2022-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
module B = Immutable_buffer

let test_immutable_buffer () =
  let check (expected, b) =
    Alcotest.(check string) expected expected (B.to_string b)
  in
  List.iter check
    [
      ("just string", B.of_string "just string");
      ( "combine",
        B.combine [ B.of_string "co"; B.of_string "mbi"; B.of_string "ne" ] );
      ( "with a separator",
        B.combine ~sep:" "
          [ B.of_string "with"; B.of_string "a"; B.of_string "separator" ] );
      ( "foo(1, 2, 3)",
        B.combine
          [
            B.of_string "foo(";
            B.combine ~sep:", "
              [ B.of_string "1"; B.of_string "2"; B.of_string "3" ];
            B.of_string ")";
          ] );
    ]

let tests =
  Testo.categorize "immutable buffer"
    [ Testo.create "test immutable buffer" test_immutable_buffer ]
