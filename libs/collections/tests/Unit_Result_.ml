(* Nathan Taylor
 *
 * Copyright (C) Semgrep, Inc. All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see
 * <https://www.gnu.org/licenses/>.
 *)

let t = Testo.create

let test_collect () =
  let chk expected actual =
    Alcotest.(check (result (list int) string)) __LOC__ actual expected
  in

  Result_.collect [] |> chk (Ok []);
  Result_.collect [ Ok 42 ] |> chk (Ok [ 42 ]);
  Result_.collect [ Ok 1; Ok 2; Ok 3 ] |> chk (Ok [ 1; 2; 3 ]);
  Result_.collect [ Ok 1; Error "uh oh"; Ok 3 ] |> chk (Error "uh oh");
  Result_.collect [ Ok 1; Error "first"; Error "missed" ] |> chk (Error "first")

let test_list_map_ok () =
  let res =
    Result_.list_map
      (function
        | (0 | 1) as x -> Ok (string_of_int x)
        | _ -> Alcotest.fail "bad input list")
      [ 0; 1 ]
  in
  assert (res = Ok [ "0"; "1" ])

let test_list_map_error () =
  let res =
    Result_.list_map
      (function
        | 0
        | 1 ->
            Ok ()
        | 2 -> Error "error on 2"
        | _ -> Alcotest.fail "Result_.list_map failed to be lazy")
      [ 0; 1; 2; 3; 4 ]
  in
  assert (res = Error "error on 2")

let tests =
  Testo.categorize "Result_"
    [
      t "collect" test_collect;
      t "list_map ok" test_list_map_ok;
      t "list_map error" test_list_map_error;
    ]
