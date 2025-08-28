(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
let top a b =
  let add_one a = a + 1 [@@trace] in
  add_one a + b

let add_data_to_span _sp _data = ()

let top2 a b =
  let%trace sp = "example" in
  a + 1;
  add_data_to_span sp [ ("a", `Int 1) ]
