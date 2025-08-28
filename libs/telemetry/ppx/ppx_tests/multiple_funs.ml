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
let foo a b = a + b [@@trace]
and bar a b = a + b [@@other]

let top a b =
  let add_one a = a + 1 [@@trace] in
  add_one a + b

let just_add a b = a + b
let label a b = a + b [@@trace "example function name"]
