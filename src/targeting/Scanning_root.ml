(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   A dedicated type for scanning roots so as to clarify the code.
*)

type t = Fpath.t [@@deriving show]

let of_fpath x = x
let to_fpath x = x
let of_string = Fpath.v
let to_string = Fpath.to_string
